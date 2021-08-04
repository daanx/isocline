{- ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
---------------------------------------------------------------------------- -}
{-|
Description : Binding to the Isocline library, a portable alternative to GNU Readline
Copyright   : (c) 2021, Daan Leijen
License     : MIT
Maintainer  : daan@effp.org
Stability   : Experimental

![logo](https://raw.githubusercontent.com/daanx/isocline/main/doc/isocline-inline.svg) 
A Haskell wrapper around the [Isocline C library](https://github.com/daanx/isocline#readme) 
which can provide an alternative to GNU Readline.
(The Isocline library is included whole and not a separate dependency).

Isocline works across Unix, Windows, and macOS, and relies on a minimal subset of ANSI escape sequences.
It has a good multi-line editing mode (use shift/ctrl-enter) which is nice for inputting small functions etc.
Other features include support for colors, history, completion, unicode, undo/redo, 
incremental history search, inline hints, syntax highlighting, etc.

Minimal example with history:

@
import System.Console.Isocline

main :: IO ()
main  = do putStrLn \"Welcome\"
           `setHistory` \"history.txt\" 200
           input \<- `readline` \"myprompt\"     -- full prompt becomes \"myprompt> \"
           putStrLn (\"You wrote:\\n\" ++ input)
@

Or using custom completions with an interactive loop:

@
import System.Console.Isocline
import Data.Char( toLower )

main :: IO ()
main 
  = do `setStyleColor' 'StylePrompt' 'AnsiMaroon'
       `setHistory` "history.txt" 200
       `enableAutoTab` `True`
       interaction

interaction :: IO ()
interaction 
  = do s <- `readlineEx` \"hαskell\" (Just completer) Nothing 
       putStrLn (\"You wrote:\\n\" ++ s)
       if (s == \"\" || s == \"exit\") then return () else interaction
                     
completer :: `CompletionEnv` -> String -> IO () 
completer cenv input
  = do `completeFileName` cenv input Nothing [\".\",\"\/usr\/local\"] [\".hs\"]  -- use [] for any extension
       `completeWord` cenv input wcompleter

wcompleter :: String -> [`Completion`]
wcompleter input
  = `completionsFor` (map toLower input) 
      [\"print\",\"println\",\"prints\",\"printsln\",\"prompt\"]      
@

See a larger [example](https://github.com/daanx/isocline/blob/main/test/Example.hs) 
with syntax highlighting and more extenstive custom completion 
in the [Github repository](https://github.com/daanx/isocline).

Enjoy,
-- Daan
-}
module System.Console.Isocline( 
      -- * Readline
      readline, 
      readlineEx,      
      
      -- * History
      setHistory,
      historyClear,
      historyRemoveLast,
      historyAdd,

      -- * Completion
      CompletionEnv,      
      completeFileName,
      completeWord,
      completeQuotedWord,

      Completion(..),
      completion,
      completionWithDisplay,
      isPrefix,
      completionsFor,
      wordCompleter,

      -- * Syntax Highlighting 
      TextAttr(..),
      makeAttrHighlighter,
      attrDefault,
      withAttr,
      withAttrColor,
      withAttrBgColor,
      withAttrUnderline,
      withAttrReverse,
      withAttrDefault,

      -- * Terminal
      termWrite,
      termWriteLn,
      termColor,
      termBgColor,
      termUnderline,
      termReverse,
      termReset,
      
      -- * Configuration
      setPromptMarker,
      enableAutoTab,
      enableColor,
      enableBeep,
      enableMultiline,
      enableHistoryDuplicates,
      enableCompletionPreview,
      enableMultilineIndent,
      enableHint,
      enableHighlight,
      enableInlineHelp,
      
      Color(..), 
      Style(..),
      setStyleColor,
      getStyleColor,
            
      -- * Advanced
      setDefaultCompleter,      
      addCompletion,
      addCompletions,
      completeWordPrim,
      completeQuotedWordPrim,

      readlineMaybe,
      readlineExMaybe,
      readlinePrim,
      readlinePrimMaybe,

      getPromptMarker,
      getContinuationPromptMarker,
      stopCompleting,
      hasCompletions,

      -- * Low-level highlighting
      HighlightEnv,      
      setDefaultHighlighter,      
      setDefaultAttrHighlighter,
      -- highlightEsc,
      highlightColor,
      highlightBgColor,
      highlightUnderline,
      highlightReverse,
      highlightSetAttr,
      highlightSetAttrDiff

    ) where


import Data.List( intersperse, isPrefixOf )
import Control.Monad( when, foldM )
import Control.Exception( bracket )
import Foreign.C.String( CString, peekCString, peekCStringLen, withCString, castCharToCChar )
import Foreign.Ptr
import Foreign.C.Types

-- the following are used for utf8 encoding.
import qualified Data.ByteString as B ( useAsCString, packCString )
import qualified Data.Text as T  ( pack, unpack )
import Data.Text.Encoding as TE  ( decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error  ( lenientDecode )


----------------------------------------------------------------------------
-- C Types
----------------------------------------------------------------------------

data IcCompletionEnv  

-- | Abstract list of current completions.
newtype CompletionEnv = CompletionEnv (Ptr IcCompletionEnv)

type CCompleterFun = Ptr IcCompletionEnv -> CString -> IO ()
type CompleterFun  = CompletionEnv -> String -> IO ()


data IcHighlightEnv

-- | Abstract highlight environment
newtype HighlightEnv = HighlightEnv (Ptr IcHighlightEnv)    

type CHighlightFun = Ptr IcHighlightEnv -> CString -> Ptr () -> IO ()
type HighlightFun  = HighlightEnv -> String -> IO ()



----------------------------------------------------------------------------
-- Basic readline
----------------------------------------------------------------------------

foreign import ccall ic_free        :: (Ptr a) -> IO () 
foreign import ccall ic_malloc      :: CSize -> IO (Ptr a)
foreign import ccall ic_strdup      :: CString -> IO CString
foreign import ccall ic_readline    :: CString -> IO CString
foreign import ccall ic_readline_ex :: CString -> FunPtr CCompleterFun -> (Ptr ()) -> FunPtr CHighlightFun -> (Ptr ()) -> IO CString

unmaybe :: IO (Maybe String) -> IO String
unmaybe action
  = do mb <- action
       case mb of
         Nothing -> return ""
         Just s  -> return s

-- | @readline prompt@: Read (multi-line) input from the user with rich editing abilities. 
-- Takes the prompt text as an argument. The full prompt is the combination
-- of the given prompt and the promp marker (@\"> \"@ by default) .
-- See also 'readlineEx', 'readlineMaybe', 'enableMultiline', 'setStyleColor', and 'setPromptMarker'.
readline :: String -> IO String  
readline prompt
  = unmaybe $ readlineMaybe prompt

-- | As 'readline' but returns 'Nothing' on end-of-file or other errors (ctrl-C/ctrl-D).
readlineMaybe:: String -> IO (Maybe String)
readlineMaybe prompt
  = withUTF8String prompt $ \cprompt ->
    do cres <- ic_readline cprompt
       res  <- peekUTF8StringMaybe cres
       ic_free cres
       return res

-- | @readlineEx prompt mbCompleter mbHighlighter@: as 'readline' but
-- uses the given @mbCompleter@ function to complete words on @tab@ (instead of the default completer). 
-- and the given @mbHighlighter@ function to highlight the input (instead of the default highlighter).
-- See also 'readline' and 'readlineExMaybe'.
readlineEx :: String -> Maybe (CompletionEnv -> String -> IO ()) -> Maybe (String -> [TextAttr]) -> IO String
readlineEx prompt completer highlighter
  = unmaybe $ readlineExMaybe prompt completer highlighter

-- | As 'readlineEx' but returns 'Nothing' on end-of-file or other errors (ctrl-C/ctrl-D).
-- See also 'readlineMaybe'.
readlineExMaybe :: String -> Maybe (CompletionEnv -> String -> IO ()) -> Maybe (String -> [TextAttr]) -> IO (Maybe String) 
readlineExMaybe prompt completer mbhighlighter
  = readlinePrimMaybe prompt completer (case mbhighlighter of
                                          Nothing -> Nothing
                                          Just hl -> Just (makeAttrHighlighter hl))

-- | @readlinePrim prompt mbCompleter mbHighlighter@: as 'readline' but
-- uses the given @mbCompleter@ function to complete words on @tab@ (instead of the default completer). 
-- and the given @mbHighlighter@ function to highlight the input (instead of the default highlighter).
-- See also 'readlineEx' and 'readlinePrimMaybe'.
readlinePrim :: String -> Maybe (CompletionEnv -> String -> IO ()) -> Maybe (HighlightEnv -> String -> IO ()) -> IO String
readlinePrim prompt completer highlighter
  = unmaybe $ readlinePrimMaybe prompt completer highlighter

-- | As 'readlinePrim' but returns 'Nothing' on end-of-file or other errors (ctrl-C/ctrl-D).
-- See also 'readlineMaybe'.
readlinePrimMaybe :: String -> Maybe (CompletionEnv -> String -> IO ()) -> Maybe (HighlightEnv -> String -> IO ()) -> IO (Maybe String) 
readlinePrimMaybe prompt completer highlighter
  = withUTF8String prompt $ \cprompt ->
    do ccompleter   <- makeCCompleter completer
       chighlighter <- makeCHighlighter highlighter
       cres <- ic_readline_ex cprompt ccompleter nullPtr chighlighter nullPtr
       res  <- peekUTF8StringMaybe cres
       ic_free cres
       when (ccompleter /= nullFunPtr)   $ freeHaskellFunPtr ccompleter
       when (chighlighter /= nullFunPtr) $ freeHaskellFunPtr chighlighter
       return res

----------------------------------------------------------------------------
-- History
----------------------------------------------------------------------------

foreign import ccall ic_set_history           :: CString -> CInt -> IO ()
foreign import ccall ic_history_remove_last   :: IO ()
foreign import ccall ic_history_clear         :: IO ()
foreign import ccall ic_history_add           :: CString -> IO ()

-- | @setHistory filename maxEntries@: 
-- Enable history that is persisted to the given file path with a given maximum number of entries.
-- Use -1 for the default entries (200).
-- See also 'enableHistoryDuplicates'.
setHistory :: FilePath -> Int -> IO ()
setHistory fname maxEntries
  = withUTF8String0 fname $ \cfname ->
    do ic_set_history cfname (toEnum maxEntries)

-- | Isocline automatically adds input of more than 1 character to the history.
-- This command removes the last entry.
historyRemoveLast :: IO ()
historyRemoveLast 
  = ic_history_remove_last

-- | Clear the history.
historyClear :: IO ()
historyClear
  = ic_history_clear

-- | @historyAdd entry@: add @entry@ to the history.
historyAdd :: String -> IO ()
historyAdd entry
  = withUTF8String0 entry $ \centry ->
    do ic_history_add centry 


----------------------------------------------------------------------------
-- Completion
----------------------------------------------------------------------------
-- use our own CBool for compatibility with an older base
type CCBool = CInt

type CCharClassFun = CString -> CLong -> IO CCBool
type CharClassFun  = Char -> Bool

foreign import ccall ic_set_default_completer :: FunPtr CCompleterFun -> IO ()
foreign import ccall "wrapper" ic_make_completer :: CCompleterFun -> IO (FunPtr CCompleterFun)
foreign import ccall "wrapper" ic_make_charclassfun :: CCharClassFun -> IO (FunPtr CCharClassFun)

foreign import ccall ic_add_completion        :: Ptr IcCompletionEnv -> CString -> CString -> IO CChar
foreign import ccall ic_complete_filename     :: Ptr IcCompletionEnv -> CString -> CChar -> CString -> CString -> IO ()
foreign import ccall ic_complete_word         :: Ptr IcCompletionEnv -> CString -> FunPtr CCompleterFun -> IO ()
foreign import ccall ic_complete_quoted_word  :: Ptr IcCompletionEnv -> CString -> FunPtr CCompleterFun -> FunPtr CCharClassFun -> CChar -> CString -> IO ()

foreign import ccall ic_has_completions       :: Ptr IcCompletionEnv -> IO CCBool
foreign import ccall ic_stop_completing       :: Ptr IcCompletionEnv -> IO CCBool

-- | A completion entry
data Completion = Completion { 
  display :: String,      -- ^ display of the completion in the completion menu
  replacement :: String,  -- ^ actual replacement
  help :: String          -- ^ help message (currently not supported)
}

-- | Create a completion with just a replacement
completion :: String -> Completion
completion replacement
  = Completion "" replacement ""

-- | Create a completion with a separate display string
completionWithDisplay :: String -> String -> Completion
completionWithDisplay display replacement
  = Completion display replacement ""  

-- | Is the given input a prefix of the completion replacement?
isPrefix :: String -> Completion -> Bool
isPrefix input compl
  = isPrefixOf input (replacement compl)

-- | @completionsFor input replacements@: Filter those @replacements@ that 
-- start with the given @input@, and return them as completions.
completionsFor :: String -> [String] -> [Completion]
completionsFor input rs
  = map completion (filter (isPrefixOf input) rs)

-- | Convenience: creates a completer function directly from a list
-- of candidate completion strings. Uses `completionsFor` to filter the 
-- input and `completeWord` to handle quotes.
-- For example: @'readlineEx' \"myprompt\" (Just ('wordCompleter' completer)) Nothing@.
wordCompleter :: [String] -> (CompletionEnv -> String -> IO ()) 
wordCompleter completions
  = (\cenv input -> completeWord cenv input (\input -> completionsFor input completions))

-- | @setDefaultCompleter completer@: Set a new tab-completion function @completer@ 
-- that is called by Isocline automatically. 
-- The callback is called with a 'CompletionEnv' context and the current user
-- input up to the cursor.
-- By default the 'completeFileName' completer is used.
-- This overwrites any previously set completer.
setDefaultCompleter :: (CompletionEnv -> String -> IO ()) -> IO ()
setDefaultCompleter completer 
  = do ccompleter <- makeCCompleter (Just completer)
       ic_set_default_completer ccompleter

makeCCompleter :: Maybe CompleterFun -> IO (FunPtr CCompleterFun)
makeCCompleter Nothing = return nullFunPtr
makeCCompleter (Just completer)
  = ic_make_completer wrapper
  where
    wrapper :: Ptr IcCompletionEnv -> CString -> IO ()
    wrapper rpcomp cprefx
      = do prefx <- peekUTF8String0 cprefx
           completer (CompletionEnv rpcomp) prefx


-- | @addCompletion compl completion@: Inside a completer callback, add a new completion.
-- If 'addCompletion' returns 'True' keep adding completions,
-- but if it returns 'False' an effort should be made to return from the completer
-- callback without adding more completions.
addCompletion :: CompletionEnv -> Completion -> IO Bool
addCompletion (CompletionEnv rpc) (Completion display replacement _)
  = withUTF8String0 display $ \cdisplay ->
    withUTF8String replacement $ \crepl ->
    do cbool <- ic_add_completion rpc cdisplay crepl
       return (fromEnum cbool /= 0)
    
-- | @addCompletions compl completions@: add multiple completions at once.
-- If 'addCompletions' returns 'True' keep adding completions,
-- but if it returns 'False' an effort should be made to return from the completer
-- callback without adding more completions.
addCompletions :: CompletionEnv -> [Completion] -> IO Bool
addCompletions compl [] = return True
addCompletions compl (c:cs)
  = do continue <- addCompletion compl c
       if (continue) 
         then addCompletions compl cs
         else return False

-- | @completeFileName compls input dirSep roots extensions@: 
-- Complete filenames with the given @input@, a possible directory separator @dirSep@, 
-- a list of root folders @roots@  to search from
-- (by default @["."]@), and a list of extensions to match (use @[]@ to match any extension).
-- The directory separator is used when completing directory names.
-- For example, using g @\'/\'@ as a directory separator, we get:
--
-- > /ho         --> /home/
-- > /home/.ba   --> /home/.bashrc
--
completeFileName :: CompletionEnv -> String -> Maybe Char -> [FilePath] -> [String] -> IO ()
completeFileName (CompletionEnv rpc) prefx dirSep roots extensions
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 (concat (intersperse ";" roots)) $ \croots ->
    withUTF8String0 (concat (intersperse ";" extensions)) $ \cextensions ->
    do let cdirSep = case dirSep of
                       Nothing -> toEnum 0
                       Just c  -> castCharToCChar c
       ic_complete_filename rpc cprefx cdirSep croots cextensions

-- | @completeWord compl input completer@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- For example, with a @hello world@ completion, we get:
--
-- > hel        -->  hello\ world
-- > hello\ w   -->  hello\ world
-- > hello w    -->                   # no completion, the word is just 'w'>
-- > "hel       -->  "hello world" 
-- > "hello w   -->  "hello world"
--
-- The call @('completeWord' compl prefx fun)@ is a short hand for 
-- @('completeQuotedWord' compl prefx fun (not . separator) \'\\\\\' \"\'\\\"\")@.
-- where @separator = \c -> c `elem` \" \\t\\r\\n,.;:/\\\\(){}[]\"@.
completeWord :: CompletionEnv -> String -> (String -> [Completion]) -> IO () 
completeWord cenv input completer
  = completeWordPrim cenv input cenvCompleter
  where
    cenvCompleter cenv input
      = do addCompletions cenv (completer input)
           return ()
  
-- | @completeQuotedWord compl input completer isWordChar escapeChar quoteChars@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- Unlike 'completeWord', this function takes an explicit function to determine /word/ characters,
-- the /escape/ character, and a string of /quote/ characters.
-- See also 'completeWord'.
completeQuotedWord :: CompletionEnv -> String -> (String -> [Completion]) -> (Char -> Bool) -> Maybe Char -> String -> IO () 
completeQuotedWord cenv input completer isWordChar escapeChar quoteChars
  = completeQuotedWordPrim cenv input cenvCompleter isWordChar escapeChar quoteChars
  where
    cenvCompleter cenv input 
      = do addCompletions cenv (completer input)
           return ()


-- | @completeWordPrim compl input completer@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- For example, with a @hello world@ completion, we get:
--
-- > hel        -->  hello\ world
-- > hello\ w   -->  hello\ world
-- > hello w    -->                   # no completion, the word is just 'w'>
-- > "hel       -->  "hello world" 
-- > "hello w   -->  "hello world"
--
-- The call @('completeWordPrim' compl prefx fun)@ is a short hand for 
-- @('completeQuotedWordPrim' compl prefx fun (not . separator) \'\\\\\' \"\'\\\"\")@.
-- where @separator = \c -> c `elem` \" \\t\\r\\n,.;:/\\\\(){}[]\"@.
completeWordPrim :: CompletionEnv -> String -> (CompletionEnv -> String -> IO ()) -> IO () 
completeWordPrim (CompletionEnv rpc) prefx completer
  = withUTF8String prefx $ \cprefx ->
    do ccompleter <- makeCCompleter (Just completer)
       ic_complete_word rpc cprefx ccompleter
       freeHaskellFunPtr ccompleter
  
-- | @completeQuotedWordPrim compl input completer isWordChar escapeChar quoteChars@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- Unlike 'completeWord', this function takes an explicit function to determine /word/ characters,
-- the /escape/ character, and a string of /quote/ characters.
-- See also 'completeWord'.
completeQuotedWordPrim :: CompletionEnv -> String -> (CompletionEnv -> String -> IO ()) -> (Char -> Bool) -> Maybe Char -> String -> IO () 
completeQuotedWordPrim (CompletionEnv rpc) prefx completer isWordChar escapeChar quoteChars
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 quoteChars $ \cquoteChars ->
    do let cescapeChar = case escapeChar of
                          Nothing -> toEnum 0
                          Just c  -> castCharToCChar c                      
       ccompleter <- makeCCompleter (Just completer)
       cisWordChar <- makeCharClassFun isWordChar
       ic_complete_quoted_word rpc cprefx ccompleter cisWordChar cescapeChar cquoteChars
       freeHaskellFunPtr cisWordChar
       freeHaskellFunPtr ccompleter
  
makeCharClassFun :: (Char -> Bool) -> IO (FunPtr CCharClassFun)
makeCharClassFun isInClass
  = let charClassFun :: CString -> CLong -> IO CCBool
        charClassFun cstr clen 
          = let len = (fromIntegral clen :: Int)
            in if (len <= 0) then return (cbool False)
                else do s <- peekCStringLen (cstr,len)
                        return (if null s then (cbool False) else cbool (isInClass (head s)))
    in ic_make_charclassfun charClassFun

-- | If this returns 'True' an effort should be made to stop completing and return from the callback.
stopCompleting :: CompletionEnv -> IO Bool
stopCompleting (CompletionEnv rpc)
  = uncbool $ ic_stop_completing rpc

-- | Have any completions be generated so far?
hasCompletions :: CompletionEnv -> IO Bool
hasCompletions (CompletionEnv rpc)
  = uncbool $ ic_has_completions rpc



----------------------------------------------------------------------------
-- Syntax highlighting
----------------------------------------------------------------------------

foreign import ccall ic_set_default_highlighter     :: FunPtr CHighlightFun -> Ptr () -> IO ()
foreign import ccall "wrapper" ic_make_highlight_fun:: CHighlightFun -> IO (FunPtr CHighlightFun)

foreign import ccall ic_highlight_color     :: Ptr IcHighlightEnv -> CLong -> CInt -> IO ()
foreign import ccall ic_highlight_bgcolor   :: Ptr IcHighlightEnv -> CLong -> CInt -> IO ()
foreign import ccall ic_highlight_underline :: Ptr IcHighlightEnv -> CLong -> CInt -> IO ()
foreign import ccall ic_highlight_reverse   :: Ptr IcHighlightEnv -> CLong -> CInt -> IO ()

{-
type CHighlightEscFun = CString -> Ptr () -> IO CString
type HighlightEscFun  = String -> String

foreign import ccall ic_highlight_esc       :: Ptr IcHighlightEnv -> CString -> FunPtr CHighlightEscFun -> Ptr () -> IO ()
foreign import ccall "wrapper" ic_make_highlight_esc_fun:: CHighlightEscFun -> IO (FunPtr CHighlightEscFun)
-}

-- | Set a syntax highlighter.
-- There can only be one highlight function, setting it again disables the previous one.
setDefaultHighlighter :: (HighlightEnv -> String -> IO ()) -> IO ()
setDefaultHighlighter highlighter
  = do chighlighter <- makeCHighlighter (Just highlighter)
       ic_set_default_highlighter chighlighter nullPtr
  where
    chighlightFun henv cinput carg
      = do input <- peekUTF8String0 cinput
           highlighter (HighlightEnv henv) input


makeCHighlighter :: Maybe (HighlightEnv -> String -> IO ()) -> IO (FunPtr CHighlightFun)
makeCHighlighter Nothing = return nullFunPtr
makeCHighlighter (Just highlighter)
  = ic_make_highlight_fun wrapper
  where
    wrapper henv cinput carg
      = do input <- peekUTF8String0 cinput
           highlighter (HighlightEnv henv) input

{-
-- | Use an escape sequence highlighter from inside a highlighter callback.
highlightEsc :: HighlightEnv -> String -> (String -> String) -> IO ()
highlightEsc (HighlightEnv henv) input highlight
  = withUTF8String0 input $ \cinput ->
    do cfun <- ic_make_highlight_esc_fun wrapper
       ic_highlight_esc henv cinput cfun nullPtr
  where
    wrapper cinput carg
      = do input <- peekUTF8String0 cinput
           withUTF8String0 (highlight input) $ \coutput ->
             ic_strdup coutput
-}

-- | @highlightColor henv pos color@: Set the color of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this color until another
-- color is set again.
highlightColor :: HighlightEnv -> Int -> Color -> IO ()
highlightColor (HighlightEnv henv) pos color 
  = do ic_highlight_color henv (clong (-pos)) (ccolor color)

-- | @highlightBgColor henv pos bgcolor@: Set the background color of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this background color until another
-- background color is set again.
highlightBgColor :: HighlightEnv -> Int -> Color -> IO ()
highlightBgColor (HighlightEnv henv) pos color 
  = do ic_highlight_bgcolor henv (clong (-pos)) (ccolor color)

-- | @highlightUnderline henv pos bgcolor@: Set underline of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this underlining until another
-- underlining is set again.
highlightUnderline :: HighlightEnv -> Int -> Bool -> IO ()
highlightUnderline (HighlightEnv henv) pos enable
  = do ic_highlight_underline henv (clong (-pos)) (cbool enable) 

-- | @highlightReverse henv pos bgcolor@: Set reverse video mode of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this reverse mode until another
-- reverse mode is set again.
highlightReverse :: HighlightEnv -> Int -> Bool -> IO ()
highlightReverse (HighlightEnv henv) pos enable
  = do ic_highlight_reverse henv (clong (-pos)) (cbool enable) 


-- | Text attributes for a single character.
data TextAttr = TextAttr{ 
  attrColor     :: Color, -- ^ color
  attrBgColor   :: Color, -- ^ background color
  attrUnderline :: Bool,  -- ^ underline
  attrReverse   :: Bool   -- ^ reverse video
}

-- | Default text attribute.
attrDefault :: TextAttr
attrDefault = TextAttr AnsiDefault AnsiDefault False False 

-- | Use the given text attribute for each character in the string. 
withAttr :: TextAttr -> String -> [TextAttr]
withAttr attr s = map (const attr) s

-- | Use the given color for each character in the string
withAttrColor :: Color -> String -> [TextAttr]
withAttrColor color s = withAttr (attrDefault{ attrColor = color }) s

-- | Use the given background color for each character in the string
withAttrBgColor :: Color -> String -> [TextAttr]
withAttrBgColor color s = withAttr (attrDefault{ attrBgColor = color }) s

-- | Use underline for each character in the string
withAttrUnderline :: String -> [TextAttr]
withAttrUnderline  s = withAttr (attrDefault{ attrUnderline = True}) s

-- | Use reverse video for each character in the string
withAttrReverse :: String -> [TextAttr]
withAttrReverse  s = withAttr (attrDefault{ attrReverse = True}) s

-- | Use the default text attributes for each character in the string.
withAttrDefault :: String -> [TextAttr]
withAttrDefault s = withAttr attrDefault s


-- | @highlightSetAttrDiff henv pos current attr@: Set new text attribute @attr@ 
-- at position @pos@; but only set any properties
-- that differ from the @current@ text attributes.
highlightSetAttrDiff :: HighlightEnv -> Int -> TextAttr -> TextAttr -> IO ()
highlightSetAttrDiff henv pos current attr
  = do when (attrColor current /= attrColor attr)         $ highlightColor henv pos (attrColor attr)
       when (attrBgColor current /= attrBgColor attr)     $ highlightBgColor henv pos (attrBgColor attr)
       when (attrUnderline current /= attrUnderline attr) $ highlightUnderline henv pos (attrUnderline attr)
       when (attrReverse current /= attrReverse attr)     $ highlightReverse henv pos (attrReverse attr)
       return ()

-- | @highlightSetAttr henv pos attr@: Set new text attribute @attr@ 
-- at position @pos@. 
highlightSetAttr :: HighlightEnv -> Int -> TextAttr -> IO ()
highlightSetAttr henv pos (TextAttr color bgcolor underline reverse) 
  = do -- todo: add one function to set all at once at the C side
       highlightColor henv pos color
       highlightBgColor henv pos bgcolor
       highlightUnderline henv pos underline
       highlightReverse henv pos reverse

-- | Set a syntax highlighter that uses a pure function that returns a list
-- of text attributes for each character in the input.
-- There can only be one highlight function, setting it again disables the previous one.
setDefaultAttrHighlighter :: (String -> [TextAttr]) -> IO ()
setDefaultAttrHighlighter highlight 
  = setDefaultHighlighter (makeAttrHighlighter highlight)


-- | Create a highlighter from a pure function that returns a list
-- of text attributes for each character in the input.
makeAttrHighlighter :: (String -> [TextAttr]) -> (HighlightEnv -> String -> IO ()) 
makeAttrHighlighter highlight
  = highlightWrapper
  where 
    highlightWrapper :: HighlightEnv -> String -> IO ()
    highlightWrapper henv input
      = do let attrs = highlight input               
           foldM (setAttr henv) attrDefault (zip [0..] attrs)
           return ()
    
    setAttr :: HighlightEnv -> TextAttr -> (Int,TextAttr) -> IO TextAttr
    setAttr henv current (pos,attr) 
      = do highlightSetAttrDiff henv pos current attr
           return attr


----------------------------------------------------------------------------
-- Terminal
----------------------------------------------------------------------------

foreign import ccall ic_write           :: CString -> IO ()
foreign import ccall ic_writeln         :: CString -> IO ()
foreign import ccall ic_term_color      :: CInt -> IO ()
foreign import ccall ic_term_bgcolor    :: CInt -> IO ()
foreign import ccall ic_term_underline  :: CCBool -> IO ()
foreign import ccall ic_term_reverse    :: CCBool -> IO ()
foreign import ccall ic_term_reset      :: IO ()

-- | Write output to the terminal where ANSI CSI sequences are
-- handled portably across platforms (including Windows).
termWrite :: String -> IO ()
termWrite s
  = withUTF8String0 s $ \cs -> ic_write cs

-- | Write output with a ending newline to the terminal where 
-- ANSI CSI sequences are handled portably across platforms (including Windows).
termWriteLn :: String -> IO ()
termWriteLn s
  = withUTF8String0 s $ \cs -> ic_writeln cs  

-- | Set the terminal text color. The color is auto adjusted for terminals with less colors.
termColor :: Color -> IO ()
termColor color
  = ic_term_color (ccolor color)

-- | Set the terminal text background color. The color is auto adjusted for terminals with less colors.
termBgColor :: Color -> IO ()
termBgColor color
  = ic_term_bgcolor (ccolor color)  

-- | Set the terminal text underline mode.
termUnderline :: Bool -> IO ()
termUnderline enable
  = ic_term_underline (cbool enable)  

-- | Set the terminal text reverse video mode.
termReverse :: Bool -> IO ()
termReverse enable
  = ic_term_reverse (cbool enable)  

-- | Reset the terminal text mode to defaults
termReset :: IO ()
termReset 
  = ic_term_reset


----------------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------------
foreign import ccall ic_set_style_color   :: CInt -> CInt -> IO ()
foreign import ccall ic_get_style_color   :: CInt -> IO CInt
foreign import ccall ic_set_prompt_marker :: CString -> CString -> IO ()
foreign import ccall ic_get_prompt_marker :: IO CString
foreign import ccall ic_get_continuation_prompt_marker :: IO CString
foreign import ccall ic_enable_multiline  :: CCBool -> IO CCBool
foreign import ccall ic_enable_beep       :: CCBool -> IO CCBool
foreign import ccall ic_enable_color      :: CCBool -> IO CCBool
foreign import ccall ic_enable_auto_tab   :: CCBool -> IO CCBool
foreign import ccall ic_enable_inline_help:: CCBool -> IO CCBool
foreign import ccall ic_enable_hint       :: CCBool -> IO CCBool
foreign import ccall ic_enable_highlight  :: CCBool -> IO CCBool
foreign import ccall ic_enable_history_duplicates :: CCBool -> IO CCBool
foreign import ccall ic_enable_completion_preview :: CCBool -> IO CCBool
foreign import ccall ic_enable_multiline_indent   :: CCBool -> IO CCBool



cbool :: Bool -> CCBool
cbool True  = toEnum 1
cbool False = toEnum 0

uncbool :: IO CCBool -> IO Bool
uncbool action
  = do i <- action
       return (i /= toEnum 0)


ccolor :: Color -> CInt
ccolor clr = toEnum (fromEnum clr)

unccolor :: IO CInt -> IO Color
unccolor action
  = do i <- action
       return (toEnum (fromEnum i))


clong :: Int -> CLong
clong l = toEnum l


-- | @setPromptMarker marker multiline_marker@: Set the prompt @marker@ (by default @\"> \"@). 
-- and a possible different continuation prompt marker @multiline_marker@ for multiline 
-- input (defaults to @marker@).
setPromptMarker :: String -> String -> IO ()
setPromptMarker marker multiline_marker  
  = withUTF8String0 marker $ \cmarker ->
    withUTF8String0 multiline_marker $ \cmultiline_marker ->
    do ic_set_prompt_marker cmarker cmultiline_marker


-- | Get the current prompt marker.
getPromptMarker :: IO String
getPromptMarker 
  = do cstr  <- ic_get_prompt_marker
       if (nullPtr == cstr) 
         then return ""
         else do cstr2 <- ic_strdup cstr
                 peekUTF8String0 cstr2

-- | Get the current prompt continuation marker for multi-line input.
getContinuationPromptMarker :: IO String
getContinuationPromptMarker 
  = do cstr <- ic_get_continuation_prompt_marker
       if (nullPtr == cstr) 
         then return ""
         else do cstr2 <- ic_strdup cstr
                 peekUTF8String0 cstr2


-- | Disable or enable multi-line input (enabled by default).
-- Returns the previous value.
enableMultiline :: Bool -> IO Bool
enableMultiline enable
  = do uncbool $ ic_enable_multiline (cbool enable)

-- | Disable or enable sound (enabled by default).
-- | A beep is used when tab cannot find any completion for example.
-- Returns the previous value.
enableBeep :: Bool -> IO Bool
enableBeep enable
  = do uncbool $ ic_enable_beep (cbool enable)

-- | Disable or enable color output (enabled by default).
-- Returns the previous value.
enableColor :: Bool -> IO Bool
enableColor enable
  = do uncbool $ ic_enable_color (cbool enable)

-- | Disable or enable duplicate entries in the history (duplicate entries are not allowed by default).
-- Returns the previous value.
enableHistoryDuplicates :: Bool -> IO Bool
enableHistoryDuplicates enable
  = do uncbool $ ic_enable_history_duplicates (cbool enable)


-- | Disable or enable automatic tab completion after a completion 
-- to expand as far as possible if the completions are unique. (disabled by default).
-- Returns the previous value.
enableAutoTab :: Bool -> IO Bool
enableAutoTab enable
  = do uncbool $ ic_enable_auto_tab (cbool enable)


-- | Disable or enable short inline help message (for history search etc.) (enabled by default).
-- Pressing F1 always shows full help regardless of this setting. 
-- Returns the previous value.
enableInlineHelp :: Bool -> IO Bool
enableInlineHelp enable
  = do uncbool $ ic_enable_inline_help (cbool enable)

-- | Disable or enable preview of a completion selection (enabled by default)
-- Returns the previous value.
enableCompletionPreview :: Bool -> IO Bool
enableCompletionPreview enable
  = do uncbool $ ic_enable_completion_preview (cbool enable)

-- | Styles for user interface elements
data Style
  = StylePrompt   -- ^ style for the prompt (text and marker) (`AnsiGreen` by default)
  | StyleInfo     -- ^ info: for example, numbers in the completion menu (`AnsiDarkGray` by default).
  | StyleDiminish -- ^ diminish: for example, non matching parts in a history search (`AnsiLightGray` by default)
  | StyleEmphasis -- ^ emphasis: for example, the matching part in a history search (@'RGB' 0xFFFFD7@ by default).
  | StyleHint     -- ^ hint: for inline hints (`AnsiDarkGray` by default).
  deriving (Show,Eq,Enum)

cstyle :: Style -> CInt
cstyle style = toEnum (fromEnum style)

-- | Set the color used for interface elements (like the prompt).
-- Use `ColorNone` to use the default color. (but `AnsiDefault` for the default terminal text color!
-- See also 'Style'.
setStyleColor :: Style -> Color -> IO ()
setStyleColor style color
  = ic_set_style_color (cstyle style) (ccolor color)

-- | Get the color used for interface elements.
-- See also 'Style'.
getStyleColor :: Style -> IO Color
getStyleColor style 
  = unccolor $ ic_get_style_color (cstyle style) 

-- | Disable or enable automatic indentation to line up the
-- multiline prompt marker with the initial prompt marker (enabled by default).
-- Returns the previous value.
-- See also 'setPromptMarker'.
enableMultilineIndent :: Bool -> IO Bool
enableMultilineIndent enable
  = do uncbool $ ic_enable_multiline_indent (cbool enable)

-- | Disable or enable automatic inline hinting (enabled by default)
-- Returns the previous value.
enableHint :: Bool -> IO Bool
enableHint enable
  = do uncbool $ ic_enable_hint (cbool enable)

-- | Disable or enable syntax highlighting (enabled by default).
-- Returns the previous value.
enableHighlight :: Bool -> IO Bool
enableHighlight enable
  = do uncbool $ ic_enable_highlight (cbool enable)


----------------------------------------------------------------------------
-- Colors
----------------------------------------------------------------------------

-- | Terminal colors. Used for example in 'setStyleColor'.
-- On terminals with less colors, RGB values are auto translated to the closest ANSI colors (256 or 16 color).
data Color  = AnsiBlack
            | AnsiMaroon
            | AnsiGreen
            | AnsiOrange
            | AnsiNavy
            | AnsiPurple
            | AnsiTeal
            | AnsiLightGray
            | AnsiDarkGray
            | AnsiRed
            | AnsiLime
            | AnsiYellow
            | AnsiBlue
            | AnsiMagenta
            | AnsiCyan
            | AnsiWhite
            | AnsiDefault            -- ^ Default terminal text color
            | ColorNone              -- ^ No color (often used for a default color)
            | RGB Int                -- ^ RGB hex value
            deriving (Show,Eq,Ord)

-- | Create a color from an RGB hex value @0x@RRGGBB
rgb :: Int -> Color 
rgb hex = RGB (hex `mod` 0x1000000)

-- | Create a color from separate red, green, and blue values in the range @0@ - @0xFF@.
rgbx :: Int -> Int -> Int -> Color
rgbx r g b = rgb ((65535*(r`mod`0xFF)) + (256*(g`mod`0xFF)) + (b`mod`0xFF))

instance Enum Color where
  fromEnum color 
    = case color of
        ColorNone       -> 0
        RGB rgb         -> (0x1000000 + rgb)
        AnsiBlack       -> 30
        AnsiMaroon      -> 31
        AnsiGreen       -> 32
        AnsiOrange      -> 33
        AnsiNavy        -> 34
        AnsiPurple      -> 35
        AnsiTeal        -> 36
        AnsiLightGray   -> 37
        AnsiDarkGray    -> 90
        AnsiRed         -> 91
        AnsiLime        -> 92
        AnsiYellow      -> 93
        AnsiBlue        -> 94
        AnsiMagenta     -> 95
        AnsiCyan        -> 96
        AnsiWhite       -> 97
        AnsiDefault     -> 39

  toEnum color 
    = case color of
        30 -> AnsiBlack
        31 -> AnsiMaroon
        32 -> AnsiGreen
        33 -> AnsiOrange
        34 -> AnsiNavy
        35 -> AnsiPurple
        36 -> AnsiTeal
        37 -> AnsiLightGray
        90 -> AnsiDarkGray
        91 -> AnsiRed
        92 -> AnsiLime
        93 -> AnsiYellow
        94 -> AnsiBlue
        95 -> AnsiMagenta
        96 -> AnsiCyan
        97 -> AnsiWhite
        39 -> AnsiDefault
        _  -> if (color >= 0x1000000 && color <= 0x1FFFFFF) 
                then RGB (color - 0x1000000)
                else ColorNone


----------------------------------------------------------------------------
-- UTF8 Strings
----------------------------------------------------------------------------

withUTF8String0 :: String -> (CString -> IO a) -> IO a
withUTF8String0 s action
  = if (null s) then action nullPtr else withUTF8String s action

peekUTF8String0 :: CString -> IO String
peekUTF8String0 cstr
  = if (nullPtr == cstr) then return "" else peekUTF8String cstr

peekUTF8StringMaybe :: CString -> IO (Maybe String)
peekUTF8StringMaybe cstr
  = if (nullPtr == cstr) then return Nothing 
     else do s <- peekUTF8String cstr
             return (Just s)

peekUTF8String :: CString -> IO String
peekUTF8String cstr
  = do bstr <- B.packCString cstr
       return (T.unpack (TE.decodeUtf8With lenientDecode bstr))

withUTF8String :: String -> (CString -> IO a) -> IO a
withUTF8String str action
  = do let bstr = TE.encodeUtf8 (T.pack str)
       B.useAsCString bstr action
       