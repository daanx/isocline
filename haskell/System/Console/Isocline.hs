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
(The Isocline library is included whole and there are no runtime dependencies).

Isocline works across Unix, Windows, and macOS, and relies on a minimal subset of ANSI escape sequences.
It has a good multi-line editing mode (use shift/ctrl-enter) which is nice for inputting small functions etc.
Other features include support for colors, history, completion, unicode, undo/redo, 
incremental history search, inline hints, brace matching, syntax highlighting, rich text using bbcode
formatting, etc.

Minimal example with history:

@
import System.Console.Isocline

main :: IO ()
main  = do putStrLn \"Welcome\"
           `setHistory` \"history.txt\" 200
           input \<- `readline` \"myprompt\"     -- full prompt becomes \"myprompt> \"
           `putFmtLn` (\"[gray]You wrote:[\/gray]\\n\" ++ input)
@

Or using custom completions with an interactive loop:

@
import System.Console.Isocline
import Data.Char( toLower )

main :: IO ()
main 
  = do `styleDef' "ic-prompt" "ansi-maroon"
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
       `completeWord` cenv input Nothing wcompleter

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
      completeQuotedWordEx,

      Completion(..),
      completion,
      isPrefix,
      completionsFor,
      wordCompleter,

      -- * Syntax Highlighting 
      highlightFmt,

      -- * Rich text
      Style, Fmt,      
      style,
      plain,
      pre,
      
      putFmt,
      putFmtLn,

      styleDef,
      styleOpen,
      styleClose,
      withStyle,

      -- * Configuration
      setPromptMarker,
      enableAutoTab,
      enableColor,
      enableBeep,
      enableMultiline,
      enableHistoryDuplicates,
      enableCompletionPreview,
      enableMultilineIndent,
      enableHighlight,
      enableInlineHelp,
      enableHint,
      setHintDelay,
      enableBraceMatching,
      enableBraceInsertion,
      setMatchingBraces,
      setInsertionBraces,    
            
      -- * Advanced
      setDefaultCompleter,      
      addCompletion,
      addCompletionPrim,
      addCompletions,
      completeWordPrim,
      completeQuotedWordPrim,
      completeQuotedWordPrimEx,

      readlineMaybe,
      readlineExMaybe,
      readlinePrim,
      readlinePrimMaybe,

      getPromptMarker,
      getContinuationPromptMarker,
      stopCompleting,
      hasCompletions,

      asyncStop,

      -- * Low-level highlighting
      HighlightEnv,      
      setDefaultHighlighter,      
      setDefaultFmtHighlighter,
      
      -- * Low-level Terminal
      termInit, 
      termDone,
      withTerm,
      termFlush,
      termWrite,
      termWriteLn,
      termColor,
      termBgColor,
      termColorAnsi,
      termBgColorAnsi,
      termUnderline,
      termReverse,
      termReset      

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
foreign import ccall ic_async_stop  :: IO CCBool

unmaybe :: IO (Maybe String) -> IO String
unmaybe action
  = do mb <- action
       case mb of
         Nothing -> return ""
         Just s  -> return s

-- | @readline prompt@: Read (multi-line) input from the user with rich editing abilities. 
-- Takes the prompt text as an argument. The full prompt is the combination
-- of the given prompt and the prompt marker (@\"> \"@ by default) .
-- See also 'readlineEx', 'readlineMaybe', 'enableMultiline', and 'setPromptMarker'.
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
readlineEx :: String -> Maybe (CompletionEnv -> String -> IO ()) -> Maybe (String -> Fmt) -> IO String
readlineEx prompt completer highlighter
  = unmaybe $ readlineExMaybe prompt completer highlighter

-- | As 'readlineEx' but returns 'Nothing' on end-of-file or other errors (ctrl-C/ctrl-D).
-- See also 'readlineMaybe'.
readlineExMaybe :: String -> Maybe (CompletionEnv -> String -> IO ()) -> Maybe (String -> Fmt) -> IO (Maybe String) 
readlineExMaybe prompt completer mbhighlighter
  = readlinePrimMaybe prompt completer (case mbhighlighter of
                                          Nothing -> Nothing
                                          Just hl -> Just (highlightFmt hl))

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

-- | Thread safe call to asynchronously send a stop event to a 'readline' 
-- which behaves as if the user pressed @ctrl-C@,
-- which will return with 'Nothing' (or @\"\"@). 
-- Returns 'True' if the event was successfully delivered.
asyncStop :: IO Bool
asyncStop
  = uncbool $ ic_async_stop

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

foreign import ccall ic_add_completion_ex     :: Ptr IcCompletionEnv -> CString -> CString -> CString -> IO CCBool
foreign import ccall ic_add_completion_prim   :: Ptr IcCompletionEnv -> CString -> CString -> CString -> CInt -> CInt -> IO CCBool
foreign import ccall ic_complete_filename     :: Ptr IcCompletionEnv -> CString -> CChar -> CString -> CString -> IO ()
foreign import ccall ic_complete_word         :: Ptr IcCompletionEnv -> CString -> FunPtr CCompleterFun -> FunPtr CCharClassFun -> IO ()
foreign import ccall ic_complete_qword        :: Ptr IcCompletionEnv -> CString -> FunPtr CCompleterFun -> FunPtr CCharClassFun -> IO ()
foreign import ccall ic_complete_qword_ex     :: Ptr IcCompletionEnv -> CString -> FunPtr CCompleterFun -> FunPtr CCharClassFun -> CChar -> CString -> IO ()

foreign import ccall ic_has_completions       :: Ptr IcCompletionEnv -> IO CCBool
foreign import ccall ic_stop_completing       :: Ptr IcCompletionEnv -> IO CCBool

-- | A completion entry
data Completion = Completion { 
  replacement :: String,  -- ^ actual replacement
  display :: String,      -- ^ display of the completion in the completion menu
  help :: String          -- ^ help message 
} deriving (Eq, Show)

-- | Create a completion with just a replacement
completion :: String -> Completion
completion replacement
  = Completion replacement "" ""

-- | @completionFull replacement display help@: Create a completion with a separate display and help string.
completionFull :: String -> String -> String -> Completion
completionFull replacement display help
  = Completion  replacement display help 


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
-- input and `completeWord` to find the word boundary.
-- For example: @'readlineEx' \"myprompt\" (Just ('wordCompleter' completer)) Nothing@.
wordCompleter :: [String] -> (CompletionEnv -> String -> IO ()) 
wordCompleter completions
  = (\cenv input -> completeWord cenv input Nothing (\input -> completionsFor input completions))

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

withCCompleter :: Maybe CompleterFun -> (FunPtr CCompleterFun -> IO a) -> IO a
withCCompleter completer action
  = bracket (makeCCompleter completer) (\cfun -> when (nullFunPtr /= cfun) (freeHaskellFunPtr cfun)) action

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
addCompletion (CompletionEnv rpc) (Completion replacement display help)
  = withUTF8String replacement $ \crepl ->
    withUTF8String0 display $ \cdisplay ->
    withUTF8String0 help $ \chelp ->    
    do cbool <- ic_add_completion_ex rpc crepl cdisplay chelp
       return (fromEnum cbool /= 0)

-- | @addCompletionPrim compl completion deleteBefore deleteAfter@: 
-- Primitive add completion, use with care and call only directly inside a completer callback.
-- If 'addCompletion' returns 'True' keep adding completions,
-- but if it returns 'False' an effort should be made to return from the completer
-- callback without adding more completions.
addCompletionPrim :: CompletionEnv -> Completion -> Int -> Int -> IO Bool
addCompletionPrim (CompletionEnv rpc) (Completion replacement display help) deleteBefore deleteAfter
  = withUTF8String replacement $ \crepl ->
    withUTF8String0 display $ \cdisplay ->
    withUTF8String0 help $ \chelp ->
    do cbool <- ic_add_completion_prim rpc crepl cdisplay chelp (toEnum deleteBefore) (toEnum deleteAfter)
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

-- | @completeWord compl input isWordChar completer@: 
-- Complete a /word/ (or /token/) and calls the user @completer@ function with just the current word
-- (instead of the whole input)
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, an possible
-- @isWordChar@ function, and a user defined 
-- @completer@ function that is called with adjusted input which 
-- is limited to the /word/ just before the cursor.
-- Pass 'Nothing' to @isWordChar@ for the default @not . separator@
-- where @separator = \c -> c `elem` \" \\t\\r\\n,.;:/\\\\(){}[]\"@.
completeWord :: CompletionEnv -> String -> Maybe (Char -> Bool) -> (String -> [Completion]) -> IO () 
completeWord cenv input isWordChar completer 
  = completeWordPrim cenv input isWordChar cenvCompleter
  where
    cenvCompleter cenv input
      = do addCompletions cenv (completer input)
           return ()

-- | @completeQuotedWord compl input isWordChar completer@: 
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
-- The call @('completeWord' compl prefx isWordChar fun)@ is a short hand for 
-- @('completeQuotedWord' compl prefx isWordChar \'\\\\\' \"\'\\\"\" fun)@.
-- Pass 'Nothing' to @isWordChar@ for the default @not . separator@
-- where @separator = \c -> c `elem` \" \\t\\r\\n,.;:/\\\\(){}[]\"@.
completeQuotedWord :: CompletionEnv -> String -> Maybe (Char -> Bool) -> (String -> [Completion]) -> IO () 
completeQuotedWord cenv input isWordChar completer 
  = completeWordPrim cenv input isWordChar cenvCompleter
  where
    cenvCompleter cenv input
      = do addCompletions cenv (completer input)
           return ()
  
-- | @completeQuotedWordEx compl input isWordChar escapeChar quoteChars completer@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- Unlike 'completeQuotedWord', this function can specify 
-- the /escape/ character and the /quote/ characters.
-- See also 'completeWord'.
completeQuotedWordEx :: CompletionEnv -> String -> Maybe (Char -> Bool) -> Maybe Char -> String -> (String -> [Completion]) -> IO () 
completeQuotedWordEx cenv input isWordChar escapeChar quoteChars completer 
  = completeQuotedWordPrimEx cenv input isWordChar escapeChar quoteChars cenvCompleter 
  where
    cenvCompleter cenv input 
      = do addCompletions cenv (completer input)
           return ()


-- | @completeWord compl input isWordChar completer@: 
-- Complete a /word/,/token/ and calls the user @completer@ function with just the current word
-- (instead of the whole input)
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, an possible
-- @isWordChar@ function, and a user defined 
-- @completer@ function that is called with adjusted input which 
-- is limited to the /word/ just before the cursor.
-- Pass 'Nothing' to @isWordChar@ for the default @not . separator@
-- where @separator = \c -> c `elem` \" \\t\\r\\n,.;:/\\\\(){}[]\"@.
completeWordPrim :: CompletionEnv -> String -> Maybe (Char -> Bool) -> (CompletionEnv -> String -> IO ()) -> IO () 
completeWordPrim (CompletionEnv rpc) prefx isWordChar completer 
  = withUTF8String prefx $ \cprefx ->
    withCharClassFun isWordChar $ \cisWordChar ->
    withCCompleter (Just completer) $ \ccompleter ->
    do ic_complete_word rpc cprefx ccompleter cisWordChar


-- | @completeWordPrim compl input isWordChar completer@: 
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
-- The call @('completeWordPrim' compl prefx isWordChar fun)@ is a short hand for 
-- @('completeQuotedWordPrim' compl prefx isWordChar \'\\\\\' \"\'\\\"\" fun)@.
-- Pass 'Nothing' to @isWordChar@ for the default @not . separator@
-- where @separator = \c -> c `elem` \" \\t\\r\\n,.;:/\\\\(){}[]\"@.
completeQuotedWordPrim :: CompletionEnv -> String -> Maybe (Char -> Bool) -> (CompletionEnv -> String -> IO ()) -> IO () 
completeQuotedWordPrim (CompletionEnv rpc) prefx isWordChar completer
  = withUTF8String prefx $ \cprefx ->
    withCharClassFun isWordChar $ \cisWordChar ->
    withCCompleter (Just completer) $ \ccompleter ->
    do ic_complete_qword rpc cprefx ccompleter cisWordChar
  

-- | @completeQuotedWordPrim compl input isWordChar escapeChar quoteChars completer@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'CompletionEnv' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- Unlike 'completeWord', this function takes an explicit function to determine /word/ characters,
-- the /escape/ character, and a string of /quote/ characters.
-- See also 'completeWord'.
completeQuotedWordPrimEx :: CompletionEnv -> String -> Maybe (Char -> Bool) -> Maybe Char -> String -> (CompletionEnv -> String -> IO ()) ->  IO () 
completeQuotedWordPrimEx (CompletionEnv rpc) prefx isWordChar escapeChar quoteChars completer
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 quoteChars $ \cquoteChars ->
    withCharClassFun isWordChar $ \cisWordChar ->
    withCCompleter (Just completer) $ \ccompleter ->
    do let cescapeChar = case escapeChar of
                          Nothing -> toEnum 0
                          Just c  -> castCharToCChar c                      
       ic_complete_qword_ex rpc cprefx ccompleter cisWordChar cescapeChar cquoteChars
       

withCharClassFun :: Maybe (Char -> Bool) -> (FunPtr CCharClassFun -> IO a) -> IO a
withCharClassFun isInClass action
  = bracket (makeCharClassFun isInClass) (\cfun -> when (nullFunPtr /= cfun) (freeHaskellFunPtr cfun))  action 

makeCharClassFun :: Maybe (Char -> Bool) -> IO (FunPtr CCharClassFun)
makeCharClassFun Nothing = return nullFunPtr
makeCharClassFun (Just isInClass)
  = let charClassFun :: CString -> CLong -> IO CCBool
        charClassFun cstr clen 
          = let len = (fromIntegral clen :: Int)
            in if (len <= 0) then return (cbool False)
                else do s <- peekCStringLen (cstr,len)
                        return (if null s then (cbool False) else cbool (isInClass (head s)))
    in do ic_make_charclassfun charClassFun
          

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
foreign import ccall ic_highlight                   :: Ptr IcHighlightEnv -> CLong -> CLong -> CString -> IO ()
foreign import ccall ic_highlight_formatted         :: Ptr IcHighlightEnv -> CString -> CString -> IO ()


-- | Set a syntax highlighter.
-- There can only be one highlight function, setting it again disables the previous one.
setDefaultHighlighter :: (HighlightEnv -> String -> IO ()) -> IO ()
setDefaultHighlighter highlighter
  = do chighlighter <- makeCHighlighter (Just highlighter)
       ic_set_default_highlighter chighlighter nullPtr

makeCHighlighter :: Maybe (HighlightEnv -> String -> IO ()) -> IO (FunPtr CHighlightFun)
makeCHighlighter Nothing = return nullFunPtr 
makeCHighlighter (Just highlighter)
  = ic_make_highlight_fun wrapper
  where 
    wrapper :: Ptr IcHighlightEnv -> CString -> Ptr () -> IO ()
    wrapper henv cinput carg
      = do input <- peekUTF8String0 cinput
           highlighter (HighlightEnv henv) input


-- | @highlight henv pos len style@: Set the style of @len@ characters
-- starting at position @pos@ in the input 
highlight :: HighlightEnv -> Int -> Int -> String -> IO ()
highlight (HighlightEnv henv) pos len style
  = withUTF8String0 style $ \cstyle ->
    do ic_highlight henv (clong (-pos)) (clong (-len)) cstyle


-- | A style for formatted strings ('Fmt').
-- For example, a style can be @"red"@ or @"b #7B3050"@. 
-- See the full list of valid [properties](https://github.com/daanx/isocline#bbcode-format)
type Style = String

-- | A string with [bbcode](https://github.com/daanx/isocline#bbcode-format) formatting.
-- For example @"[red]this is red[\/]"@.n
type Fmt   = String

-- | Use an rich text formatted highlighter from inside a highlighter callback.
highlightFmt :: (String -> Fmt) -> (HighlightEnv -> String -> IO ())
highlightFmt highlight (HighlightEnv henv) input 
  = withUTF8String0 input $ \cinput ->
    withUTF8String0 (highlight input) $ \cfmt ->
    do ic_highlight_formatted henv cinput cfmt


-- | Style a string, e.g. @style "b red" "bold and red"@ (which is equivalent to @"[b red]bold and red[\/]"@).
-- See the repo for a full description of all [styles](https://github.com/daanx/isocline#bbcode-format).
style :: Style -> Fmt -> Fmt
style st s
  = if null st then s else ("[" ++ st ++ "]" ++ s ++ "[/]") 

-- | Escape a string so no tags are interpreted as formatting.
plain :: String -> Fmt
plain s
  = if (any (\c -> (c == '[' || c == ']')) s) then "[!pre]" ++ s ++ "[/pre]" else s

-- | Style a string that is printed as is without interpreting markup inside it (using `plain`).
pre :: Style -> String -> Fmt
pre st s
  = style st (plain s)

-- | Set a syntax highlighter that uses a pure function that returns a bbcode
-- formatted string (using 'style', 'plain' etc). See 'highlightFmt' for more information.
-- There can only be one highlight function, setting it again disables the previous one.
setDefaultFmtHighlighter :: (String -> Fmt) -> IO ()
setDefaultFmtHighlighter highlight 
  = setDefaultHighlighter (highlightFmt highlight)





----------------------------------------------------------------------------
-- Print rich text
----------------------------------------------------------------------------

foreign import ccall ic_print           :: CString -> IO ()
foreign import ccall ic_println         :: CString -> IO ()
foreign import ccall ic_style_def       :: CString -> CString -> IO ()
foreign import ccall ic_style_open      :: CString -> IO ()
foreign import ccall ic_style_close     :: IO ()

-- | Output rich formatted text containing [bbcode](https://github.com/daanx/isocline#bbcode-format).
-- For example: @putFmt \"[b]bold [red]and red[\/][\/]\"@
-- All unclosed tags are automatically closed (but see also 'styleOpen').
-- See the repo for more information about [formatted output](https://github.com/daanx/isocline#formatted-output).
putFmt :: Fmt -> IO ()
putFmt s 
  = withUTF8String0 s $ \cs -> 
    do ic_print cs

-- | Output rich formatted text containing bbcode's ending with a newline.
putFmtLn :: Fmt -> IO ()
putFmtLn s 
  = withUTF8String0 s $ \cs -> 
    do ic_println cs

-- | Define (or redefine) a style.
-- For example @styleDef "warning" "crimon underline"@,
-- and then use it as @'putFmtLn' "[warning]this is a warning[/]"@. 
-- This can be very useful for theming your application with semantic styles.
-- See also [formatted output](https://github.com/daanx/isocline#formatted-output)
styleDef :: String -> Style -> IO ()
styleDef name style
  = withUTF8String0 name $ \cname ->
    withUTF8String0 style $ \cstyle ->
    do ic_style_def cname cstyle

-- | Open a style that is active for all 'putFmt' and 'putFmtLn' until it is closed again (`styleClose`).
styleOpen :: Style -> IO ()
styleOpen style
  = withUTF8String0 style $ \cstyle ->
    do ic_style_open cstyle        

-- | Close a previously opened style.
styleClose :: IO ()
styleClose 
  = ic_style_close

-- | Use a style over an action.
withStyle :: Style -> IO a -> IO a
withStyle style action
  = bracket (styleOpen style) (\() -> styleClose) (\() -> action) 


----------------------------------------------------------------------------
-- Terminal
----------------------------------------------------------------------------

foreign import ccall ic_term_init       :: IO ()
foreign import ccall ic_term_done       :: IO ()
foreign import ccall ic_term_flush      :: IO ()
foreign import ccall ic_term_write      :: CString -> IO ()
foreign import ccall ic_term_writeln    :: CString -> IO ()
foreign import ccall ic_term_underline  :: CCBool -> IO ()
foreign import ccall ic_term_reverse    :: CCBool -> IO ()
foreign import ccall ic_term_color_ansi :: CCBool -> CInt -> IO ()
foreign import ccall ic_term_color_rgb  :: CCBool -> CInt -> IO ()
foreign import ccall ic_term_style      :: CString -> IO ()
foreign import ccall ic_term_reset      :: IO ()

-- | Initialize the terminal for the @term@ functions.
-- Does nothing on most platforms but on windows enables UTF8 output
-- and potentially enables virtual terminal processing.
-- See also 'withTerm'.
termInit :: IO ()
termInit 
  = ic_term_init

-- | Done using @term@ functions.
-- See also 'withTerm'.
termDone :: IO ()
termDone 
  = ic_term_done

-- | Use the @term@ functions (brackets 'termInit' and 'termDone').
withTerm :: IO a -> IO a
withTerm action
  = bracket termInit (\() -> termDone) (\() -> action) 

-- | Flush terminal output. Happens automatically on newline (@'\\n'@) characters as well.
termFlush :: IO ()
termFlush
  = ic_term_flush  

-- | Write output to the terminal where ANSI CSI sequences are
-- handled portably across platforms (including Windows).
termWrite :: String -> IO ()
termWrite s
  = withUTF8String0 s $ \cs -> ic_term_write cs

-- | Write output with a ending newline to the terminal where 
-- ANSI CSI sequences are handled portably across platforms (including Windows).
termWriteLn :: String -> IO ()
termWriteLn s
  = withUTF8String0 s $ \cs -> ic_term_writeln cs  

-- | Set the terminal text color as a hexadecimal number @0x@rrggbb. 
-- The color is auto adjusted for terminals with less colors.
termColor :: Int -> IO ()
termColor color
  = ic_term_color_rgb (cbool True) (toEnum color)

-- | Set the terminal text background color. The color is auto adjusted for terminals with less colors.
termBgColor :: Int -> IO ()
termBgColor color
  = ic_term_color_rgb (cbool False) (toEnum color)

-- | Set the terminal text color as an ANSI palette color (between @0@ and @255@). Use 256 for the default.
-- The color is auto adjusted for terminals with less colors.
termColorAnsi :: Int -> IO ()
termColorAnsi color
  = ic_term_color_ansi (cbool True) (toEnum color)

-- | Set the terminal text background color as an ANSI palette color (between @0@ and @255@). Use 256 for the default.
-- The color is auto adjusted for terminals with less colors.
termBgColorAnsi :: Int -> IO ()
termBgColorAnsi color
  = ic_term_color_ansi (cbool False) (toEnum color)

-- | Set the terminal attributes from a style
termStyle :: Style -> IO ()
termStyle style
  = withUTF8String0 style $ \cstyle ->
    do ic_term_style cstyle

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
foreign import ccall ic_set_prompt_marker :: CString -> CString -> IO ()
foreign import ccall ic_get_prompt_marker :: IO CString
foreign import ccall ic_get_continuation_prompt_marker :: IO CString
foreign import ccall ic_enable_multiline  :: CCBool -> IO CCBool
foreign import ccall ic_enable_beep       :: CCBool -> IO CCBool
foreign import ccall ic_enable_color      :: CCBool -> IO CCBool
foreign import ccall ic_enable_auto_tab   :: CCBool -> IO CCBool
foreign import ccall ic_enable_inline_help:: CCBool -> IO CCBool
foreign import ccall ic_enable_hint       :: CCBool -> IO CCBool
foreign import ccall ic_set_hint_delay    :: CLong -> IO CLong
foreign import ccall ic_enable_highlight  :: CCBool -> IO CCBool
foreign import ccall ic_enable_history_duplicates :: CCBool -> IO CCBool
foreign import ccall ic_enable_completion_preview :: CCBool -> IO CCBool
foreign import ccall ic_enable_multiline_indent   :: CCBool -> IO CCBool
foreign import ccall ic_enable_brace_matching     :: CCBool -> IO CCBool
foreign import ccall ic_enable_brace_insertion    :: CCBool -> IO CCBool
foreign import ccall ic_set_matching_braces       :: CString -> IO ()
foreign import ccall ic_set_insertion_braces      :: CString -> IO ()

cbool :: Bool -> CCBool
cbool True  = toEnum 1
cbool False = toEnum 0

uncbool :: IO CCBool -> IO Bool
uncbool action
  = do i <- action
       return (i /= toEnum 0)

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


-- | Disable or enable brace matching (enabled by default)
-- Returns the previous value.
enableBraceMatching :: Bool -> IO Bool
enableBraceMatching enable
  = do uncbool $ ic_enable_brace_matching (cbool enable)

-- | Disable or enable automatic close brace insertion (enabled by default)
-- Returns the previous value.
enableBraceInsertion :: Bool -> IO Bool
enableBraceInsertion enable
  = do uncbool $ ic_enable_brace_insertion (cbool enable)

-- | Set pairs of matching braces, by default @\"(){}[]\"@.
setMatchingBraces :: String -> IO ()
setMatchingBraces bracePairs
  = withUTF8String0 bracePairs $ \cbracePairs ->
    do ic_set_matching_braces cbracePairs

-- | Set pairs of auto insertion braces, by default @\"(){}[]\\\"\\\"\'\'\"@.
setInsertionBraces :: String -> IO ()
setInsertionBraces bracePairs
  = withUTF8String0 bracePairs $ \cbracePairs ->
    do ic_set_insertion_braces cbracePairs


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

-- | Set the delay in milliseconds before a hint is displayed (500ms by default)
-- See also 'enableHint'
setHintDelay :: Int -> IO Int
setHintDelay ms
  = do cl <- ic_set_hint_delay (toEnum ms)
       return (fromEnum cl)


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
       
