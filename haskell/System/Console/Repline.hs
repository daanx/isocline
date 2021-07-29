{- ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
---------------------------------------------------------------------------- -}
{-|
Description : Binding to the Repline library, a portable alternative to GNU Readline
Copyright   : (c) 2021, Daan Leijen
License     : MIT
Maintainer  : daan@effp.org
Stability   : Experimental

A Haskell wrapper around the [Repline C library](https://github.com/daanx/repline#readme) 
which can provide an alternative to GNU Readline.
(The Repline library is included whole and not a separate dependency).

Repline works across Unix, Windows, and macOS, and relies on a minimal subset of ANSI escape sequences.
It has a good multi-line editing mode (use shift/ctrl-enter) which is nice for inputting small functions etc.
Other features include support for colors, history, completion, unicode, undo/redo, 
incremental history search, etc.

Minimal example with history:

@
import System.Console.Repline

main :: IO ()
main  = do putStrLn \"Welcome\"
           `setHistory` \"history.txt\" 200
           input \<- `readline` \"myprompt\"     -- full prompt becomes \"myprompt> \"
           putStrLn (\"You wrote:\\n\" ++ input)
@

Or using custom completions with an interactive loop:

@
import System.Console.Repline
import Data.Char( toLower )

main :: IO ()
main 
  = do `setPromptColor` `Green`
       `setHistory` "history.txt" 200
       `enableAutoTab` `True`
       interaction

interaction :: IO ()
interaction 
  = do s <- `readlineWithCompleter` \"hαskell\" completer
       putStrLn (\"You wrote:\\n\" ++ s)
       if (s == \"\" || s == \"exit\") then return () else interaction
                     
completer :: `Completions` -> String -> IO () 
completer compl input
  = do `completeFileName` compl input Nothing [\".\",\"\/usr\/local\"] [\".hs\"]  -- use [] for any extension
       `addCompletionsFor` compl (map toLower input) 
          [\"print\",\"println\",\"prints\",\"printsln\",\"prompt\"]
       return ()
@

A larger [example](https://github.com/daanx/repline/blob/main/test/Example.hs) 
with more extenstive custom completion can be found in the [Github repository](https://github.com/daanx/repline).

Enjoy,
-- Daan
-}
module System.Console.Repline( 
      -- * Readline
      readline, 
      readlineWithCompleter,      
      
      -- * History
      setHistory,
      historyClear,
      historyRemoveLast,
      historyAdd,

      -- * Completion
      Completions,      
      addCompletion,
      addCompletionsFor,
      completeFileName,
      completeWord,
      completeQuotedWord,

      -- * Syntax Highlighting 
      TextAttr(..),
      setHighlighterAttr,
      setHighlighterEsc,
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
      
      -- * Configuration
      setPromptMarker,
      setPromptColor,
      setReplineColors,
      
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
      
      -- * Advanced
      setDefaultCompleter,      
      readlineMaybe,
      readlineWithCompleterMaybe,   

      -- * Low-level highlighting
      Highlight,      
      setHighlighter,
      highlightEsc,
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

data RpCompletions  

-- | Abstract list of current completions.
newtype Completions = Completions (Ptr RpCompletions)

type CCompleterFun = Ptr RpCompletions -> CString -> IO ()
type CompleterFun  = Completions -> String -> IO ()


----------------------------------------------------------------------------
-- Basic readline
----------------------------------------------------------------------------

foreign import ccall rp_free      :: (Ptr a) -> IO () 
foreign import ccall rp_readline  :: CString -> IO CString
foreign import ccall rp_readline_with_completer  :: CString -> FunPtr CCompleterFun -> (Ptr a) -> IO CString

-- | @readline prompt@: Read (multi-line) input from the user with rich editing abilities. 
-- Takes the prompt text as an argument. The full prompt is the combination
-- of the given prompt and the promp marker (@\"> \"@ by default) .
-- See also 'readlineWithCompleter', 'readlineMaybe', 'enableMultiline', 'setPromptColor', and 'setPromptMarker'.
readline :: String -> IO String  
readline prompt
  = do mbRes <- readlineMaybe prompt
       case mbRes of
         Just s  -> return s
         Nothing -> return ""

-- | As 'readline' but returns 'Nothing' on end-of-file or other errors (ctrl-C/ctrl-D).
readlineMaybe:: String -> IO (Maybe String)
readlineMaybe prompt
  = withUTF8String prompt $ \cprompt ->
    do cres <- rp_readline cprompt
       res  <- peekUTF8StringMaybe cres
       rp_free cres
       return res


-- | @readlineWithCompleter prompt completer@: as 'readline' but
-- uses the given @completer@ function to complete words on @tab@ (instead of the default completer). 
-- See also 'readline' and 'setDefaultCompleter'.
readlineWithCompleter :: String -> (Completions -> String -> IO ()) -> IO String
readlineWithCompleter prompt completer 
  = do mbRes <- readlineWithCompleterMaybe prompt completer
       case mbRes of
         Just s  -> return s
         Nothing -> return ""


-- | As 'readlineWithCompleter' but returns 'Nothing' on end-of-file or other errors (ctrl-C/ctrl-D).
-- See also 'readlineWithCompleter'.
readlineWithCompleterMaybe :: String -> (Completions -> String -> IO ()) -> IO (Maybe String) 
readlineWithCompleterMaybe prompt completer 
  = withUTF8String prompt $ \cprompt ->
    do ccompleter <- makeCCompleter completer
       cres <- rp_readline_with_completer cprompt ccompleter nullPtr
       res  <- peekUTF8StringMaybe cres
       rp_free cres
       freeHaskellFunPtr ccompleter
       return res

----------------------------------------------------------------------------
-- History
----------------------------------------------------------------------------

foreign import ccall rp_set_history           :: CString -> CInt -> IO ()
foreign import ccall rp_history_remove_last   :: IO ()
foreign import ccall rp_history_clear         :: IO ()
foreign import ccall rp_history_add           :: CString -> IO ()

-- | @setHistory filename maxEntries@: 
-- Enable history that is persisted to the given file path with a given maximum number of entries.
-- Use -1 for the default entries (200).
-- See also 'enableHistoryDuplicates'.
setHistory :: FilePath -> Int -> IO ()
setHistory fname maxEntries
  = withUTF8String0 fname $ \cfname ->
    do rp_set_history cfname (toEnum maxEntries)

-- | Repline automatically adds input of more than 1 character to the history.
-- This command removes the last entry.
historyRemoveLast :: IO ()
historyRemoveLast 
  = rp_history_remove_last

-- | Clear the history.
historyClear :: IO ()
historyClear
  = rp_history_clear

-- | @historyAdd entry@: add @entry@ to the history.
historyAdd :: String -> IO ()
historyAdd entry
  = withUTF8String0 entry $ \centry ->
    do rp_history_add centry 


----------------------------------------------------------------------------
-- Completion
----------------------------------------------------------------------------
-- use our own CBool for compatibility with an older base
type CCBool = CInt

type CCharClassFun = CString -> CLong -> IO CCBool
type CharClassFun  = Char -> Bool

foreign import ccall rp_set_default_completer :: FunPtr CCompleterFun -> IO ()
foreign import ccall "wrapper" rp_make_completer :: CCompleterFun -> IO (FunPtr CCompleterFun)
foreign import ccall "wrapper" rp_make_charclassfun :: CCharClassFun -> IO (FunPtr CCharClassFun)

foreign import ccall rp_add_completion        :: Ptr RpCompletions -> CString -> CString -> IO CChar
foreign import ccall rp_complete_filename     :: Ptr RpCompletions -> CString -> CChar -> CString -> CString -> IO ()
foreign import ccall rp_complete_word         :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> IO ()
foreign import ccall rp_complete_quoted_word  :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> FunPtr CCharClassFun -> CChar -> CString -> IO ()

-- | @setDefaultCompleter completer@: Set a new tab-completion function @completer@ 
-- that is called by Repline automatically. 
-- The callback is called with a 'Completions' context and the current user
-- input up to the cursor.
-- By default the 'completeFileName' completer is used.
-- This overwrites any previously set completer.
setDefaultCompleter :: (Completions -> String -> IO ()) -> IO ()
setDefaultCompleter completer 
  = do ccompleter <- makeCCompleter completer
       rp_set_default_completer ccompleter

makeCCompleter :: CompleterFun -> IO (FunPtr CCompleterFun)
makeCCompleter completer
  = rp_make_completer wrapper
  where
    wrapper :: Ptr RpCompletions -> CString -> IO ()
    wrapper rpcomp cprefx
      = do prefx <- peekUTF8String0 cprefx
           completer (Completions rpcomp) prefx


-- | @addCompletion compl display completion@: Inside a completer callback, add a new completion with a 
-- @display@ string and @completion@ string. If display is empty, the completion is used to 
-- display as well. If 'addCompletion' returns 'True' keep adding completions,
-- but if it returns 'False' an effort should be made to return from the completer
-- callback without adding more completions.
addCompletion :: Completions -> String -> String -> IO Bool
addCompletion (Completions rpc) display completion 
  = withUTF8String0 display $ \cdisplay ->
    withUTF8String completion $ \ccompletion ->
    do cbool <- rp_add_completion rpc cdisplay ccompletion
       return (fromEnum cbool /= 0)
    
-- | @addCompletionsFor compl input candidates@: add completions for any candidate
-- string in @candidates@ for which @input@ is a prefix.
addCompletionsFor :: Completions -> String -> [String] -> IO Bool
addCompletionsFor compl input candidates
  = do results <- mapM add (filter (input `isPrefixOf`) candidates)
       return (and results)
  where
    add completion 
      = addCompletion compl completion completion

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
completeFileName :: Completions -> String -> Maybe Char -> [FilePath] -> [String] -> IO ()
completeFileName (Completions rpc) prefx dirSep roots extensions
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 (concat (intersperse ";" roots)) $ \croots ->
    withUTF8String0 (concat (intersperse ";" extensions)) $ \cextensions ->
    do let cdirSep = case dirSep of
                       Nothing -> toEnum 0
                       Just c  -> castCharToCChar c
       rp_complete_filename rpc cprefx cdirSep croots cextensions

-- | @completeWord compl input completer@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'Completions' environment @compl@, the current @input@, and a user defined 
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
-- @('completeQuotedWord' compl prefx fun \" \\t\\r\\n\" \'\\\\\' \"\'\\\"\")@.
completeWord :: Completions -> String -> (Completions -> String -> IO ()) -> IO () 
completeWord (Completions rpc) prefx completer
  = withUTF8String prefx $ \cprefx ->
    do ccompleter <- makeCCompleter completer
       rp_complete_word rpc cprefx ccompleter
       freeHaskellFunPtr ccompleter
  
-- | @completeQuotedWord compl input completer isWordChar escapeChar quoteChars@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'Completions' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- Unlike 'completeWord', this function takes an explicit function to determine /word/ characters,
-- the /escape/ character, and a string of /quote/ characters.
-- See also 'completeWord'.
completeQuotedWord :: Completions -> String -> (Completions -> String -> IO ()) -> (Char -> Bool) -> Maybe Char -> String -> IO () 
completeQuotedWord (Completions rpc) prefx completer isWordChar escapeChar quoteChars
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 quoteChars $ \cquoteChars ->
    do let cescapeChar = case escapeChar of
                          Nothing -> toEnum 0
                          Just c  -> castCharToCChar c                      
       ccompleter <- makeCCompleter completer
       cisWordChar <- makeCharClassFun isWordChar
       rp_complete_quoted_word rpc cprefx ccompleter cisWordChar cescapeChar cquoteChars
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
    in rp_make_charclassfun charClassFun



----------------------------------------------------------------------------
-- Syntax highlighting
----------------------------------------------------------------------------

data RpHighlightEnv

-- | Abstract highlight environment
newtype Highlight = Highlight (Ptr RpHighlightEnv)    

type CHighlightFun = Ptr RpHighlightEnv -> CString -> Ptr () -> IO ()
type HighlightFun  = Highlight -> String -> IO ()

foreign import ccall rp_set_highlighter     :: FunPtr CHighlightFun -> Ptr () -> IO ()
foreign import ccall "wrapper" rp_make_highlight_fun:: CHighlightFun -> IO (FunPtr CHighlightFun)

foreign import ccall rp_highlight_color     :: Ptr RpHighlightEnv -> CLong -> CInt -> IO ()
foreign import ccall rp_highlight_bgcolor   :: Ptr RpHighlightEnv -> CLong -> CInt -> IO ()
foreign import ccall rp_highlight_underline :: Ptr RpHighlightEnv -> CLong -> CInt -> IO ()
foreign import ccall rp_highlight_reverse   :: Ptr RpHighlightEnv -> CLong -> CInt -> IO ()

type CHighlightEscFun = CString -> Ptr () -> IO CString
type HighlightEscFun  = String -> String

foreign import ccall rp_set_highlighter_esc :: FunPtr CHighlightEscFun -> IO ()
foreign import ccall rp_highlight_esc       :: Ptr RpHighlightEnv -> CString -> FunPtr CHighlightEscFun -> Ptr () -> IO ()
foreign import ccall "wrapper" rp_make_highlight_esc_fun:: CHighlightEscFun -> IO (FunPtr CHighlightEscFun)
foreign import ccall rp_strdup              :: CString -> IO CString

-- | Set a syntax highlighter.
-- There can only be one highlight function, setting it again disables the previous one.
-- See also 'setHighlighterEsc'.
setHighlighter :: (Highlight -> String -> IO ()) -> IO ()
setHighlighter highlightFun
  = do chlFun <- rp_make_highlight_fun chighlightFun 
       rp_set_highlighter chlFun nullPtr
  where
    chighlightFun henv cinput carg
      = do input <- peekUTF8String0 cinput
           highlightFun (Highlight henv) input


-- | Set a syntax highlighter that uses a pure function to insert ANSI CSI SGR sequences
-- to highlight the code. This should function should _not_ add or delete characters
-- and only add basic CSI sequences to set the color, background color, underline, or reverse video.
-- See <https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters>
-- There can only be one highlight function, setting it again disables the previous one.
-- 
-- See also `setHighlighterAttr` for a highlighter based on text attributes instead.
setHighlighterEsc :: (String -> String) -> IO ()
setHighlighterEsc highlight
  = do cfun <- rp_make_highlight_esc_fun chighlight
       rp_set_highlighter_esc cfun
  where
    chighlight cinput carg
      = do input <- peekUTF8String0 cinput
           withUTF8String0 (highlight input) $ \coutput ->
             rp_strdup coutput

-- | Use an escape sequence highlighter from inside `setHighlighter`.
-- It is recommended to use ` setHighlighterEsc` instead.
highlightEsc :: Highlight -> String -> (String -> String) -> IO ()
highlightEsc (Highlight henv) input highlight
  = withUTF8String0 input $ \cinput ->
    do cfun <- rp_make_highlight_esc_fun chighlight
       rp_highlight_esc henv cinput cfun nullPtr
  where
    chighlight cinput carg
      = do input <- peekUTF8String0 cinput
           withUTF8String0 (highlight input) $ \coutput ->
             rp_strdup coutput


-- | @highlightColor henv pos color@: Set the color of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this color until another
-- color is set again.
highlightColor :: Highlight -> Int -> Color -> IO ()
highlightColor (Highlight henv) pos color 
  = do rp_highlight_color henv (clong (-pos)) (ccolor color)

-- | @highlightBgColor henv pos bgcolor@: Set the background color of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this background color until another
-- background color is set again.
highlightBgColor :: Highlight -> Int -> Color -> IO ()
highlightBgColor (Highlight henv) pos color 
  = do rp_highlight_bgcolor henv (clong (-pos)) (ccolor color)

-- | @highlightUnderline henv pos bgcolor@: Set underline of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this underlining until another
-- underlining is set again.
highlightUnderline :: Highlight -> Int -> Bool -> IO ()
highlightUnderline (Highlight henv) pos enable
  = do rp_highlight_underline henv (clong (-pos)) (cbool enable) 

-- | @highlightReverse henv pos bgcolor@: Set reverse video mode of a character
-- at position @pos@ in the input (from inside a highlighter).
-- All following characters will have this reverse mode until another
-- reverse mode is set again.
highlightReverse :: Highlight -> Int -> Bool -> IO ()
highlightReverse (Highlight henv) pos enable
  = do rp_highlight_reverse henv (clong (-pos)) (cbool enable) 


-- | Text attributes for a single character.
data TextAttr = TextAttr{ 
  attrColor     :: Color, -- ^ color
  attrBgColor   :: Color, -- ^ background color
  attrUnderline :: Bool,  -- ^ underline
  attrReverse   :: Bool   -- ^ reverse video
}

-- | Default text attribute.
attrDefault :: TextAttr
attrDefault = TextAttr ColorDefault ColorDefault False False 

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
highlightSetAttrDiff :: Highlight -> Int -> TextAttr -> TextAttr -> IO ()
highlightSetAttrDiff henv pos current attr
  = do when (attrColor current /= attrColor attr)         $ highlightColor henv pos (attrColor attr)
       when (attrBgColor current /= attrBgColor attr)     $ highlightBgColor henv pos (attrBgColor attr)
       when (attrUnderline current /= attrUnderline attr) $ highlightUnderline henv pos (attrUnderline attr)
       when (attrReverse current /= attrReverse attr)     $ highlightReverse henv pos (attrReverse attr)
       return ()

-- | @highlightSetAttr henv pos attr@: Set new text attribute @attr@ 
-- at position @pos@. 
highlightSetAttr :: Highlight -> Int -> TextAttr -> IO ()
highlightSetAttr henv pos (TextAttr color bgcolor underline reverse) 
  = do -- todo: add one function to set all at once at the C side
       highlightColor henv pos color
       highlightBgColor henv pos bgcolor
       highlightUnderline henv pos underline
       highlightReverse henv pos reverse

-- | Set a syntax highlighter that uses a pure function to return a list
-- of text attributes for each character in the input.
-- There can only be one highlight function, setting it again disables the previous one.
--
-- See also `setHighlighterEsc` for a highlight function based on escape sequences instead.
setHighlighterAttr :: (String -> [TextAttr]) -> IO ()
setHighlighterAttr highlight 
  = setHighlighter highlightFun
  where 
    highlightFun :: Highlight -> String -> IO ()
    highlightFun henv input
      = do let attrs = highlight input               
           foldM (setAttr henv) attrDefault (zip [0..] attrs)
           return ()
    
    setAttr :: Highlight -> TextAttr -> (Int,TextAttr) -> IO TextAttr
    setAttr henv current (pos,attr) 
      = do highlightSetAttrDiff henv pos current attr
           return attr


----------------------------------------------------------------------------
-- Terminal
----------------------------------------------------------------------------

foreign import ccall rp_write  :: CString -> IO ()
foreign import ccall rp_writeln :: CString -> IO ()

-- | Write output to the terminal where ANSI CSI sequences are
-- handled portably across platforms (including Windows).
termWrite :: String -> IO ()
termWrite s
  = withUTF8String0 s $ \cs -> rp_write cs

-- | Write output with a ending newline to the terminal where 
-- ANSI CSI sequences are handled portably across platforms (including Windows).
termWriteLn :: String -> IO ()
termWriteLn s
  = withUTF8String0 s $ \cs -> rp_writeln cs  


----------------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------------
foreign import ccall rp_set_prompt_color  :: CInt -> IO ()
foreign import ccall rp_set_iface_colors  :: CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall rp_set_prompt_marker :: CString -> CString -> IO ()
foreign import ccall rp_enable_multiline  :: CCBool -> IO ()
foreign import ccall rp_enable_beep       :: CCBool -> IO ()
foreign import ccall rp_enable_color      :: CCBool -> IO ()
foreign import ccall rp_enable_auto_tab   :: CCBool -> IO ()
foreign import ccall rp_enable_inline_help:: CCBool -> IO ()
foreign import ccall rp_enable_hint       :: CCBool -> IO ()
foreign import ccall rp_enable_highlight  :: CCBool -> IO ()
foreign import ccall rp_enable_history_duplicates :: CCBool -> IO ()
foreign import ccall rp_enable_completion_preview :: CCBool -> IO ()
foreign import ccall rp_enable_multiline_indent   :: CCBool -> IO ()



cbool :: Bool -> CCBool
cbool True  = toEnum 1
cbool False = toEnum 0

ccolor :: Color -> CInt
ccolor clr = toEnum (fromEnum clr)

clong :: Int -> CLong
clong l = toEnum l


-- | Set the color of the prompt.
setPromptColor :: Color -> IO ()
setPromptColor color
  = rp_set_prompt_color (ccolor color)


-- | @setPromptMarker marker multiline_marker@: Set the prompt @marker@ (by default @\"> \"@). 
-- and a possible different continuation prompt marker @multiline_marker@ for multiline 
-- input (defaults to @marker@).
setPromptMarker :: String -> String -> IO ()
setPromptMarker marker multiline_marker  
  = withUTF8String0 marker $ \cmarker ->
    withUTF8String0 multiline_marker $ \cmultiline_marker ->
    do rp_set_prompt_marker cmarker cmultiline_marker

-- | Disable or enable multi-line input (enabled by default).
enableMultiline :: Bool -> IO ()
enableMultiline enable
  = do rp_enable_multiline (cbool enable)

-- | Disable or enable sound (enabled by default).
-- | A beep is used when tab cannot find any completion for example.
enableBeep :: Bool -> IO ()
enableBeep enable
  = do rp_enable_beep (cbool enable)

-- | Disable or enable color output (enabled by default).
enableColor :: Bool -> IO ()
enableColor enable
  = do rp_enable_color (cbool enable)

-- | Disable or enable duplicate entries in the history (duplicate entries are not allowed by default).
enableHistoryDuplicates :: Bool -> IO ()
enableHistoryDuplicates enable
  = do rp_enable_history_duplicates (cbool enable)


-- | Disable or enable automatic tab completion after a completion 
-- to expand as far as possible if the completions are unique. (disabled by default).
enableAutoTab :: Bool -> IO ()
enableAutoTab enable
  = do rp_enable_auto_tab (cbool enable)


-- | Disable or enable short inline help message (for history search etc.) (enabled by default).
-- Pressing F1 always shows full help regardless of this setting. 
enableInlineHelp :: Bool -> IO ()
enableInlineHelp enable
  = do rp_enable_inline_help (cbool enable)

-- | Disable or enable preview of a completion selection (enabled by default)
enableCompletionPreview :: Bool -> IO ()
enableCompletionPreview enable
  = do rp_enable_completion_preview (cbool enable)

-- | Set the color used for interface elements:
--
-- - info: for example, numbers in the completion menu (`DarkGray` by default).
-- - diminish: for example, non matching parts in a history search (`LightGray` by default).
-- - emphasis: for example, the matching part in a history search (`White` by default).
-- - hint: for inline hints (`DarkGray` by default).
--
-- Use `ColorNone` to use the default color. (but `ColorDefault` for the default terminal text color!
setReplineColors :: Color -> Color -> Color -> Color -> IO ()
setReplineColors colorInfo colorDiminish colorEmphasis colorHint
  = rp_set_iface_colors (ccolor colorInfo) (ccolor colorDiminish) (ccolor colorEmphasis) (ccolor colorHint)

-- | Disable or enable automatic indentation to line up the
-- multiline prompt marker with the initial prompt marker (enabled by default).
-- See also 'setPromptMarker'.
enableMultilineIndent :: Bool -> IO ()
enableMultilineIndent enable
  = do rp_enable_multiline_indent (cbool enable)

-- | Disable or enable automatic inline hinting (enabled by default)
enableHint :: Bool -> IO ()
enableHint enable
  = do rp_enable_hint (cbool enable)

-- | Disable or enable syntax highlighting (enabled by default).
enableHighlight :: Bool -> IO ()
enableHighlight enable
  = do rp_enable_highlight (cbool enable)


----------------------------------------------------------------------------
-- Colors
----------------------------------------------------------------------------

-- | Terminal colors. Used for example in 'setPromptColor'.
data Color  = Black
            | Maroon
            | Green
            | Orange
            | Navy
            | Purple
            | Teal
            | LightGray
            | DarkGray
            | Red
            | Lime
            | Yellow
            | Blue
            | Magenta
            | Cyan
            | White
            | ColorDefault
            | ColorNone
            deriving (Show,Eq,Ord)

instance Enum Color where
  fromEnum color 
    = case color of
        Black       -> 30
        Maroon      -> 31
        Green       -> 32
        Orange      -> 33
        Navy        -> 34
        Purple      -> 35
        Teal        -> 36
        LightGray   -> 37
        DarkGray    -> 90
        Red         -> 91
        Lime        -> 92
        Yellow      -> 93
        Blue        -> 94
        Magenta     -> 95
        Cyan        -> 96
        White       -> 97
        ColorDefault-> 39
        ColorNone   -> 0

  toEnum color 
    = case color of
        30 -> Black
        31 -> Maroon
        32 -> Green
        33 -> Orange
        34 -> Navy
        35 -> Purple
        36 -> Teal
        37 -> LightGray
        90 -> DarkGray
        91 -> Red
        92 -> Lime
        93 -> Yellow
        94 -> Blue
        95 -> Magenta
        96 -> Cyan
        97 -> White
        39 -> ColorDefault
        _  -> ColorNone


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
       