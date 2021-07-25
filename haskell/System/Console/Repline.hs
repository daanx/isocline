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
The Repline library is included and not a separate dependency.

Repline works across Unix, Windows, and macOS, and relies on a minimal subset of ANSI escape sequences.
It has a good multi-line editing mode (use shift/ctrl-enter) which is nice for inputting small functions etc.
Other features include support for colors, history, completion, unicode, undo/redo, 
incremental history search, etc.

Minimal example:

@
import System.Console.Repline

main :: IO ()
main  = do putStrLn \"Welcome\"
           `setPromptColor` `Green`
           `setHistory` \"history.txt\" 200
           input \<- `readline` \"myprompt\"     -- full prompt becomes \"myprompt> \"
           putStrLn (\"You wrote:\\n\" ++ input)
@

A larger [example](https://github.com/daanx/repline/blob/main/test/Example.hs) 
with custom tab completion can be found in the Github repository.

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
      
      Color(..), 
      
      -- * Advanced
      setDefaultCompleter,      
      readlineMaybe,
      readlineWithCompleterMaybe      
    ) where


import Data.List( intersperse, isPrefixOf )
import Control.Exception( bracket )
import Foreign.C.String( CString, peekCString, withCString, castCharToCChar )
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
-- See also 'enableMultiline', 'setPromptColor', and 'setPromptMarker'.
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


readlineWithCompleter :: String -> (Completions -> String -> IO ()) -> IO String
readlineWithCompleter prompt completer 
  = do mbRes <- readlineWithCompleterMaybe prompt completer
       case mbRes of
         Just s  -> return s
         Nothing -> return ""


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

foreign import ccall rp_set_completer         :: FunPtr CCompleterFun -> IO ()
foreign import ccall "wrapper" rp_make_completer :: CCompleterFun -> IO (FunPtr CCompleterFun)
foreign import ccall rp_add_completion        :: Ptr RpCompletions -> CString -> CString -> IO CChar
foreign import ccall rp_complete_filename     :: Ptr RpCompletions -> CString -> CChar -> CString -> CString -> IO ()
foreign import ccall rp_complete_word         :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> IO ()
foreign import ccall rp_complete_quoted_word  :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> CString -> CChar -> CString -> IO ()

-- | @setDefaultCompleter completer@: Set a new tab-completion function @completer@ 
-- that is called by Repline automatically. 
-- The callback is called with a 'Completions' context and the current user
-- input up to the cursor.
-- By default the 'completeFileName' completer is used.
-- This overwrites any previously set completer.
setDefaultCompleter :: (Completions -> String -> IO ()) -> IO ()
setDefaultCompleter completer 
  = do ccompleter <- makeCCompleter completer
       rp_set_completer ccompleter

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
-- Complete filenames with the given @input@, a possible directory separator @dirSep@
-- used to complete directories, a list of root folders @roots@  to search from
-- (by default @["."]@), and a list of extensions to match (use @[]@ to match any extension).
-- For example, using @\'/\'@ as a directory separator, we get:
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
  
-- | @completeQuotedWord compl input completer nonWordChars escapeChar quoteChars@: 
-- Complete a /word/ taking care of automatically quoting and escaping characters.
-- Takes the 'Completions' environment @compl@, the current @input@, and a user defined 
-- @completer@ function that is called with adjusted input which is unquoted, unescaped,
-- and limited to the /word/ just before the cursor.
-- Unlike 'completeWord', this function takes an explicit string of /non-word/ characters,
-- the /escape/ character, and a string of /quote/ characters.
-- See also 'completeWord'.
completeQuotedWord :: Completions -> String -> (Completions -> String -> IO ()) -> String -> Maybe Char -> String -> IO () 
completeQuotedWord (Completions rpc) prefx completer nonWordChars escapeChar quoteChars
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 nonWordChars $ \cnonWordChars ->
    withUTF8String0 quoteChars $ \cquoteChars ->
    do let cescapeChar = case escapeChar of
                          Nothing -> toEnum 0
                          Just c  -> castCharToCChar c
       ccompleter <- makeCCompleter completer
       rp_complete_quoted_word rpc cprefx ccompleter cnonWordChars cescapeChar cquoteChars
       freeHaskellFunPtr ccompleter
  

----------------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------------
foreign import ccall rp_set_prompt_color  :: CInt -> IO ()
foreign import ccall rp_set_iface_colors  :: CInt -> CInt -> CInt -> IO ()
foreign import ccall rp_set_prompt_marker :: CString -> IO ()
foreign import ccall rp_enable_multiline  :: CCBool -> IO ()
foreign import ccall rp_enable_beep       :: CCBool -> IO ()
foreign import ccall rp_enable_color      :: CCBool -> IO ()
foreign import ccall rp_enable_history_duplicates :: CCBool -> IO ()
foreign import ccall rp_enable_auto_tab    :: CCBool -> IO ()
foreign import ccall rp_enable_completion_preview :: CCBool -> IO ()

-- use our own CBool for compatibility with an older base
type CCBool = CInt

cbool :: Bool -> CCBool
cbool True  = toEnum 1
cbool False = toEnum 0

ccolor :: Color -> CInt
ccolor clr = toEnum (fromEnum clr)


-- | Set the color of the prompt.
setPromptColor :: Color -> IO ()
setPromptColor color
  = rp_set_prompt_color (ccolor color)


-- | Set the prompt marker. Pass @\"\"@ for the default marker (@\"> \"@).
setPromptMarker :: String -> IO ()
setPromptMarker marker
  = withUTF8String0 marker $ \cmarker ->
    do rp_set_prompt_marker cmarker

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

-- | Disable or enable preview of a completion selection (enabled by default)
enableCompletionPreview :: Bool -> IO ()
enableCompletionPreview enable
  = do rp_enable_completion_preview (cbool enable)

-- | Set the color used for interface elements:
-- - info: for example, numbers in the completion menu (`DarkGray` by default)
-- - diminish: for example, non matching parts in a history search (`LightGray` by default)
-- - highlight: for example, the matching part in a history search (`White` by default)
-- Use `ColorNone` to use the default color. (but `ColorDefault` for the default terminal text color!
setReplineColors :: Color -> Color -> Color -> IO ()
setReplineColors colorInfo colorDiminish colorHighlight
  = rp_set_iface_colors (ccolor colorInfo) (ccolor colorDiminish) (ccolor colorHighlight)

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
       