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
main  = `withRepline` $ \\rp ->
        do putStrLn \"Welcome\"
           `setPromptColor` rp `Green`
           `setHistory` rp \"history.txt\" 200
           input \<- `readline` rp \"myprompt\"     -- full prompt becomes \"myprompt> \"
           putStrLn (\"You wrote:\\n\" ++ input)
@

A larger [example](https://github.com/daanx/repline/blob/main/test/Example.hs) 
with custom tab completion can be found in the Github repository.

Enjoy,
-- Daan
-}
module System.Console.Repline( 
      -- * Readline
      Rp,       
      withRepline,
      readline, 
    
      -- * History
      setHistory,
      historyClear,
      historyRemoveLast,
      historyAdd,

      -- * Completion
      Completions,
      setCompleter,
      addCompletion,
      completeFileName,
      completeWord,
      completeQuotedWord,

      -- * Configuration
      setPromptColor,
      setPromptMarker,
      enableColor,
      enableBeep,
      enableMultiline,
      enableHistoryDuplicates,
      Color(..), 
      
      -- * Advanced
      initialize, 
      done, 
      readlineMaybe
    ) where


import Data.List( intersperse )
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

data RpEnv
data RpCompletions  

-- | The Repline environment.
newtype Rp          = Rp (Ptr RpEnv)

-- | Abstract list of current completions.
newtype Completions = Completions (Ptr RpCompletions)

type CCompleterFun = Ptr RpCompletions -> CString -> IO ()
type CompleterFun  = Completions -> String -> IO ()


----------------------------------------------------------------------------
-- Basic readline
----------------------------------------------------------------------------

foreign import ccall rp_init      :: IO (Ptr RpEnv)
foreign import ccall rp_done      :: Ptr RpEnv -> IO ()
foreign import ccall rp_readline  :: Ptr RpEnv -> CString -> IO CString
foreign import ccall rp_free      :: Ptr RpEnv -> (Ptr a) -> IO () 

-- | Initialize Repline (see also 'withRepline')
initialize :: IO Rp
initialize 
  = fmap Rp rp_init

-- | Discard the Repline environment (automatically done at program exit as well)
done :: Rp -> IO ()
done (Rp rpenv) 
  = rp_done rpenv

-- | @readline rp prompt@: Read (multi-line) input from the user with rich editing abilities. 
-- Takes the prompt text as an argument. The full prompt is the combination
-- of the given prompt and the promp marker (@\"> \"@ by default) .
-- See also 'enableMultiline', 'setPromptColor', and 'setPromptMarker'.
readline :: Rp -> String -> IO String  
readline rp prompt
  = do mbRes <- readlineMaybe rp prompt
       case mbRes of
         Just s  -> return s
         Nothing -> return ""

-- | As 'readline' but returns 'Nothing' on end-of-file or other errors (ctrl-C/ctrl-D).
readlineMaybe:: Rp -> String -> IO (Maybe String)
readlineMaybe (Rp rpenv) prompt
  = withUTF8String prompt $ \cprompt ->
    do cres <- rp_readline rpenv cprompt
       res  <- peekUTF8StringMaybe cres
       rp_free rpenv cres
       return res


----------------------------------------------------------------------------
-- History
----------------------------------------------------------------------------

foreign import ccall rp_set_history           :: Ptr RpEnv -> CString -> CInt -> IO ()
foreign import ccall rp_history_remove_last   :: Ptr RpEnv -> IO ()
foreign import ccall rp_history_clear         :: Ptr RpEnv -> IO ()
foreign import ccall rp_history_add           :: Ptr RpEnv -> CString -> IO ()

-- | @setHistory rp filename maxEntries@: 
-- Enable history that is persisted to the given file path with a given maximum number of entries.
-- Use -1 for the default entries (200).
-- See also 'enableHistoryDuplicates'.
setHistory :: Rp -> FilePath -> Int -> IO ()
setHistory (Rp rp) fname maxEntries
  = withUTF8String0 fname $ \cfname ->
    do rp_set_history rp cfname (toEnum maxEntries)

-- | Repline automatically adds input of more than 1 character to the history.
-- This command removes the last entry.
historyRemoveLast :: Rp -> IO ()
historyRemoveLast (Rp rp)
  = rp_history_remove_last rp

-- | Clear the history.
historyClear :: Rp -> IO ()
historyClear (Rp rp)
  = rp_history_clear rp

-- | @withRepline action@: Perform @action@ with a fresh Repline environment.
withRepline :: (Rp -> IO a) -> IO a
withRepline action
  = bracket initialize done action

-- | @historyAdd rp entry@: add @entry@ to the history.
historyAdd :: Rp -> String -> IO ()
historyAdd (Rp rp) entry
  = withUTF8String0 entry $ \centry ->
    do rp_history_add rp centry 

----------------------------------------------------------------------------
-- Completion
----------------------------------------------------------------------------

foreign import ccall rp_set_completer         :: Ptr RpEnv -> FunPtr CCompleterFun -> IO ()
foreign import ccall "wrapper" rp_make_completer :: CCompleterFun -> IO (FunPtr CCompleterFun)
foreign import ccall rp_add_completion        :: Ptr RpCompletions -> CString -> CString -> IO CChar
foreign import ccall rp_complete_filename     :: Ptr RpCompletions -> CString -> CChar -> CString -> IO ()
foreign import ccall rp_complete_word         :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> IO ()
foreign import ccall rp_complete_quoted_word  :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> CString -> CChar -> CString -> IO ()

-- | @setCompleter rp completer@: Set a new tab-completion function @completer@ 
-- that is called by Repline automatically. 
-- The callback is called with a 'Completions' context and the current user
-- input up to the cursor.
-- By default the 'completeFileName' completer is used.
-- This overwrites any previously set completer.
setCompleter :: Rp -> (Completions -> String -> IO ()) -> IO ()
setCompleter (Rp rp) completer 
  = do ccompleter <- makeCCompleter completer
       rp_set_completer rp ccompleter

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
    
-- | @completeFileName compls input dirSep roots@: 
-- Complete filenames with the given @input@, a possible directory separator @dirSep@
-- used to complete directories, and a list of root folders @roots@ to search from
-- (by default @["."]@).
-- For example, using @\'/\'@ as a directory separator, we get:
--
-- > /ho         --> /home/
-- > /home/.ba   --> /home/.bashrc
--
completeFileName :: Completions -> String -> Maybe Char -> [FilePath] -> IO ()
completeFileName (Completions rpc) prefx dirSep roots
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 (concat (intersperse ";" roots)) $ \croots ->
    do let cdirSep = case dirSep of
                       Nothing -> toEnum 0
                       Just c  -> castCharToCChar c
       rp_complete_filename rpc cprefx cdirSep croots

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
  

----------------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------------
foreign import ccall rp_set_prompt_color  :: Ptr RpEnv -> CInt -> IO ()
foreign import ccall rp_set_prompt_marker :: Ptr RpEnv -> CString -> IO ()
foreign import ccall rp_enable_multiline  :: Ptr RpEnv -> CCBool -> IO ()
foreign import ccall rp_enable_beep       :: Ptr RpEnv -> CCBool -> IO ()
foreign import ccall rp_enable_color      :: Ptr RpEnv -> CCBool -> IO ()
foreign import ccall rp_enable_history_duplicates :: Ptr RpEnv -> CCBool -> IO ()

-- use our own CBool for compatibility with an older base
type CCBool = CInt

cbool :: Bool -> CCBool
cbool True  = toEnum 1
cbool False = toEnum 0


-- | Set the color of the prompt.
setPromptColor :: Rp -> Color -> IO ()
setPromptColor (Rp rp) color
  = rp_set_prompt_color rp (toEnum (fromEnum color))


-- | Set the prompt marker. Pass @\"\"@ for the default marker (@\"> \"@).
setPromptMarker :: Rp -> String -> IO ()
setPromptMarker (Rp rp) marker
  = withUTF8String0 marker $ \cmarker ->
    do rp_set_prompt_marker rp cmarker

-- | Disable or enable multi-line input (enabled by default).
enableMultiline :: Rp -> Bool -> IO ()
enableMultiline (Rp rp) enable
  = do rp_enable_multiline rp (cbool enable)

-- | Disable or enable sound (enabled by default).
-- | A beep is used when tab cannot find any completion for example.
enableBeep :: Rp -> Bool -> IO ()
enableBeep (Rp rp) enable
  = do rp_enable_beep rp (cbool enable)

-- | Disable or enable color output (enabled by default).
enableColor :: Rp -> Bool -> IO ()
enableColor (Rp rp) enable
  = do rp_enable_color rp (cbool enable)

-- | Disable or enable duplicate entries in the history (duplicate entries are not allowed by default).
enableHistoryDuplicates :: Rp -> Bool -> IO ()
enableHistoryDuplicates (Rp rp) enable
  = do rp_enable_history_duplicates rp (cbool enable)



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
        ColorDefault -> 39

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
        _  -> ColorDefault


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
       