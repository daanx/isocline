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

See <https://github.com/daanx/repline/haskell#readme> for more information.

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

Enjoy,
-- Daan
-}
module System.Console.Repline( 
      -- * Readline
      Rp,       
      withRepline,
      readline, 

      -- * Configuration
      Color(..), 
      setPromptColor,

      -- * History
      setHistory,
      historyClear,
      historyRemoveLast,

      -- * Completion
      Completions,
      setCompleter,
      addCompletion,
      completeFileName,
      completeWord,
      completeQuotedWord,

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

newtype Rp          = Rp (Ptr RpEnv)
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
foreign import ccall rp_set_prompt_color      :: Ptr RpEnv -> CInt -> IO ()


initialize :: IO Rp
initialize 
  = fmap Rp rp_init

done :: Rp -> IO ()
done (Rp rpenv) 
  = rp_done rpenv

readline :: Rp -> String -> IO String  
readline rp prompt
  = do mbRes <- readlineMaybe rp prompt
       case mbRes of
         Just s  -> return s
         Nothing -> return ""

readlineMaybe:: Rp -> String -> IO (Maybe String)
readlineMaybe (Rp rpenv) prompt
  = withUTF8String prompt $ \cprompt ->
    do cres <- rp_readline rpenv cprompt
       res  <- peekUTF8StringMaybe cres
       rp_free rpenv cres
       return res

setPromptColor :: Rp -> Color -> IO ()
setPromptColor (Rp rp) color
  = rp_set_prompt_color rp (toEnum (fromEnum color))



----------------------------------------------------------------------------
-- History
----------------------------------------------------------------------------

foreign import ccall rp_set_history           :: Ptr RpEnv -> CString -> CInt -> IO ()
foreign import ccall rp_history_remove_last   :: Ptr RpEnv -> IO ()
foreign import ccall rp_history_clear         :: Ptr RpEnv -> IO ()

setHistory :: Rp -> String -> Int -> IO ()
setHistory (Rp rp) fname maxEntries
  = withUTF8String0 fname $ \cfname ->
    do rp_set_history rp cfname (toEnum maxEntries)

historyRemoveLast :: Rp -> IO ()
historyRemoveLast (Rp rp)
  = rp_history_remove_last rp

historyClear :: Rp -> IO ()
historyClear (Rp rp)
  = rp_history_clear rp

withRepline :: (Rp -> IO a) -> IO a
withRepline action
  = bracket initialize done action



----------------------------------------------------------------------------
-- Completion
----------------------------------------------------------------------------

foreign import ccall rp_set_completer         :: Ptr RpEnv -> FunPtr CCompleterFun -> IO ()
foreign import ccall "wrapper" rp_make_completer :: CCompleterFun -> IO (FunPtr CCompleterFun)
foreign import ccall rp_add_completion        :: Ptr RpCompletions -> CString -> CString -> IO CChar
foreign import ccall rp_complete_filename     :: Ptr RpCompletions -> CString -> CChar -> CString -> IO ()
foreign import ccall rp_complete_word         :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> IO ()
foreign import ccall rp_complete_quoted_word  :: Ptr RpCompletions -> CString -> FunPtr CCompleterFun -> CString -> CChar -> CString -> IO ()


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


addCompletion :: Completions -> String -> String -> IO Bool
addCompletion (Completions rpc) display completion 
  = withUTF8String0 display $ \cdisplay ->
    withUTF8String completion $ \ccompletion ->
    do cbool <- rp_add_completion rpc cdisplay ccompletion
       return (fromEnum cbool /= 0)
    

completeFileName :: Completions -> String -> Maybe Char -> [FilePath] -> IO ()
completeFileName (Completions rpc) prefx dirSep roots
  = withUTF8String prefx $ \cprefx ->
    withUTF8String0 (concat (intersperse ";" roots)) $ \croots ->
    do let cdirSep = case dirSep of
                       Nothing -> toEnum 0
                       Just c  -> castCharToCChar c
       rp_complete_filename rpc cprefx cdirSep croots

completeWord :: Completions -> String -> (Completions -> String -> IO ()) -> IO () 
completeWord (Completions rpc) prefx completer
  = withUTF8String prefx $ \cprefx ->
    do ccompleter <- makeCCompleter completer
       rp_complete_word rpc cprefx ccompleter
  
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
-- Colors
----------------------------------------------------------------------------

-- Color
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
       