{- ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
---------------------------------------------------------------------------- -}

module System.Console.Repline( 
      Rp, Color(..), 
      
      withRepline,
      readline,
      setPromptColor,

      setHistory,
      historyClear,
      historyRemoveLast,

      RpComp,
      setCompleter,
      addCompletion,
      completeFileName,
      completeWord,
      completeQuotedWord,

      initialize, done    
    ) where

import Data.List( intersperse )
import Control.Exception( bracket )
import Foreign.C.String( CString, peekCString, withCString, castCharToCChar )
import Foreign.Ptr
import Foreign.C.Types



data RpEnv
data RpCompEnv  

newtype Rp     = Rp (Ptr RpEnv)
newtype RpComp = RpComp (Ptr RpCompEnv)

type CCompleterFun = Ptr RpCompEnv -> CString -> IO ()
type CompleterFun  = RpComp -> String -> IO ()

foreign import ccall rp_init      :: IO (Ptr RpEnv)
foreign import ccall rp_done      :: Ptr RpEnv -> IO ()
foreign import ccall rp_readline  :: Ptr RpEnv -> CString -> IO CString
foreign import ccall rp_free      :: Ptr RpEnv -> (Ptr a) -> IO () 
foreign import ccall rp_set_prompt_color      :: Ptr RpEnv -> CInt -> IO ()
foreign import ccall rp_set_history           :: Ptr RpEnv -> CString -> CInt -> IO ()
foreign import ccall rp_history_remove_last   :: Ptr RpEnv -> IO ()
foreign import ccall rp_history_clear         :: Ptr RpEnv -> IO ()

foreign import ccall rp_set_completer         :: Ptr RpEnv -> FunPtr CCompleterFun -> IO ()
foreign import ccall "wrapper" rp_make_completer :: CCompleterFun -> IO (FunPtr CCompleterFun)
foreign import ccall rp_add_completion        :: Ptr RpCompEnv -> CString -> CString -> IO CChar
foreign import ccall rp_complete_filename     :: Ptr RpCompEnv -> CString -> CChar -> CString -> IO ()
foreign import ccall rp_complete_word         :: Ptr RpCompEnv -> CString -> FunPtr CCompleterFun -> IO ()
foreign import ccall rp_complete_quoted_word  :: Ptr RpCompEnv -> CString -> FunPtr CCompleterFun -> CString -> CChar -> CString -> IO ()


withCString0 :: String -> (CString -> IO a) -> IO a
withCString0 s action
  = if (null s) then action nullPtr else withCString s action

setCompleter :: Rp -> (RpComp -> String -> IO ()) -> IO ()
setCompleter (Rp rp) completer 
  = do ccompleter <- makeCCompleter completer
       rp_set_completer rp ccompleter

makeCCompleter :: CompleterFun -> IO (FunPtr CCompleterFun)
makeCCompleter completer
  = rp_make_completer wrapper
  where
    wrapper :: Ptr RpCompEnv -> CString -> IO ()
    wrapper rpcomp cprefx
      = do prefx <- peekCString cprefx
           completer (RpComp rpcomp) prefx


addCompletion :: RpComp -> String -> String -> IO Bool
addCompletion (RpComp rpc) display completion 
  = withCString0 display $ \cdisplay ->
    withCString completion $ \ccompletion ->
    do cbool <- rp_add_completion rpc cdisplay ccompletion
       return (fromEnum cbool /= 0)
    

completeFileName :: RpComp -> String -> Maybe Char -> [FilePath] -> IO ()
completeFileName (RpComp rpc) prefx dirSep roots
  = withCString prefx $ \cprefx ->
    withCString0 (concat (intersperse ";" roots)) $ \croots ->
    do let cdirSep = case dirSep of
                       Nothing -> toEnum 0
                       Just c  -> castCharToCChar c
       rp_complete_filename rpc cprefx cdirSep croots

completeWord :: RpComp -> String -> (RpComp -> String -> IO ()) -> IO () 
completeWord (RpComp rpc) prefx completer
  = withCString prefx $ \cprefx ->
    do ccompleter <- makeCCompleter completer
       rp_complete_word rpc cprefx ccompleter
  
completeQuotedWord :: RpComp -> String -> (RpComp -> String -> IO ()) -> String -> Maybe Char -> String -> IO () 
completeQuotedWord (RpComp rpc) prefx completer nonWordChars escapeChar quoteChars
  = withCString prefx $ \cprefx ->
    withCString0 nonWordChars $ \cnonWordChars ->
    withCString0 quoteChars $ \cquoteChars ->
    do let cescapeChar = case escapeChar of
                          Nothing -> toEnum 0
                          Just c  -> castCharToCChar c
       ccompleter <- makeCCompleter completer
       rp_complete_quoted_word rpc cprefx ccompleter cnonWordChars cescapeChar cquoteChars
  

initialize :: IO Rp
initialize 
  = fmap Rp rp_init

done :: Rp -> IO ()
done (Rp rpenv) 
  = rp_done rpenv

readline :: Rp -> String -> IO String  
readline (Rp rpenv) prompt
  = withCString prompt $ \cprompt ->
    do cres <- rp_readline rpenv cprompt
       res  <- peekCString cres
       rp_free rpenv cres
       return res

setPromptColor :: Rp -> Color -> IO ()
setPromptColor (Rp rp) color
  = rp_set_prompt_color rp (toEnum (fromEnum color))


setHistory :: Rp -> String -> Int -> IO ()
setHistory (Rp rp) fname maxEntries
  = withCString0 fname $ \cfname ->
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
