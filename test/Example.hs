{- ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
---------------------------------------------------------------------------- -}
import System.Console.Repline
import Data.List (isPrefixOf)
import Data.Char 
import Control.Monad( when )

main :: IO ()
main
  = do termWriteLn welcome                 -- like putStrLn but handles escape sequences portably
       setPromptColor Green                -- custom prompt color
       setHistory "history.txt" 200        -- history
       setHighlighterAttr highlighter      -- syntax highlighting
       enableAutoTab True                  -- complete as far as possible
       interaction
  where
    welcome = "\n\x1B[33mHaskell Repline sample program:\x1B[0m\n" ++
              "- Type 'exit' to quit. (or use ctrl+d).\n" ++
              "- Press F1 for help on editing commands.\n" ++
              "- Use ctrl+enter for multiline input (or alt-enter on macOS).\n" ++
              "- Type 'id' (or 'ex', 'f', or 'h') followed by tab for completion.\n" ++
              "- Use ctrl+r to search the history.\n" ++
              "\n"

interaction :: IO ()
interaction 
  = do s <- readlineWithCompleter "hαskell" completer   -- or use `readline` without completion
       putStrLn $ unlines ["--------",s,"--------"]
       if (s == "" || s == "exit") 
         then return ()
         else interaction


----------------------------------------------------------------------------
-- Tab Completion
----------------------------------------------------------------------------       

completer :: Completions -> String -> IO () 
completer compl input
  = do completeFileName compl input Nothing [".","/usr/local"] [] {-any extension-}
       completeWord compl input wordCompleter
  
wordCompleter :: Completions-> String -> IO ()   
wordCompleter compl input0
  = do -- simple completion based on available words
       let input = map toLower input0
       addCompletionsFor compl input ["print","printer","println","printsln","prompt"]
       -- add many hello repline completions
       when (not (null input) && input `isPrefixOf` "hello repline ") $
         do helloCompletions 1 100000
       -- display versus replacement
       when (input == "id") $
         do addCompletion compl "D — (x) => x"       "(x) => x"                
            addCompletion compl "Haskell — \\x -> x" "\\x -> x"
            addCompletion compl "Idris — \\x => x"   "\\x => x"
            addCompletion compl "Koka — fn(x){ x }"  "fn(x){ x }"    
            addCompletion compl "Ocaml — fun x -> x" "fun x -> x"
            return ()
  where
    helloCompletions :: Int -> Int -> IO ()
    helloCompletions i max 
      = if (i >= max) 
          then return () 
          else do continue <- addCompletion compl "" ("hello repline " ++ show i)
                  helloCompletions (if continue then (i+1) else max) max


----------------------------------------------------------------------------
-- Syntax highlighting
-- uses a simple tokenizer but a full fledged one probably needs 
-- Parsec or regex's for syntax highlighting
----------------------------------------------------------------------------       

highlighter :: String -> [TextAttr]
highlighter input
  = tokenize input
  where
    tokenize [] = []
    tokenize s@('/':'/':_) 
      = let (t,ds) = span (/='\n') s in withAttrColor Green t ++ tokenize ds
    tokenize s@(c:cs)
      | isAlpha c   = let (t,ds) = span isAlpha s
                      in (if (t `elem` ["fun","return","if","then","else"]) 
                             then withAttrColor Yellow t 
                           else if (t `elem` ["int","double","char","void"])
                             then withAttrColor Cyan t 
                             else withAttrDefault t)
                         ++ tokenize ds
      | isDigit c   = let (t,ds) = span isDigit s 
                      in withAttrColor White t ++ tokenize ds
      | otherwise   = withAttrDefault [c] ++ tokenize cs



