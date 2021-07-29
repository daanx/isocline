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
       enableAutoTab True                  -- complete as far as possible
       interaction
  where
    welcome = "\n\x1B[33mHaskell Repline sample program:\x1B[0m\n" ++
              "- Type 'exit' to quit. (or use ctrl+d).\n" ++
              "- Press F1 for help on editing commands.\n" ++
              "- Use shift+tab for multiline input (or ctrl-enter).\n" ++
              "- Type 'p' (or 'id', 'f', or 'h') followed by tab for completion.\n" ++
              "- Type 'fun' or 'int' to see syntax highlighting\n" ++
              "- Use ctrl+r to search the history.\n" ++
              "\n"

interaction :: IO ()
interaction 
  = do s <- readlineEx "hαskell" (Just completer) (Just highlighter)
       putStrLn $ unlines ["--------",s,"--------"]
       if (s == "" || s == "exit") 
         then return ()
         else interaction


----------------------------------------------------------------------------
-- Tab Completion
----------------------------------------------------------------------------       

completer :: CompletionEnv -> String -> IO () 
completer compl input
  = do completeFileName compl input Nothing [".","/usr/local"] [] {-any extension-}
       completeWord compl input wordCompletions
  
wordCompletions :: String -> [Completion]
wordCompletions input0
  = let input = map toLower input0
    in -- simple completion based on available words
       (completionsFor input ["print","printer","println","printsln","prompt"])
       ++
       -- with display versus replacement
       (if (input == "id") 
         then map (\(d,r) -> completionWithDisplay d r) $ 
              [ ("D — (x) => x",       "(x) => x")
              , ("Haskell — \\x -> x", "\\x -> x")
              , ("Idris — \\x => x",   "\\x => x")
              , ("Koka — fn(x){ x }",  "fn(x){ x }")
              , ("Ocaml — fun x -> x", "fun x -> x")]
         else []) ++
       -- add many hello repline completions; we should generate these lazily!
       (if (not (null input) && input `isPrefixOf` "hello repline ") 
         then map (\i -> completion ("hello repline " ++ show i)) [1..100000]
         else [])
  

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
                      in withAttrColor Blue t ++ tokenize ds
      | otherwise   = withAttrDefault [c] ++ tokenize cs

