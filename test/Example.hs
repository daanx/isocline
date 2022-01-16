{- ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
---------------------------------------------------------------------------- -}

import System.Console.Isocline
import Data.List (isPrefixOf)
import Data.Char 
import Control.Monad( when )

main :: IO ()
main
  = do styleDef "kbd" "gray underline"     -- define a style
       styleDef "ic-prompt" "#00A060"      -- or redefine a system style
       putFmtLn welcome                 
       setHistory "history.txt" 200        -- history
       enableAutoTab True                  -- complete as far as possible
       interaction
  where
    welcome = "\n[b]Isocline[/b] sample program:\n" ++
              "- Type 'exit' to quit. (or use [kbd]ctrl-d[/]).\n" ++
              "- Press [kbd]F1[/] for help on editing commands.\n" ++
              "- Use [kbd]shift-tab[/] for multiline input. (or [kbd]ctrl-enter[/], or [kbd]ctrl-j[/])\n" ++
              "- Type 'p' (or 'id', 'f', or 'h') followed by tab for completion.\n" ++
              "- Type 'fun' or 'int' to see syntax highlighting\n" ++
              "- Use [kbd]ctrl-r[/] to search the history.\n"

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
       completeWord compl input Nothing wordCompletions
  
wordCompletions :: String -> [Completion]
wordCompletions input0
  = let input = map toLower input0
    in -- simple completion based on available words
       (completionsFor input ["print","printer","println","printsln","prompt"])
       ++
       -- with display versus replacement
       (if (input == "id") 
         then map (\(d,r) -> Completion r d "") $    -- Completion replacement display help
              [ ("D — (x) => x",       "(x) => x")
              , ("Haskell — \\x -> x", "\\x -> x")
              , ("Idris — \\x => x",   "\\x => x")
              , ("Ocaml — fun x -> x", "fun x -> x")
              , ("Koka — fn(x) x",  "fn(x) x")
              , ("Rust — |x| x", "|x| x") ]
         else []) 
       ++
       -- add many hello isocline completions; we should generate these lazily!
       (if (not (null input) && input `isPrefixOf` "hello_isocline_") 
         then map (\i -> completion ("hello_isocline_" ++ show i)) [1..100000]
         else [])
  

----------------------------------------------------------------------------
-- Syntax highlighting
-- uses a simple tokenizer but a full fledged one probably needs 
-- Parsec or regex's for syntax highlighting
----------------------------------------------------------------------------       

highlighter :: String -> Fmt
highlighter input
  = tokenize input
  where
    tokenize [] = []
    tokenize s@('/':'/':_)  -- comment    
      = let (t,ds) = span (/='\n') s in style "#408700" (plain t) ++ tokenize ds
    tokenize s@(c:cs)
      | isAlpha c   = let (t,ds) = span isAlpha s
                      in (if (t `elem` ["fun","struct","var","val"]) 
                            then style "keyword" t   -- builtin style
                          else if (t `elem` ["return","if","then","else"]) 
                            then style "control" t   -- builtin style
                          else if (t `elem` ["int","double","char","void"])
                            then style "#00AFAF" t   -- or use specific colors
                            else plain t)            -- never lose input, all original characters must be present!
                         ++ tokenize ds
      | isDigit c   = let (t,ds) = span isDigit s 
                      in style "number" t ++ tokenize ds
      | otherwise   = plain [c] ++ tokenize cs      -- never lose input

