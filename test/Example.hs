import System.Console.Repline
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Control.Monad( when )

main :: IO ()
main
  = do putStrLn welcome
       setPromptColor Green
       setHistory "history.txt" 200
       setDefaultCompleter completer
       interaction
  where
    welcome = "\nHaskell Repline sample program:\n" ++
              "- Type 'exit' to quit. (or use ctrl+d).\n" ++
              "- Press F1 for help on editing commands.\n" ++
              "- Use ctrl+enter for multiline input (or alt-enter on macOS).\n" ++
              "- Type 'id' (or 'ex', 'f', or 'h') followed by tab for completion.\n" ++
              "- Use ctrl+r to search the history.\n" ++
              "\n"

interaction :: IO ()
interaction 
  = do s <- readline "hαskell"
       putStrLn $ unlines ["--------",s,"--------"]
       if (s == "" || s == "exit") 
         then return ()
         else interaction
       
       
completer :: Completions -> String -> IO () 
completer compl input
  = do completeFileName compl input Nothing [".","/usr/local"]
       completeWord compl input wordCompleter
  
wordCompleter :: Completions-> String -> IO ()   
wordCompleter compl input0
  = do let input = map toLower input0
       when (input `isPrefixOf` "hello repline") $
         do helloCompletions 1 100000
       when (input == "id") $
         do addCompletion compl "D — (x) => x"       "d"                
            addCompletion compl "Haskell — \\x -> x" "haskell"
            addCompletion compl "Idris — \\x => x"   "idris"
            addCompletion compl "Koka — fn(x){ x }"  "koka"    
            addCompletion compl "Ocaml — fun x -> x" "ocaml"
            return ()
  where
    helloCompletions :: Int -> Int -> IO ()
    helloCompletions i max 
      = if (i >= max) 
          then return () 
          else do continue <- addCompletion compl "" ("hello repline " ++ show i)
                  helloCompletions (if continue then (i+1) else max) max
