import System.Console.Repline
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Control.Monad( when )

main :: IO ()
main
  = withRepline $ \rp ->
    do setPromptColor rp Green
       setHistory rp "history.txt" 200
       setCompleter rp completer
       interaction rp

interaction :: Rp -> IO ()
interaction rp
  = do s <- readline rp "haskell"
       putStrLn $ unlines ["--------",s,"--------"]
       if (s == "" || s == "exit") 
         then return ()
         else interaction rp
       
       
completer :: RpComp -> String -> IO () 
completer rpc input
  = do completeFileName rpc input Nothing [".","/usr/local"]
       completeWord rpc input wordCompleter
  
wordCompleter :: RpComp -> String -> IO ()   
wordCompleter rpc input0
  = do let input = map toLower input0
       when (input `isPrefixOf` "hello repline") $
         do helloCompletions 1 100000
       when (input == "id") $
         do addCompletion rpc "D — (x) => x" "d"                
            addCompletion rpc "Haskell — \\x -> x" "haskell"
            addCompletion rpc "Idris — \\x => x" "idris"
            addCompletion rpc "Koka — fn(x){ x }" "koka"    
            addCompletion rpc "Ocaml — fun x -> x" "ocaml"
            return ()
  where
    helloCompletions :: Int -> Int -> IO ()
    helloCompletions i max 
      = if (i >= max) then return () else
        do continue <- addCompletion rpc "" ("hello repline " ++ show i)
           if continue then helloCompletions (i+1) max
                       else return ()