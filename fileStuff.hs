main = do
{
    putStrLn "Hello World!" ;
    content <- getLine ;	
    putStrLn $ "stuff: " ++ content 
}