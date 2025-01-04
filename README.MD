# Gatds

#### Gadts | a command line calculator
 
#### This calculator supports
>  - sin 
>  - cosine 
>  - tangent 
> - sqrt 
>  - (+) addition
>  - (-) subtraction
>  - (- x) negative numbers as prefix notation
>  -  exponenation (^) operator
>  - (/) division 
>  - (help) a useless help command that displays bad helping output
>> ```haskell
    showHelpMenu :: IO ()
    showHelpMenu = do 
        putStrLn "! to quit or type exit"
        putStrLn "help for this menu"
        putStrLn "Normal mode is to enter mathematical expressions"
   ```
>  - and a (!) or (exit) command which stops the program.
>
#### Example of what you will see when running the program as of (1/4/2025)
   ```text 
   =Calculator=
   :22 + 21
   Expression = (22.0) + (21.0)
   Evaluation = 43.0
   :(29 / 24) + 21
   Expression = ((29.0) / (24.0)) + (21.0)
   Evaluation = 22.208333333333332
   :!
   Thank you for using this calculator
   ```
