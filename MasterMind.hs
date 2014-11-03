module Main where

import  Data.List      
import  System.Random   

{-Verifica se não possue os numeros 9,8,0 na lista-}
oneToSeven [] = True
oneToSeven (a:x)
	|a == '0' = False
	|a == '8' = False
	|a == '9' = False
	|otherwise = oneToSeven x

{-Verifica se tem duas cores/numeros repetidas e chama a função oneToSeven-}
noRepeat attempt  
	|attempt == nub attempt = oneToSeven attempt
	|otherwise = False

{-Verifica se a tentativa tem 4 digitos e chama a função noRepeat-}
valid attempt  
	| length attempt == 4 = noRepeat attempt
	| otherwise = False

{-Cria uma senha randomica de 4 digitos e sem repetição-}
randPsw :: IO Integer
randPsw = do
	psw <- randomRIO (1000, 9876)
	if noRepeat (show (psw)) == False 
		then randPsw
	else return psw

{-Verifica se a tentativa está correta-}
isEqual attempt psw = attempt == psw

{-Avalia tentativa, "dica"-}
compareAttempt [] _ _ _ _ = []
compareAttempt (a:x) tattempt [] tpsw psw = "v"++(compareAttempt x (tattempt-1) psw 4 psw)
compareAttempt (a:x) tattempt (b:y) tpsw psw
	| a == b && tattempt == tpsw = "p"++(compareAttempt x (tattempt-1) psw 4 psw)
	| a == b && not (tattempt == tpsw) = "b"++(compareAttempt x (tattempt-1) psw 4 psw)
	| otherwise = compareAttempt (a:x) tattempt y (tpsw-1) psw

{-Embaralha a dica-}
shuffleAux [] shuffled aux = shuffled
shuffleAux (a:x) shuffled aux
	| aux == 1 = shuffleAux x (a:shuffled) 0
	| otherwise = shuffleAux x (shuffled ++ [a]) 1

shuffle [] = []
shuffle ans = shuffleAux ans [] 1

{-Conta quantas vezes o usuario realizou as tentativas e trata to o fluxo de acerto ou erro-}
attempts :: (Num i, Eq i, Show i) => i -> String -> IO ()
attempts 0 _ = putStrLn "Game Over... You Lose!"
attempts i psw = do
	putStrLn $ "Entre com o seu palpite ("++ (show i) ++"): (Os digitos do seu palpite devem ser unicos e estar entre 1000 e 9876)"
	attempt <- getLine
	if valid attempt then
		if isEqual attempt psw
			then putStrLn $ (show attempt) ++ "\nParabens você ganho!"
		else do
			putStrLn $ (show attempt) ++ "\tSeu palpite gero a seguinte dica -> " ++ shuffle (compareAttempt attempt 4 psw 4 psw) ++ "\""
			attempts (i-1) psw
	else do
		putStrLn $ (show attempt) ++ "- Palpite não é valido!"
		attempts (i-1) psw


{-Cria numero randomico e realiza a primeira chamada das tentativas-}
main = do
	putStrLn "Gerando senha..."
	psw <- randPsw
	putStrLn "Iniciando jogo..."
	attempts 10 (show psw)

			

