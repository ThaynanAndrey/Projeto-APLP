--necessita instalar Data.Matrix, Data.List.Split
import Data.Matrix
import Control.Monad (unless)
import Data.List.Split
import Data.Char

data Posicao = Posicao { cor :: Int
,x :: Int, y :: Int, pilhaDeBolinhas :: Int
} deriving Show


data Teste = Teste {a :: Int} deriving Show


--Mostra o menu do programa
mostrarMenu :: IO ()
mostrarMenu = do
 putStrLn ("#######################################")
 putStrLn ("#######################################")
 putStrLn ("##########  Chain Reaction   ##########")
 putStrLn ("#######################################")
 putStrLn ("#######################################")
 putStrLn ("------------ Menu do Jogo ------------")
 putStrLn ("1) Começar o jogo")
 putStrLn ("2) Sair")
 putStrLn ("Digite o número de sua opção: ")

prompt :: IO ()
prompt = do
    opcao <- getLine
    unless (opcao == "2") $ do
        putStrLn $ "You entered: " ++ opcao
        configurarTamanhoTabuleiro
        prompt 

convertStringInInt :: [String] -> [Int]
convertStringInInt = map read

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do 
  print $  a (getElem 1 n action)
  repeatNTimes (n-1) action


configurarTamanhoTabuleiro :: IO()
configurarTamanhoTabuleiro = do
    putStrLn ("Defina o tamanho para o tabuleiro(máximo: 26x26): (Ex: 10 10) ")
    formato <- getLine
    let coord = splitOn " " formato
    let length = do convertStringInInt coord
    let building = do product length
    let list_all = replicate building Teste {a="1"}
        m1 = fromList (head length) (last length) list_all
    repeatNTimes (head length) m1
    -- print list

main :: IO ()
main = do
    mostrarMenu
    prompt
    -- let teste = [T {a=1}, T {a=2}]
    -- let m2 = fromList 2 1 teste
    -- print $ setElem T {a=3} (1, 1) m2
    -- -- print $ getRow 2 m2
    -- -- let numbersList = [] 
    -- -- let opcao = read opcaoInput :: Int
    -- print teste1
    -- converter valores para inteiro
    -- read "34"::Int