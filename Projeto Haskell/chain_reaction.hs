import Data.Char
import Control.Concurrent

-- Utilizado para as guias do tabuleiro, na qual cada letra
-- representara a coluna do tabuleiro.
guiaColuna = "ABCDEFGHIJKLMNOPQRSTUVXYWZ"

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

-- Formata a linha do tabuleiro em espaços a partir da lista passada.
formatarLinhaTabuleiro :: [String] -> Int -> String
formatarLinhaTabuleiro [] _ = ""
formatarLinhaTabuleiro (a:xs) cont = do 
    if(cont == 0) then do
        a ++ formatarLinhaTabuleiro xs (cont+1)
    else do
        " " ++ a ++ formatarLinhaTabuleiro xs (cont+1)

-- Formata o valor da posicao do tabuleiro, seguindo a ideia:
--    Se o valor na posicao for N, entao a posicao eh impressa com valor vazio.
--    Se não, o valor eh impresso com a letra do jogador e a quantidade de bolinhas
--    naquela posicao.
formatarValorPosicao :: String -> String -> String
formatarValorPosicao a b
    | a == "N" = "  "
    | otherwise = a ++ b

-- Formata cada linha do jogo para apresentacao ao Usuario.
obterLinhaFormatada :: [(String, Int)] -> String
obterLinhaFormatada linha = do
    let linhaFormatada = [ (formatarValorPosicao a (show b)) | (a, b) <- linha]  
    formatarLinhaTabuleiro linhaFormatada 0

-- Realiza a insercao de espacamento de acordo com a numeracao da linha,
-- mantendo-se o alinhamento do tabuleiro.
inserirEspacamento :: Int -> String
inserirEspacamento cont | cont < 10 = " | "
                        | otherwise = "| "

guiasColuna :: [(String, Int)] -> Int -> String
guiasColuna [] _ = ""
guiasColuna (a:xs) cont = " " ++ [guiaColuna !! cont] ++ " " ++ (guiasColuna xs (cont + 1))

-- Realiza a impressao do tabuleiro.
imprimirTauleiro :: [[(String, Int)]] -> Int -> IO()
imprimirTauleiro [] _ = putStrLn("")
imprimirTauleiro (a:xs) cont = do
    let linhaFormatada = (show cont) ++ (inserirEspacamento cont) ++ obterLinhaFormatada a
    if(cont == 1) then do
        print ("  |" ++ guiasColuna a 0)
        print linhaFormatada
    else do
        print linhaFormatada
    imprimirTauleiro (xs) (cont+1)

-- Cria um tabuleiro novo
-- Uma lista de listas para representar a matriz.
-- Uma dupla para representar cada posicao da matriz,
-- considerando (a, b), tal que a eh o numero do jogador
-- e b eh a qtd de bolinhas naquela posicao.
criarTabuleiro :: Int -> Int -> [[(String, Int)]]
criarTabuleiro qtd_linhas qtd_colunas =
    [ [ ("N", 0) | y <- [1 .. qtd_colunas] ] | x <- [1.. qtd_linhas]  ]

-- Configurar o tamanho do Tabuleiro, de acordo com as opcao do Usuario
configurarTamanhoTabuleiro :: IO()
configurarTamanhoTabuleiro = do
    putStrLn ("Defina a quantidade de linhas do tabuleiro (máximo: 26)")
    qtd_linhas_char <- getLine
    let qtd_linhas = (read qtd_linhas_char :: Int)
    putStrLn ("Defina a quantidade de colunas do tabuleiro (máximo: 26)")
    qtd_colunas_char <- getLine
    let qtd_colunas = (read qtd_colunas_char :: Int)
    let meuTabuleiro = criarTabuleiro qtd_linhas qtd_colunas
    jogar meuTabuleiro "A" 1

-- Recebe a entrada do Usario para delinear a dinamica do jogo.
iniciarJogo :: IO ()
iniciarJogo = do
    opcao <- getLine
    if (opcao == "1") then do
        configurarTamanhoTabuleiro
    else do 
        if(opcao == "2") then do
            putStrLn $ "Saindo..."
        else do
            putStrLn $ "Voce digitou uma opacaoinvalida. Tente Novamente..."
            iniciarJogo

-- Funcao auxiliar de inserirNoTabuleiro que permite inserir na posicao correta.
inserirNaPosicao :: [(String, Int)] -> String -> Int -> Int -> [(String, Int)]
inserirNaPosicao [] _ _ _ = []
inserirNaPosicao ((a, b):xs) jogadorDaVez coluna coluna_cont
    | coluna == coluna_cont = [(jogadorDaVez, (b+1))] ++ (inserirNaPosicao xs jogadorDaVez coluna (coluna_cont+1))
    | otherwise = [(a, b)] ++ (inserirNaPosicao xs jogadorDaVez coluna (coluna_cont+1))

-- Insere na posicao estabelecida pelo usuario
inserirNoTabuleiro :: [[(String, Int)]] -> String -> Int -> Int -> Int -> [[(String, Int)]]
inserirNoTabuleiro [] _ _ _ _ = []
inserirNoTabuleiro (a:xs) jogadorDaVez linha linha_cont coluna
    | linha_cont == linha = [(inserirNaPosicao a jogadorDaVez coluna 1)] ++ (inserirNoTabuleiro xs jogadorDaVez linha (linha_cont+1) coluna)
    | otherwise = [a] ++ (inserirNoTabuleiro xs jogadorDaVez linha (linha_cont+1) coluna)

--Funcao auxiliar verifica a possibilidade de jogada na posicao
verificaPossibilidadeDeJogoPosicao :: [(String, Int)]-> Int -> Int -> Int -> String -> Bool
verificaPossibilidadeDeJogoPosicao [] _ _ _ _ = False
verificaPossibilidadeDeJogoPosicao (x:xs) linha coluna coluna_cont jogadorDaVez
    | coluna_cont == coluna = (fst x == jogadorDaVez) || (fst x == "N")
    | otherwise = verificaPossibilidadeDeJogoPosicao xs linha coluna (coluna_cont+1) jogadorDaVez

--Funcao verifica a possibilidade de jogada
verificaPossibilidadeDeJogo :: [[(String, Int)]] -> Int -> Int -> Int -> String -> Bool
verificaPossibilidadeDeJogo [] _ _ _ _ = False
verificaPossibilidadeDeJogo (x:xs) linha coluna linha_cont jogadorDaVez
    | linha_cont == linha = verificaPossibilidadeDeJogoPosicao x linha coluna 1 jogadorDaVez
    | otherwise = verificaPossibilidadeDeJogo xs linha coluna (linha_cont+1) jogadorDaVez

-- Funcao auxiliar de resetarNoTabuleiro que permite resetar os valores na posicao correta.
resetarNaPosicao :: [(String, Int)] -> Int -> Int -> [(String, Int)]
resetarNaPosicao [] _ _ = []
resetarNaPosicao ((a, b):xs) coluna coluna_cont
    | coluna == coluna_cont = [("N", 0)] ++ (resetarNaPosicao xs coluna (coluna_cont+1))
    | otherwise = [(a, b)] ++ (resetarNaPosicao xs coluna (coluna_cont+1))

-- Reseta os valores na posicao estabelecida pelo usuario
resetarNoTabuleiro :: [[(String, Int)]]  -> Int -> Int -> Int -> [[(String, Int)]]
resetarNoTabuleiro [] _ _ _ = []
resetarNoTabuleiro (a:xs) linha linha_cont coluna
    | linha_cont == linha = [(resetarNaPosicao a coluna 1)] ++ (resetarNoTabuleiro xs linha (linha_cont+1) coluna)
    | otherwise = [a] ++ (resetarNoTabuleiro xs linha (linha_cont+1) coluna)

-- Realiza a jogada do Usuario.
realizarJogada :: [[(String, Int)]] -> String -> Int -> Int -> [[(String, Int)]]
realizarJogada tabuleiro jogadorDaVez linha coluna = do
    inserirNoTabuleiro tabuleiro jogadorDaVez linha 1 coluna

--Funcao responsável por limpar a tela do terminal
limparTela :: IO()
limparTela = putStr "\ESC[2J"

--Funcao que imprime a resolucao do tabuleiro 
imprimeResolucaoTabuleiro :: [[(String,Int)]] -> String -> [(Int, Int )]-> IO()
imprimeResolucaoTabuleiro tabuleiro jogadorDaVez [] = do
    limparTela
    imprimirTauleiro tabuleiro 1
imprimeResolucaoTabuleiro tabuleiro jogadorDaVez ((a, b):xs) = do
    if(podeExplodirLinha tabuleiro a b 1) then do
        let vizinhos = pegaVizinhos tabuleiro a b 1
        limparTela
        imprimirTauleiro tabuleiro 1
        threadDelay 1000000 --dorme por 1 segundo
        if (checaVencedor tabuleiro jogadorDaVez 1) then do
            putStr("Parabéns, jogador " ++ jogadorDaVez ++ ", Você venceu!")
        else do
            let tabuleiroInserido = inserirBolasNosVizinhos tabuleiro jogadorDaVez vizinhos
            let tabuleiroResolvido = resetarNoTabuleiro tabuleiroInserido a 1 b
            imprimeResolucaoTabuleiro tabuleiroResolvido jogadorDaVez (xs ++ vizinhos)
    else do
        imprimeResolucaoTabuleiro tabuleiro jogadorDaVez xs

--Funcao que realiza o chain reaction
resolverTabuleiro :: [[(String,Int)]] -> String -> [(Int, Int )]-> [[(String, Int)]]
resolverTabuleiro tabuleiro jogadorDaVez [] = tabuleiro
resolverTabuleiro tabuleiro jogadorDaVez ((a, b):xs) = do
    if(podeExplodirLinha tabuleiro a b 1) then do
        let vizinhos = pegaVizinhos tabuleiro a b 1
        let tabuleiroInserido = inserirBolasNosVizinhos tabuleiro jogadorDaVez vizinhos
        let tabuleiroResolvido = resetarNoTabuleiro tabuleiroInserido a 1 b
        resolverTabuleiro tabuleiroResolvido jogadorDaVez (xs ++ vizinhos)
    else do
        resolverTabuleiro tabuleiro jogadorDaVez xs

--Funcao que insere bolas na lista de vizinhos passadas como parametro
inserirBolasNosVizinhos ::  [[(String,Int)]] -> String -> [(Int, Int )]-> [[(String, Int)]]
inserirBolasNosVizinhos tabuleiro _ [] = tabuleiro
inserirBolasNosVizinhos tabuleiro jogadorDaVez ((a,b):xs) = inserirBolasNosVizinhos (inserirNoTabuleiro tabuleiro jogadorDaVez a 1 b) jogadorDaVez xs

--Funcao retorna os vizinhos de uma certa coordenada
pegaVizinhos :: [[(String, Int)]] -> Int -> Int -> Int -> [(Int, Int)]
pegaVizinhos (x:xs) linha coluna linha_cont
    | linha_cont == linha = pegaVizinhosNaPosicao x linha coluna 1 xs
    | otherwise = pegaVizinhos xs linha coluna (linha_cont+1)


--Funcao auxiliar pega os vizinhos de uma certa coordenada
pegaVizinhosNaPosicao :: [(String, Int)]-> Int -> Int -> Int -> [[(String,Int)]] -> [(Int, Int)]
pegaVizinhosNaPosicao (x:xs) linha coluna coluna_cont proxima_linha
    | coluna_cont == coluna = retornaVizinhos proxima_linha xs linha coluna
    | otherwise = pegaVizinhosNaPosicao xs linha coluna (coluna_cont+1) proxima_linha


--Funcao auxiliar retorna os vizinhos de uma certa coordenada
retornaVizinhos :: [[(String,Int)]] -> [(String,Int)] -> Int -> Int -> [(Int,Int)]
retornaVizinhos [] [] linha coluna  = [(linha-1,coluna), (linha,coluna-1)]
retornaVizinhos _ _ 1 1  = [(2,1), (1,2)]
retornaVizinhos [] _ linha 1  = [(linha-1, 1), (linha, 2)]
retornaVizinhos _ [] 1 coluna  = [(1, coluna-1),(2,coluna)]
retornaVizinhos [] _ linha coluna = [(linha-1,coluna),(linha,coluna-1),(linha,coluna+1)]
retornaVizinhos _ [] linha coluna = [(linha-1,coluna),(linha+1,coluna),(linha,coluna-1)]
retornaVizinhos _ _ 1 coluna = [(1,coluna-1),(1,coluna+1),(2, coluna)]
retornaVizinhos _ _ linha 1 = [(linha-1, 1),(linha+1,1),(linha,2)]
retornaVizinhos proxima_linha proxima_coluna linha coluna = [(linha+1,coluna),(linha-1, coluna),(linha, coluna+1),(linha, coluna-1)]

-- Verifica se posicao pode explodir iterando pelas linhas
podeExplodirLinha :: [[(String, Int)]] -> Int -> Int -> Int -> Bool
podeExplodirLinha [] _ _ _ = False
podeExplodirLinha (x:xs) linha coluna linha_cont
    | linha_cont == linha = podeExplodirColuna x coluna 1 xs linha_cont
    | otherwise = podeExplodirLinha xs linha coluna (linha_cont+1)

--Funcao auxiliar que verifica se posicao pode explodir iterando pelas colunas
podeExplodirColuna :: [(String, Int)] -> Int -> Int ->  [[(String, Int)]] -> Int-> Bool
podeExplodirColuna [] _ _ _ _ = False
podeExplodirColuna ((a,b):xs) coluna coluna_cont proxima_linha linha_cont
    | coluna_cont == coluna = checaPosicaoDeExplosao proxima_linha xs linha_cont coluna_cont b
    | otherwise = podeExplodirColuna xs coluna (coluna_cont+1) proxima_linha linha_cont

--Funcao auxiliar que checa se a posição explode
checaPosicaoDeExplosao :: [[(String, Int)]] -> [(String,Int)] -> Int -> Int -> Int -> Bool
checaPosicaoDeExplosao [] [] _ _ qtdBolinhas = (qtdBolinhas >= 2)
checaPosicaoDeExplosao _ _ 1 1 qtdBolinhas =(qtdBolinhas >= 2)
checaPosicaoDeExplosao [] _ _ 1 qtdBolinhas = (qtdBolinhas >= 2)
checaPosicaoDeExplosao _ [] 1 _ qtdBolinhas =(qtdBolinhas >= 2)
checaPosicaoDeExplosao [] _ _ _ qtdBolinhas =(qtdBolinhas >= 3)
checaPosicaoDeExplosao _ [] _ _ qtdBolinhas =(qtdBolinhas >= 3)
checaPosicaoDeExplosao _ _ 1 _ qtdBolinhas =(qtdBolinhas >= 3)
checaPosicaoDeExplosao _ _ _ 1 qtdBolinhas =(qtdBolinhas >= 3)
checaPosicaoDeExplosao proxima_linha proxima_coluna linha_cont coluna_cont qtdBolinhas = (qtdBolinhas >= 4)

-- Inverte o jogador da vez.
inverterJogador :: String -> String
inverterJogador jogadorDaVez
    | jogadorDaVez == "A" = "B"
    | otherwise = "A"

-- Funcao para checar se o jogador é vencedor
checaVencedor :: [[(String,Int)]] -> String -> Int -> Bool
checaVencedor [] jogador contador = True
checaVencedor (x:xs) jogador contador = do
    if(checaPosicao x jogador 1) then do
        checaVencedor xs jogador (contador+1)
    else 
        False

--Funcao auxliar de checaVencedor para checar cada posicao da matriz
checaPosicao :: [(String, Int)] -> String -> Int -> Bool
checaPosicao [] jogador contador = True
checaPosicao ((a,b):xs) jogador contador = do
    if (a /= jogador && a /="N") then
        False
    else do
        checaPosicao xs jogador (contador+1)

-- Obtem a posicao da coluna a partir do valor passado pelo usuario.
posicaoColuna :: String -> String -> Int -> Int
posicaoColuna [] _ _ = -1
posicaoColuna (a:xs) valor cont
    | [a] == valor = cont
    | otherwise = posicaoColuna xs valor (cont+1)

-- Funcao responsavel pelo controle de jogo
jogar :: [[(String, Int)]] -> String -> Int -> IO()
jogar tabuleiro jogadorDaVez contador = do
    limparTela 
    imprimirTauleiro tabuleiro 1
    putStrLn("Sua vez jogador " ++ jogadorDaVez )
    putStrLn ("Em qual linha voce quer jogar? (Ex: 1)")
    linha_lida <- getLine
    let linha = (read linha_lida :: Int)
    putStrLn ("Em qual coluna voce quer jogar? (Ex: A)")
    coluna_lida <- getLine
    let coluna = posicaoColuna guiaColuna coluna_lida 1
    --let coluna = (read coluna_lida :: Int)
    let podeJogar = verificaPossibilidadeDeJogo tabuleiro linha coluna 1 jogadorDaVez
    if not podeJogar
        then do
            putStrLn("")
            putStrLn("")
            putStrLn("Jogada em posição inválida")
            jogar tabuleiro jogadorDaVez contador
        else do
            limparTela
    let tabuleiroComJogada = realizarJogada tabuleiro jogadorDaVez linha coluna
    let coordenadas = [(linha,coluna)]
    limparTela 
    imprimeResolucaoTabuleiro tabuleiroComJogada jogadorDaVez coordenadas
    let tabuleiroResolvido = resolverTabuleiro tabuleiroComJogada jogadorDaVez coordenadas
    if(contador > 2 && checaVencedor tabuleiroResolvido jogadorDaVez 1) then do
       putStr("Parabéns, jogador " ++ jogadorDaVez ++ ", Você venceu!")
    else do
        jogar tabuleiroResolvido (inverterJogador jogadorDaVez) (contador+1)

-- Funcao principal do programa
main :: IO ()
main = do
    mostrarMenu
    iniciarJogo