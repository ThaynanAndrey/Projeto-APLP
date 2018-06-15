import Data.Char

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
formatarLinhaTabuleiro :: [String] -> String
formatarLinhaTabuleiro [] = ""
formatarLinhaTabuleiro (a:xs) = a ++ " " ++ formatarLinhaTabuleiro xs 

-- Formata cada linha do jogo para apresentacao ao Usuario.
obterLinhaFormatada :: [(String, Int)] -> String
obterLinhaFormatada linha = do
    let linhaFormatada = [ a ++ (show b) | (a, b) <- linha]  
    formatarLinhaTabuleiro linhaFormatada

-- Realiza a impressao do tabuleiro.
imprimirTauleiro :: [[(String, Int)]] -> IO()
imprimirTauleiro [] = putStrLn("")
imprimirTauleiro (a:xs) = do
    let linhaFormatada = obterLinhaFormatada a
    print linhaFormatada
    imprimirTauleiro(xs)

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
    jogar meuTabuleiro 1

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

-- Realiza a jogada do Usuario.
realizarJogada :: Int -> Int -> Int -> IO()
realizarJogada jogadorDaVez linha coluna = do
    -- Desconsiderar esses prints
    putStrLn ("Voce chegou ate aqui, parabens")
    print "-----"
    print jogadorDaVez
    print linha
    print coluna
    print "-----"
    -- inserir o elemento na posicao que o jogador passou
    -- e realizar o chain reaction. 

-- Inverte o jogador da vez.
inverterJogador :: Int -> Int
inverterJogador jogadorDaVez | jogadorDaVez == 1 = 2
                             | otherwise = 1

-- Funcao responsavel pelo controle de jogo
jogar :: [[(String, Int)]] -> Int -> IO()
jogar tabuleiro jogadorDaVez = do
    -- Fazer a verificacao de se algum jogador ja ganhou
    imprimirTauleiro tabuleiro
    putStrLn ("Em qual linha voce quer jogar?")
    linha_lida <- getLine
    let linha = (read linha_lida :: Int)
    putStrLn ("Em qual coluna voce quer jogar?")
    coluna_lida <- getLine
    let coluna = (read coluna_lida :: Int)
    realizarJogada jogadorDaVez linha coluna
    jogar tabuleiro (inverterJogador jogadorDaVez)

-- Funcao principal do programa
main :: IO ()
main = do
    mostrarMenu
    iniciarJogo