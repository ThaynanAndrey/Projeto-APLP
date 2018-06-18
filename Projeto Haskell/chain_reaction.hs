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
    jogar meuTabuleiro "A"

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

-- Realiza a jogada do Usuario.
realizarJogada :: [[(String, Int)]] -> String -> Int -> Int -> [[(String, Int)]]
realizarJogada tabuleiro jogadorDaVez linha coluna = do
    inserirNoTabuleiro tabuleiro jogadorDaVez linha 1 coluna
    -- e realizar o chain reaction. 


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
checaPosicaoDeExplosao [] [] _ _ qtdBolinhas = (qtdBolinhas == 2)
checaPosicaoDeExplosao _ _ 1 1 qtdBolinhas =(qtdBolinhas == 2)
checaPosicaoDeExplosao [] _ _ 1 qtdBolinhas = (qtdBolinhas == 2)
checaPosicaoDeExplosao _ [] 1 _ qtdBolinhas =(qtdBolinhas == 2)
checaPosicaoDeExplosao [] _ _ _ qtdBolinhas =(qtdBolinhas == 3)
checaPosicaoDeExplosao _ [] _ _ qtdBolinhas =(qtdBolinhas == 3)
checaPosicaoDeExplosao _ _ 1 _ qtdBolinhas =(qtdBolinhas == 3)
checaPosicaoDeExplosao _ _ _ 1 qtdBolinhas =(qtdBolinhas == 3)
checaPosicaoDeExplosao proxima_linha proxima_coluna linha_cont coluna_cont qtdBolinhas = (qtdBolinhas == 4)



--inserirBolasNosVizinhos :: [[(String, Int)]] -> (Int,Int) -> String -> [[(String, Int)]]
--inserirBolasNosVizinhos tabuleiro posicoes jogadorDaVez = tabuleiro ++ [("Caio", 21)]


-- Inverte o jogador da vez.
inverterJogador :: String -> String
inverterJogador jogadorDaVez
    | jogadorDaVez == "A" = "B"
    | otherwise = "A"

-- Funcao responsavel pelo controle de jogo
jogar :: [[(String, Int)]] -> String -> IO()
jogar tabuleiro jogadorDaVez = do
    -- Fazer a verificacao de se algum jogador ja ganhou
    putStrLn("")
    imprimirTauleiro tabuleiro
    putStrLn("Sua vez jogador " ++ jogadorDaVez )
    putStrLn ("Em qual linha voce quer jogar?")
    linha_lida <- getLine
    let linha = (read linha_lida :: Int)
    putStrLn ("Em qual coluna voce quer jogar?")
    coluna_lida <- getLine
    let coluna = (read coluna_lida :: Int)
    let tabuleiroComJogada = realizarJogada tabuleiro jogadorDaVez linha coluna
    jogar tabuleiroComJogada (inverterJogador jogadorDaVez)

-- Funcao principal do programa
main :: IO ()
main = do
    mostrarMenu
    iniciarJogo