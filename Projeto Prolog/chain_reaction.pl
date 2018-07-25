
% -- Criação matriz
% -- Estrutura de posição: X Y jogador(0/1/2) countBolinhas

cria_linha(X, 0).
cria_linha(0, X).
cria_linha(X, Y):-
    Z is Y - 1,
    cria_linha(X, Z),
    assertz(posicao(X, Y, 0, 0)).

cria_matriz(0, Y).
cria_matriz(X, 0).
cria_matriz(X, Y):-
    Z is X - 1,
    cria_matriz(Z, Y),
    cria_linha(X, Y).

cria_matriz:-
    tam(X, Y),
    cria_matriz(X, Y).
    
% -- Criação matriz
% -- Imprimir tabuleiro

nova_linha(A, B):- 
    A =:= B -> nl ; true.

  % Imprime uma casa para jogador 1 (A) %
print_info_posicao(1, Qtd):-
    format('A~w ', [Qtd]).
  % Imprime uma casa para jogador -1 (B) %
print_info_posicao(-1, Qtd):-
    format('B~w ', [Qtd]).
  % Imprime uma casa para jogador 0 (vazio) %
print_info_posicao(SemJogador, Qtd):-
    format(' ~w ', [Qtd]).

  % Casos base %
print_tab(0, 0, YY).
print_tab(0, Y, YY).

  %  Se chegar na primeira coluna ele vai pra linha de cima, começando da última coluna %
print_tab(X, 0, YY):-
    nl,
    NX is X - 1,
    print_tab(NX, YY, YY).

  %  Atravessa a matriz (fatos) de trás pra frente, por linhas, começando da última linha e última coluna %
print_tab(X, Y, YY):-
    NY is Y - 1,
    % Bottom-up %
    print_tab(X, NY, YY),
    posicao(X, Y, Jogador, Qtd),
    print_info_posicao(Jogador, Qtd),
    nova_linha(Y, YY).

  % Fato a ser utilizado por quem desejar imprimir o tabuleiro %
print_tab:- 
    write('\33\[2J'),
    tam(X, Y),
    print_tab(X, Y, Y).

% -- Imprimir tabuleiro
% -- Limpar posição

reset_posicao(X, Y):- 
    retract(posicao(X, Y, Cor, Qtd)),
    assertz(posicao(X, Y, 0, 0)).

% -- Limpar posição
% -- Atualizar posicao

atualizar_posicao(X, Y, Jogador):-
    retract(posicao(X, Y, Cor, Qtd)),
    QtdNova is Qtd + 1,
    assertz(posicao(X, Y, Jogador, QtdNova)),
    tam(XX, YY),
    call(explode(X, Y, QtdNova, XX, YY)) -> 
        sleep(0.5),
        print_tab,
        write('~n-- Resolvendo tabuleiro --'),
        reset_posicao(X, Y),
        add_esquerda(X, Y, YY, Jogador),
        add_direita(X, Y, YY, Jogador),
        add_cima(X, Y, XX, Jogador),
        add_baixo(X, Y, XX, Jogador)
        ;
        true.

% -- Atualizar posicao
% -- Resolver tabuleiro

explode(1, 1, 2, XX, YY).
explode(1, YY, 2, XX, YY).
explode(XX, 1, 2, XX, YY).
explode(XX, YY, 2, XX, YY).

explode(1, Y, 3, XX, YY).
explode(X, 1, 3, XX, YY).
explode(XX, Y, 3, XX, YY).
explode(X, YY, 3, XX, YY).

explode(X, Y, 4, XX, YY).

add_direita(X, YY, YY, Jogador).
add_direita(X, Y, YY, Jogador):-
    Ny is Y + 1,
    atualizar_posicao(X, Ny, Jogador).

add_esquerda(X, 0, YY, Jogador).
add_esquerda(X, Y, YY, Jogador):-
    Ny is Y - 1,
    atualizar_posicao(X, Ny, Jogador).

add_cima(0, Y, XX, Jogador).
add_cima(X, Y, XX, Jogador):-
    Nx is X - 1,
    atualizar_posicao(Nx, Y, Jogador).

add_baixo(XX, Y, XX, Jogador).
add_baixo(X, Y, XX, Jogador):-
    Nx is X + 1,
    atualizar_posicao(Nx, Y, Jogador).

resolver_tabuleiro(X, Y, Jogador):-
    tam(XX, YY),
    write('deveria resolver centro'),
    nl.

% -- Resolver tabuleiro
% -- Realizar jogada

realizar_jogada(Jogador):-
    format('~nVez do jogador ~w. Insira a jogada (Ex 1. 2.): ', [Jogador]),
    read(X),
    read(Y),
    call(posicao(X,Y,Cor, Qntd)),
    ((Cor == Jogador);(Cor == 0)) ->
        format('~nJogando em ~w x ~w', [X, Y]),
        atualizar_posicao(X, Y, Jogador)
        ;
        realizar_jogada(Jogador).

% -- Realizar jogada
% -- Menu Jogar

menu_jogar(Jogador, Turno):-
    ((ganhador),(Turno > 2))->
     JogadorVencedor is Jogador * -1,
     write('\33\[2J'),
     print_tab,
     format('~n Jogador ~w VENCEU !!!~n', [JogadorVencedor]),
     halt(0);
    print_tab,
    realizar_jogada(Jogador),
    NJogador is Jogador * -1,
    ProxTurno is Turno + 1,
    menu_jogar(NJogador, ProxTurno).

% -- Menu Jogar
% -- Configurar tab - em seguida inicia o jogo

tam_valido(X, Y):-
    X > 2, 
    X < 25,
    Y > 2, 
    Y < 25.

configurar_tab:-
    write('Insira o tamanho do tabuleiro (Ex 5. 5.): '),
    read(TamX),
    read(TamY),
    tam_valido(TamX, TamY) -> 
        retractall(tam(X, Y)),
        assertz(tam(TamX, TamY)),
        cria_matriz,
        menu_jogar(1, 0)
        ; 
        configurar_tab.

% -- Configurar tab
% -- Verificar ganhador

nao_tem_a:-
    not(call(posicao(X, Y, 1, B))).

nao_tem_b:-
    not(call(posicao(X, Y, -1, B))).

ganhador:-
        ((nao_tem_a) , not(nao_tem_b)) ; (not(nao_tem_a), (nao_tem_b)).
 
% -- Verificar ganhador
% -- Menu

opcao(1):-
    configurar_tab.

menu:-
    write('Bem vindo a Chain Reaction!'),
    nl,
    write('Para jogar insira 1, para sair insira 2.'),
    nl,
    read(OPCAO),
    opcao(OPCAO).
    menu.

% -- Menu

:- initialization(main).
main :- 
    menu,
    halt(0).
