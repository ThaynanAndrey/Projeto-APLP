# Projeto-APLP
Projeto de Aplicações de Paradigmas de Programação escrito nas linguagens C++, Haskell e Prolog para o jogo Chain Reaction.

### Restrições em C++:  
* Utilizar o compilador g++ 5.4.0, executado em ambiente Ubuntu 5.4.0-6ubuntu1~16.04.9.  

### 1. O que é:  
  É um jogo de tabuleiro que pode ser jogado por 2 jogadores, no qual o jogador deve ir conquistando casas para alcançar e ocupar as casas do adversário.

### 2. Elementos do jogo:  
  - O jogo ocorre no terminal do computador;  
  - Um tabuleiro de tamanho definido pelo usuário, sendo tamanho mínimo 12x8, podendo-se aumentar em ambas a direções;  
  - Cada jogador recebe uma letra que será apresentada nas posições do tabuleiro, ou seja, primeiro jogador recebe a letra A e o segundo a letra B. Ao lado da letra terá o número de bolinhas que ele possui naquela posição;  
Espaços que os jogadores não alcançaram ainda no decorrer do jogo ficam vazios.

Segue uma ideia de tabuleiro a ser apresentado em um tabuleiro 2x2:

			|  A1 |    |
			|  B2 | A1 |

### 3. Como jogar
  Cada jogador seleciona uma posição no tabuleiro por vez, onde esta posição pode estar vazia ou ter um marcador da sua cor.

   #### a. Ao jogar

   ##### i. Jogar em casa vazia: Faz com que a casa assuma o valor 1 para cor do jogador 
   ##### ii. Jogar em casa com marcador: Haverão três situações, quando a peça estiver numa quina do tabuleiro, ao colocar o   segundo marcador em cima do primeiro vai fazer com que este se divida e ocupe as duas posições adjacentes, deixando a posição da quina livre. Quando a peça estiver numa parede, o mesmo acontece na colocação do terceiro marcador. Para qualquer outra posição no tabuleiro no quarto marcador.

   #### b. Efeitos colaterais

   Quando um marcador explode e suas peças vão para as casas adjacentes, elas podem “stackar” com peças já existentes, e causar novas divisões, as divisões acabam quando não há mais peças a se dividirem ou se algum jogador ganhar. Se uma das casas vizinhas forem de marcadores inimigos, estes passam a ser do jogador que desencadeou a divisão, independente da quantidade de peças adversárias na casa atual.

   (As explosões serão representadas por uma sequência de impressões do tabuleiro no terminal).

   #### c. Fim de jogo

   O jogo acaba quando apenas um jogador ainda tem marcadores no tabuleiro.
