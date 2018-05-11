#include <bits/stdc++.h>
#include <iostream>
#include <unistd.h>
#define TAM 12
#define MAX_IT 1000
#define VAZIO 0
#define J1 1
#define J2 2
#define INDICE_PRIMEIRA_LINHA 0
#define INDICE_PRIMEIRA_COLUNA 0

using namespace std;

/**
 * Guia utilizado para as posições do tabuleiro, ou seja, letra
 * A representa coluna 0, letra B coluna 1, e assim por diante.
 */
string guias = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

/**
 * Limpa a tela do terminal para que o tabuleiro seja impresso no final do
 * terminal, facilitando a interação com o usuário.
 */
void limparTela() {
  cout << string( 100, '\n' );
  cout << "\033[1;1H";
}

/**
 * Estrutura para definir as posições do tabuleiro.
 * - cor: cor do jogador;
 * - x: número da linha no tabuleiro;
 * - y: número da coluna no tabuleiro;
 * - pilhaDeBolinhas: quantidade de bolinhas empilhadas nessa posição.
 */
struct posicao {
  int cor, x, y, pilhaDeBolinhas;
};

/**
 *Limpa o tabuleiro do jogo, deixando todas as posições vazias.
 */
void limparTab(posicao tabuleiro[TAM][TAM]) {
  for (int i = 0; i < TAM; i++) {
    for (int j = 0; j < TAM; j++) {
      tabuleiro[i][j].cor = VAZIO;
      tabuleiro[i][j].pilhaDeBolinhas = VAZIO;
      tabuleiro[i][j].x = i;
      tabuleiro[i][j].y = j;
    }
  }
}

/**
 * Retorna um booleano que indica se a jogada é na quina do tabuleiro.
 *
 * @param {posicao} jogada, Jogada realizada em determinada posição.
 * @return {bool} True se está na quina, false caso contrário.
 */
bool isQuina(posicao jogada) {
  const int INDICE_ULTIMA_COLUNA = TAM - 1;
  const int INDICE_ULTIMA_LINHA = TAM - 1;
  return jogada.x == INDICE_PRIMEIRA_LINHA && jogada.y == INDICE_PRIMEIRA_COLUNA
    || jogada.x == INDICE_PRIMEIRA_LINHA && jogada.y == INDICE_ULTIMA_COLUNA
    || jogada.x == INDICE_ULTIMA_LINHA && jogada.y == INDICE_PRIMEIRA_COLUNA
    || jogada.x == INDICE_ULTIMA_LINHA && jogada.y == INDICE_ULTIMA_COLUNA;
}

/**
 * Retorna um booleano que indica se a jogada foi realizada em uma
 * das paredes do tabuleiro.
 *
 * @param {posicao} jogada, Jogada realizada em determinada posição.
 * @return {bool} True se está em uma das paredes do tabuleiro, false caso contrário.
 */
bool isParede(posicao jogada) {
  const int INDICE_ULTIMA_COLUNA = TAM - 1;
  const int INDICE_ULTIMA_LINHA = TAM - 1;
  return !isQuina(jogada)
    && (jogada.x == INDICE_PRIMEIRA_LINHA
        || jogada.y == INDICE_PRIMEIRA_COLUNA
        || jogada.x == INDICE_ULTIMA_LINHA
        || jogada.y == INDICE_ULTIMA_COLUNA);
}

/**
 * Retorna um booleano que indica se pode explodir na quina do tabuleiro.
 *
 * @param {posicao} jogada, Jogada realizada em determinada posição.
 * @return {bool} True se pode explodir, false caso contrário.
 */
bool podeExplodirNaQuina(posicao jogada) {
  const int QUANTIDADE_MAX_BOLINHAS_QUINA = 1;
  return isQuina(jogada) && jogada.pilhaDeBolinhas > QUANTIDADE_MAX_BOLINHAS_QUINA;
}

/**
 * Retorna um booleano que indica se pode explodir na parede do tabuleiro.
 *
 * @param {posicao} jogada, Jogada realizada em determinada posição.
 * @return {bool} True se pode explodir, false caso contrário.
 */
bool podeExplodirNaParede(posicao jogada) {
  const int QUANTIDADE_MAX_BOLINHAS_PAREDE = 2;
  return isParede(jogada) && jogada.pilhaDeBolinhas > QUANTIDADE_MAX_BOLINHAS_PAREDE;
}

/**
 * Retorna um booleano que indica se pode explodir no meio do tabuleiro, ou seja,
 * não é quina nem parede.
 *
 * @param {posicao} jogada, Jogada realizada em determinada posição.
 * @return {bool} True se pode explodir, false caso contrário.
 */
bool podeExplodirNoMeio(posicao jogada) {
  const int QUANTIDADE_MAX_BOLINHAS_MEIO = 3;
  return jogada.pilhaDeBolinhas > QUANTIDADE_MAX_BOLINHAS_MEIO;
}

/**
 * Retorna um valor booleano que indica se a pilha de bolinhas pode explodir,
 * ou seja, ultrapassou a quantidade limite de bolinhas para a posição.
 * Explode:
 *        - Se a jogada é na quina e tem mais de uma bolinha na pilha;
 *        - Se a jogada é na parede e tem mais de duas bolinhas da pilha;
 *        - Se a jogada é em outra posição que não é quina nem parede e tem
 *          mais de três bolinhas na pilha.
 *
 * @param {posicao} jogada, Jogada realizada em determinada posição.
 * @return {bool} True se pode explodir, false caso contrário.
 */
bool podeExplodir(posicao jogada) {
  return podeExplodirNaQuina(jogada)
        || podeExplodirNaParede(jogada)
        || podeExplodirNoMeio(jogada);
}

/**
 * Retorna um booleano que indica se a coordenada da jogada é válida, o que
 * implica em respeitar as dimensões máximas do tabuleiro.
 *
 * @param {int} x, coordenada x do tabuleiro.
 * @param {int} y, coordenada y do tabuleiro.
 * @return {bool} True se é uma coordenada válida, false caso contrário.
 */
bool isCoordenadaValida(int x, int y) {
  const int INDICE_ULTIMA_COLUNA = TAM - 1;
  const int INDICE_ULTIMA_LINHA = TAM - 1;
  return x >= INDICE_PRIMEIRA_LINHA
        && y >= INDICE_PRIMEIRA_COLUNA
        && x <= INDICE_ULTIMA_LINHA
        && y <= INDICE_ULTIMA_COLUNA;
}

/**
 * Obtém todos os vizinhos de uma posição determinada pela jogada.
 *
 * @param {posicao} jogada, jogada realizada em determinada posição.
 * @param {array} tabuleiro, tabuleiro do jogo.
 * @return {vector} vetor com os vizinhos.
 */
vector<posicao*> getVizinhos(posicao* jogada, posicao tabuleiro[TAM][TAM]) {
  vector<posicao*> saida;
  int x = (*jogada).x;
  int y = (*jogada).y;
  if (isCoordenadaValida(x, y + 1)) {
    saida.push_back( & tabuleiro[x][y + 1]);
  }
  if (isCoordenadaValida(x, y - 1)) {
    saida.push_back( & tabuleiro[x][y - 1]);
  }
  if (isCoordenadaValida(x + 1, y)) {
    saida.push_back( & tabuleiro[x + 1][y]);
  }
  if (isCoordenadaValida(x - 1, y)) {
    saida.push_back( & tabuleiro[x - 1][y]);
  }
  return saida;
}

/**
 * Imprime as posições do tabuleiro do jogo no terminal.
 * @param {array} tabuleiro, tabuleiro do jogo.
 */
void imprimirPosicoesTabuleiro(posicao tabuleiro[TAM][TAM]) {
  for (int i = 0; i < TAM; i++) {
    for (int j = 0; j < TAM; j++) {
      posicao p = tabuleiro[i][j];
      if (j == 0) {
        cout << (i < 9 ? "0" : "") << (i+1) << " |";
      }

      char n[1];
      sprintf(n, "%d", p.pilhaDeBolinhas);

      cout << " ";
      cout << (p.cor == VAZIO ? " " : p.cor == J1 ? "A" : "B");
      cout << (p.pilhaDeBolinhas == VAZIO ? " " : n);
      cout << " ";
    }
    cout << "|";
    cout << endl;
  }
  cout << "   ";
}

/**
 * Imprime o tabuleiro do jogo no terminal.
 * @param {array} tabuleiro, tabuleiro do jogo.
 */
void imprimirTabuleiro(posicao tabuleiro[TAM][TAM]) {
  limparTela();
  cout << "------------Tabuleiro--------------" << endl;
  cout << "   ";

  for (int i = 0; i < TAM; i++) {
    cout << "  " << guias[i] << " ";
  }
  cout << endl << "   ";
  for (int i = 0; i < TAM; i++) {
    cout << " __ ";
  }
  cout << endl;
  imprimirPosicoesTabuleiro(tabuleiro);
  for (int i = 0; i < TAM; i++) {
    cout << " __ ";
  }
  cout << endl;
}

/**
 * Realiza a jogada no tabuleiro, resolvendo as explosões que possam acontecer.
 *
 * @param {posicao} jogada, jogada realizada em determinada posição.
 * @param {array} tabuleiro, tabuleiro do jogo.
 */
void resolverTabuleiro(posicao* jogada, posicao tabuleiro[TAM][TAM]) {
  deque<posicao*> aProcessar;
  aProcessar.push_back(jogada);

  int iteracao = 0;
  while (!aProcessar.empty() && iteracao++ < MAX_IT) {
    posicao* proximo = aProcessar.front();
    aProcessar.pop_front();

    if (!podeExplodir(*proximo)) {
      continue;
    }

    vector<posicao*> vizinhos = getVizinhos(proximo, tabuleiro);
    for (int i = 0; i < vizinhos.size(); i++) {
      posicao* viz = vizinhos[i];
      (*viz).pilhaDeBolinhas++;
      (*viz).cor = (*proximo).cor;
      if (podeExplodir(*viz)) {
        aProcessar.push_front(viz);
      } else {
        aProcessar.push_back(viz);
      }
    }

    (*proximo).pilhaDeBolinhas = 0;
    (*proximo).cor = 0;

    imprimirTabuleiro(tabuleiro);
    usleep(1000 * 500);
  }
}

/**
 * Imprime o menu incial do jogo.
 */
int imprimirMenuInicial() {
  cout << "#######################################" << endl;
  cout << "#######################################" << endl;
  cout << "##########  Chain Reaction   ##########" << endl;
  cout << "#######################################" << endl;
  cout << "#######################################" << endl << endl;

  int opcao;
  cout << "------------ Menu do Jogo ------------" << endl;
  cout << "1) Começar o jogo" << endl;
  cout << "2) Sair" << endl;
  cout << "Digite o número de sua opção: " ;
  cin >> opcao;

  cout << endl << endl;

  return opcao;
}

/**
 * Obtém o ganhador do jogo, caso exista.
 *
 * @param {array} tabuleiro, Tabuleiro do jogo.
 * @return {int} inteiro que indica o vencedor do jogo ou 0 se ninguém ganhou.
 */
int getGanhador(posicao tabuleiro[TAM][TAM]) {
    int pecasJogadorUm = 0, pecasJogadorDois = 0;
    for (int i = 0; i < TAM; i++) {
      for (int j = 0; j < TAM; j++) {
        posicao p = tabuleiro[i][j];
        if (p.cor == J1) {
          pecasJogadorUm++;
        } else if (p.cor == J2) {
          pecasJogadorDois++;
        }
      }
    }

    bool ninguemJogou = pecasJogadorDois == 0 && pecasJogadorUm == 0;
    bool doisTemPecas = pecasJogadorDois > 0 && pecasJogadorUm > 0;
    bool primeiraJogada = pecasJogadorDois == 1 && pecasJogadorUm == 0
      || pecasJogadorUm == 1 && pecasJogadorDois == 0;

    bool ninguemGanhou = primeiraJogada || ninguemJogou || doisTemPecas;
    return ninguemGanhou ? VAZIO : pecasJogadorUm == 0 ? J2 : J1;
}

/**
 * Realiza a jogada determinada pelo jogador.
 *
 * @param {int} numJogador, Número do jogador responsável pela jogada.
 * @param {array} tabuleiro, Tabuleiro do jogo.
 */
void realizarJogada(int numJogador, posicao tabuleiro[TAM][TAM]) {
    int opcaoColunaJogador, opcaoLinhaJogador;
    char letraLinha;

    imprimirTabuleiro(tabuleiro);

    cout << endl << "Jogador " << guias[numJogador-1] << endl;
    cout << "Escolha a posicao (Ex: A1): ";
    cin >> letraLinha >> opcaoColunaJogador;

    opcaoLinhaJogador = guias.find(letraLinha, 0);
    opcaoColunaJogador--;

    posicao* jogada = & tabuleiro[opcaoColunaJogador][opcaoLinhaJogador];

    bool casaInvalida = (*jogada).pilhaDeBolinhas > VAZIO && (*jogada).cor != numJogador;
    if (casaInvalida) {
      realizarJogada(numJogador, tabuleiro);
    } else {
      if((*jogada).cor == VAZIO) {
          (*jogada).pilhaDeBolinhas = 1;
          (*jogada).cor = numJogador;
      } else if((*jogada).cor == numJogador) {
          (*jogada).pilhaDeBolinhas++;
      }
      resolverTabuleiro(jogada, tabuleiro);
    }
}

/**
 * Função principal do jogo.
 */
int main() {
  int opcao = imprimirMenuInicial();
  while(opcao == 1) {
    posicao tabuleiro[TAM][TAM];
    limparTab(tabuleiro);
    int jogadorAtual = J1;

    do {
      realizarJogada(jogadorAtual, tabuleiro);
      jogadorAtual = jogadorAtual == J1 ? J2 : J1;
    } while (getGanhador(tabuleiro) == VAZIO);

    cout << endl << "Parabéns, Jogador " << (getGanhador(tabuleiro) == J1 ? "A" : "B") << ", você venceu!" << endl << endl;

    opcao = imprimirMenuInicial();
  }
  cout << "Até a próxima!" << endl;
}
