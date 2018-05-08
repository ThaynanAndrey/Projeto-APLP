#include <bits/stdc++.h>
#include <iostream>
#include <unistd.h>
#define TAM 12
#define MAX_IT 1000
#define VAZIO 0
#define J1 1
#define J2 2

using namespace std;

string guias = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

void limparTela() {
  cout << string( 100, '\n' );
  cout << "\033[1;1H";
}

struct posicao {
  int cor, x, y, stack;
};

void limparTab(posicao tabuleiro[TAM][TAM]) {
  for (int i = 0; i < TAM; i++) {
    for (int j = 0; j < TAM; j++) {
      tabuleiro[i][j].cor = VAZIO;
      tabuleiro[i][j].stack = VAZIO;
      tabuleiro[i][j].x = i;
      tabuleiro[i][j].y = j;
    }
  }
}

bool isCanto(posicao jogada) {
  return jogada.x == 0 && jogada.y == 0
    || jogada.x == 0 && jogada.y == TAM - 1
    || jogada.x == TAM - 1 && jogada.y == 0
    || jogada.x == TAM - 1 && jogada.y == TAM - 1;
}

bool isParede(posicao jogada) {
  return !isCanto(jogada)
    && (jogada.x == 0
        || jogada.y == 0
        || jogada.x == TAM - 1
        || jogada.y == TAM - 1);
}

bool mayExplode(posicao jogada) {
  return isCanto(jogada) && jogada.stack > 1
    || isParede(jogada) && jogada.stack > 2
    || jogada.stack > 3;
}

bool isCoordenadaValida(int x, int y) {
  return x >= 0 && y >= 0 && x < TAM && y < TAM;
}

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

void printTab(posicao tabuleiro[TAM][TAM]) {
  limparTela();
  cout << "------------Tabuleiro--------------" << endl;
  cout << "   ";

  for (int i = 0; i < TAM; i++) { cout << "  " << guias[i] << " "; }
  cout << endl;
  
  cout << "   ";
  for (int i = 0; i < TAM; i++) { cout << " __ "; }
  cout << endl;
  
  for (int i = 0; i < TAM; i++) {
    for (int j = 0; j < TAM; j++) {
      posicao p = tabuleiro[i][j];
      if (j == 0) { 
        cout << (i < 9 ? "0" : "") << (i+1) << " |"; 
      }

      char n[1];
      sprintf(n, "%d", p.stack);

      cout << " ";
      cout << (p.cor == VAZIO ? " " : p.cor == J1 ? "A" : "B");
      cout << (p.stack == VAZIO ? " " : n);
      cout << " ";
    }
    cout << "|";
    cout << endl;
  }
  
  cout << "   ";
  for (int i = 0; i < TAM; i++) { cout << " __ "; }
  cout << endl;
}

void resolverTabuleiro(posicao* jogada, posicao tabuleiro[TAM][TAM]) {
  deque<posicao*> aProcessar;
  aProcessar.push_back(jogada);

  int iteracao = 0;
  while (!aProcessar.empty() && iteracao++ < MAX_IT) {
    posicao* proximo = aProcessar.front();
    aProcessar.pop_front();

    if (!mayExplode(*proximo)) {
      continue;
    }

    vector<posicao*> vizinhos = getVizinhos(proximo, tabuleiro);
    for (int i = 0; i < vizinhos.size(); i++) {
      posicao* viz = vizinhos[i];
      (*viz).stack++;
      (*viz).cor = (*proximo).cor;
      if (mayExplode(*viz)) {
        aProcessar.push_front(viz);
      } else {
        aProcessar.push_back(viz);
      }
    }

    (*proximo).stack = 0;
    (*proximo).cor = 0;

    printTab(tabuleiro);
    usleep(1000 * 500);
  }
}

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

void realizarJogada(int numJogador, posicao tabuleiro[TAM][TAM]) {
    int opcaoColunaJogador, opcaoLinhaJogador;
    char letraLinha;

    printTab(tabuleiro);

    cout << endl << "Jogador " << guias[numJogador-1] << endl;
    cout << "Escolha a posicao: ";
    cin >> letraLinha >> opcaoColunaJogador;

    opcaoLinhaJogador = guias.find(letraLinha, 0);
    opcaoColunaJogador--;

    posicao* jogada = & tabuleiro[opcaoColunaJogador][opcaoLinhaJogador];
    
    bool casaInvalida = (*jogada).stack > VAZIO && (*jogada).cor != numJogador;
    if (casaInvalida) {
      realizarJogada(numJogador, tabuleiro);
      return;
    }

    if((*jogada).cor == VAZIO) {
        (*jogada).stack = 1;
        (*jogada).cor = numJogador;
    } else if((*jogada).cor == numJogador) {
        (*jogada).stack++;
    }
    resolverTabuleiro(jogada, tabuleiro);
}

int main() {
  int opcao = imprimirMenuInicial();
  
  if(opcao == 1) {
    posicao tabuleiro[TAM][TAM];
    limparTab(tabuleiro);
    int jogadorAtual = J1;
    do {
      realizarJogada(jogadorAtual, tabuleiro);
      jogadorAtual = jogadorAtual == J1 ? J2 : J1;
    } while (getGanhador(tabuleiro) == VAZIO);

    cout << endl << "Jogador " << (getGanhador(tabuleiro) == J1 ? "A" : "B") << " venceu!" << endl;
  } else {
    cout << "Até a próxima!" << endl;
  }

  // TODO:
  // 1. Menu para interação no jogo (Quase finalizado)
  // 2. Definir como será a impressão do tabuleiro (FALTA)
  // 3. Implementar jogabilidade pela linha de comando (Feito)
  // 4. Uniformizar código (FALTA)
  // 5. Documentar, se necessário (FALTA)
  // 6. Definir se haverá impressão da animação do tabuleiro se resolvendo (FALTA)
}
