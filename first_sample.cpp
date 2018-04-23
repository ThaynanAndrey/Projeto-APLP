#include <bits/stdc++.h> 
#include <iostream> 
#define TAM 12
#define MAX_IT 1000

using namespace std;

struct posicao {
  int cor, x, y, stack;
};

void limparTab(posicao tabuleiro[TAM][TAM]) {
  for (int i = 0; i < TAM; i++) {
    for (int j = 0; j < TAM; j++) {
      tabuleiro[i][j].cor = 0;
      tabuleiro[i][j].stack = 0;
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
  }
}

void printTab(posicao tabuleiro[TAM][TAM]) {
  for (int i = 0; i < TAM; i++) {
    for (int j = 0; j < TAM; j++) {
      posicao p = tabuleiro[i][j];
      cout << (p.cor == 0 ? "N" : p.cor == 1 ? "A" : "B") << p.stack;
      cout << " ";
    }
    cout << endl;
  }
}

int main() {
  posicao tabuleiro[TAM][TAM];
  limparTab(tabuleiro);

  posicao* jogada1 = & tabuleiro[0][0];

  (*jogada1).stack = 2;
  (*jogada1).cor = 1;
  resolverTabuleiro(jogada1, tabuleiro);

  (*jogada1).stack = 2;
  (*jogada1).cor = 1;
  resolverTabuleiro(jogada1, tabuleiro);

  (*jogada1).stack = 2;
  (*jogada1).cor = 1;
  resolverTabuleiro(jogada1, tabuleiro);

  printTab(tabuleiro);

  // TODO:
  // 1. Menu para interação no jogo
  // 2. Definir como será a impressão do tabuleiro
  // 3. Implementar jogabilidade pela linha de comando
  // 4. Uniformizar código
  // 5. Documentar, se necessário
  // 6. Definir se haverá impressão da animação do tabuleiro se resolvendo (interessante)
}
