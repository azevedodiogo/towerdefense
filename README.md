# Tower Defense

Este repositório destina-se ao jogo de Tower Defense desenvolvido na unidade curricular “Laboratórios de Informática 1”.

Este jogo de Tower Defense desafia o jogador a defender a base de ondas de inimigos, usando torres para neutralizá-los. O utilizador tem a possibilidade de escolher o nível de dificuldade do jogo que pretende jogar e, quando este termina o jogo com sucesso além de ser registado num sistema de proezas tal conquista o utilizador é desafiado a avançar para um nível seguinte.

A interface inicial do jogo apresenta uma disposição apelativa de menus e opções, com o botão “Start” em destaque, direcionando o jogador para o menu de seleção de níveis previamente descrito. No lado direito da tela, encontram-se as secções dedicadas às conquistas e proezas, às regras e instruções do jogo, além dos créditos, onda consta a menção aos desenvolvedores do jogo.

O mapa é uma das bases do Tower Defense, sendo esta uma grelha constituída por três tipos de terrenos distintos, sendo eles a terra, a água e a relva. Além do mapa, elementos como torres, bases e portais são fundamentais para a estrutura do jogo.

As torres, colocadas sobre a relva, disparam automaticamente projéteis que causam dano ou aplicam efeitos como gelo (imobilizando os inimigos), fogo (dano contínuo ao inimigo) e resina (reduz a velocidade do inimigo). É de salientar que os projéteis podem ter efeitos combinados, aos quais são designados por sinergias.

Os inimigos seguem trajetórias definidas ou seja deslocam-se sempre sobre posições de terra, salvaguardando-se o facto de que este caminha para a base.
Durante o jogo, o jogador deparar-se-á com uma barra, localizada em cima de cada inimigo, de modo a que o utilizador saiba a vida do mesmo. Quando este é atingido por um projétil, a barra fica com a cor do mesmo. No decorrer do jogo, o jogador compra torres com créditos ganhos ao eliminar inimigos.
Os créditos referidos são atualizados constantemente e são visíveis durante o jogo, bem como a vida da base.

Os portais de um Tower Defense são os pontos de entrada no mapa dos inimigos, surgindo e começando desta maneira o seu percurso em direção à base do jogador. Para que estes inimigos surjam são lançadas pelos portais ondas, cada uma composta por um grupo de inimigos que são libertados em intervalos regulares. O funcionamento dos portais é essencial para o ritmo do jogo, criando desafios constantes que o jogador precisa de enfrentar e superar.

Durante o jogo, o jogador utilizará o teclado para executar ações como a aquisição de torres, reinicialização do jogo e pausa.

  1. Para pausar o jogo, o jogador deve pressionar a tecla 'p' (de "pausar").
  2. Para adquirir uma torre, o jogador deve pressionar inicialmente a tecla 'c' (de "comprar"), momento em que um círculo amarelo é exibido no canto superior esquerdo. Esse círculo indica a posição proposta para a colocação da torre. O jogador deve utilizar as teclas direcionais para mover o círculo amarelo pelo mapa e escolher a localização desejada. Para concluir a compra de uma torre, o jogador deve pressionar uma das teclas numéricas, onde cada número corresponde a uma torre específica. Caso o jogador não possua créditos suficientes para a transação, o círculo mudará de amarelo para vermelho, indicando a impossibilidade de prosseguir com a compra.
  3. Para reiniciar o jogo, voltando ao ínicio do nível que se encontra, o jogador deve pressionar a tecla 'r' (de "reiniciar")

No contexto da estrutura do jogo, particularmente no que diz respeito à conclusão do nível, a vitória é obtida quando o jogador consegue eliminar todos os adversários, assegurando a defesa integral da base. Por sua vez, a derrota ocorre com a destruição da base pelas forças inimigas.
Este documento apresenta todas as informações cruciais e necessárias para uma visão aprofundada da dinâmica e dos objetivos do jogo, garantindo que o jogador disponha de uma base sólida para tirar o máximo partido da experiência interativa.

Made with ❤️ by Diogo Azevedo & Vera Almeida

## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do Cabal.

```bash
cabal run --verbose=0
```

## Interpretador

Para abrir o interpretador do Haskell (GHCi) com o projeto carregado, utilize o comando `repl` do Cabal

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Execute os testes com o comando `test` do Cabal e utilize a flag `--enable-coverage` para gerar um relatório de cobertura de testes.

```bash
cabal test --enable-coverage
```

Execute os exemplos da documentação como testes com a biblioteca
[`doctest`](https://hackage.haskell.org/package/doctest). Para instalar o
executável utilize o comando `cabal install doctest`.

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest --verbose=0
```

## Documentação

A documentação do projeto pode ser gerada recorrendo ao [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```
