{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Diogo Matos Azevedo <a109727@alunos.uminho.pt>
              Vera da Silva Almeida <a110723@alunos.uminho.pt>

-- O propósito desta tarefa é simular um modelo de jogo, com um foco nos movimentos dos inimigos e na interação destes com as torres e a base.
Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}

module Tarefa3 where

import Tarefa1
import Tarefa2
import LI12425
import Data.List

--------------------------------------------------------------------------------------------------------------------------------------------------------


{- | a função `atualizaJogo` simula a evolução de um jogo após um intervalo de tempo. 

As atualizações são realizadas em três etapas principais:
   
1. Atualização dos portais, adicionando novos inimigos ao jogo.
2. Atualização dos inimigos (movimento, vida, posição, etc.) e da base (vida e créditos).
3. Atualização das torres e dos seus efeitos sobre os inimigos.

=== Exemplos de Uso:

* `base` = Base {vidaBase = 100, posicaoBase = (2, 2), creditosBase = 50}
* `portal` = Portal {posicaoPortal = (0, 0), ondasPortal = [Onda [] 5 0 0]}
* `torre` = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))
* `mapa` = [ [Terra, Relva, Agua, Relva, Terra], [Terra, Terra, Terra, Relva, Agua], [Agua, Relva, Terra, Relva, Relva] ]
* `inimigo` = Inimigo (1, 1) Norte 100 1.0 10 20 [] (0, 0) 0
* `jogo` = Jogo base [portal] [torre] mapa [inimigo] [] Um

>>>
atualizaJogo 0.3 jogo
Jogo {
baseJogo = Base {vidaBase = 100.0, posicaoBase = (2.0,2.0), creditosBase = 50}, 
portaisJogo = [Portal {posicaoPortal = (0.0,0.0), ondasPortal = []}], 
torresJogo = [Torre {posicaoTorre = (5.0,5.0), danoTorre = 25.0, alcanceTorre = 3.0, rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 0.0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}}], 
mapaJogo = [[Terra,Relva,Agua,Relva,Terra],[Terra,Terra,Terra,Relva,Agua],[Agua,Relva,Terra,Relva,Relva]], 
inimigosJogo = [Inimigo {posicaoInimigo = (1.3,1.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (0.0,0.0), tempoInimigo = 0.0}], 
lojaJogo = [], 
nivelJogo = Um }

-}

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t (Jogo base portais torres mapa inimigos loja n) =

    let (portaisAtualizados, novosInimigos) = atualizaPortais t portais inimigos
        (baseAtualizada, inimigosAtualizados) = atualizaInimigos t mapa base novosInimigos
        (torresAtualizadas, inimigosAtingidos) = atualizaTorres t torres inimigosAtualizados


    in Jogo baseAtualizada portaisAtualizados torresAtualizadas mapa inimigosAtingidos loja n


---------------------------------------------------------------------------------------------


-- COMPORTAMENTO DAS TORRES


{- | A função `disparaTorre` faz a torre disparar projeteis contra os inimigos detetados. Utiliza funções auxiliares como a `inimigosNoAlcance` e a `atingeInimigo` da Tarefa 2.

=== Exemplos de Uso:

* `torre` = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))

* `listaInimigos` = [
            Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0,
            Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0,
            Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (10.0, 10.0) 0
          ]

>>> disparaTorre 0.2 torre1 listaInimigos
(Torre {posicaoTorre = (5.0,5.0), danoTorre = 25.0, alcanceTorre = 3.0, rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 2.0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}},
[Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 75.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}, tempoInimigo = 0.0], posInicial = (3.0,4.0)},Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 55.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}], posInicial = (7.0,5.0), tempoInimigo = 0.0},Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [], posInicial = (10.0,10.0), tempoInimigo = 0.0}])

-}

disparaTorre :: Tempo -> Torre -> [Inimigo] -> (Torre, [Inimigo])
disparaTorre tempo torre inimigos

            -- A torre ainda não pode disparar (cooldown).
            | tempoTorre torre > 0 = (torre {tempoTorre = max 0 (tempoTorre torre - tempo)}, inimigos)

            -- Sem inimigos no alcance.
            | null (inimigosNoAlcance torre inimigos) = (torre, inimigos)                       -- Utiliza a funcao 'inimigosNoAlcance' da Tarefa2.

            -- A torre pode disparar e tem inimigos no alcance.
            | otherwise = let ini = geraID inimigos                                             -- Lista dos inimigos com ID.
                              iniIDalcance = iniNoAlcanceID torre ini                           -- Seleciona os inimigos que estão ao alcance da torre.
                              iniRajada = take (rajadaTorre torre) iniIDalcance                 -- Seleciona os inimigos que a torre consegue atacar de uma só vez.
                              iniAtingidos = atualizaInimigoID torre iniRajada                  -- Atualiza os inimigos atingidos, utilizando a funcao 'atingeInimigo' da tarefa2.
                              iniRestantes = filtraInimigoID ini iniRajada                      -- Retira os inimigos atingidos, ficando apenas os outros.
                              lista = iniAtingidos ++ iniRestantes                              -- Lista atualizada com todos os inimigos em jogo e respetivas ids.
                              lordenada = sortBy (\(x, _) (y, _) -> compare x y) lista          -- Lista por ordem.


                          in (torre {tempoTorre = cicloTorre torre}, map snd lordenada)




{- | a função `geraID` gera uma id para cada inimigo da lista. 

Implementamos esta função porque os inimigos de uma onda inicialmente têm todos as mesmas características, logo se utilizassemos a 'elem' para retirar, da lista de inimigos, os inimigos que vão ser atingidos pela torre (iniRajada) iriamos ter uma lista no final com menos inimigos do que o suposto. 
Assim, a nossa solução foi inventar uma identidade (atribuindo-lhes um número) para os inimigos, pelo que em vez de comprarmos o inimigo em si, comparamos apenas as suas identidades. 

=== Exemplos de Uso:

* `inimigos` = [Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0, Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0)] 0

>>> geraID inimigos
[(0,Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (3.0,4.0), tempoInimigo = 0.0}),
 (1,Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (7.0,5.0), tempoInimigo = 0.0})]

-}

geraID :: [Inimigo] -> [(Int, Inimigo)]
geraID = zip [0..]