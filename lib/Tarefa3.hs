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