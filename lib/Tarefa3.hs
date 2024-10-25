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

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t (Jogo base portais torres mapa inimigos loja n) =

    let (portaisAtualizados, novosInimigos) = atualizaPortais t portais inimigos
        (baseAtualizada, inimigosAtualizados) = atualizaInimigos t mapa base novosInimigos
        (torresAtualizadas, inimigosAtingidos) = atualizaTorres t torres inimigosAtualizados


    in Jogo baseAtualizada portaisAtualizados torresAtualizadas mapa inimigosAtingidos loja n