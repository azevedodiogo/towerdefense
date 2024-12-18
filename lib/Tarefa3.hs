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
