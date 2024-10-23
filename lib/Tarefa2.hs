{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Diogo Matos Azevedo <a109727@alunos.uminho.pt>
              Vera da Silva Almeida <a110723@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

------------------------------------------------------------------------------------------------------------------------------------------------

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance t (i:li) | dist (posicaoTorre t) (posicaoInimigo i) <= alcanceTorre t = i : inimigosNoAlcance t li
                           | otherwise = inimigosNoAlcance t li

    where dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

------------------------------------------------------------------------------------------------------------------------------------------------