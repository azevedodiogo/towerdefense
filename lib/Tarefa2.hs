{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Diogo Matos Azevedo <a109727@alunos.uminho.pt>
              Vera da Silva Almeida <a110723@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

---------------------------------------------------------------------------------------------------------------------------------------

{-| a função `inimigosNoAlcance` calcula os inimigos ao alcance de uma dada torre. 

=== Exemplo de Uso:

* `torre` = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))

* `listaInimigos` = [
            Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0,
            Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0,
            Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (10.0, 10.0) 0
          ]

>>> inimigosNoAlcance torre listaInimigos     
    [Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (3.0,4.0), tempoInimigo = 0.0},
    Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (7.0,5.0), tempoInimigo = 0.0}] 

-}


inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance t (i:li) | dist (posicaoTorre t) (posicaoInimigo i) <= alcanceTorre t = i : inimigosNoAlcance t li
                           | otherwise = inimigosNoAlcance t li

    where dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

---------------------------------------------------------------------------------------------------------------------------------------

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo t i = i
     {vidaInimigo = max 0 (vidaInimigo i - danoTorre t),                                -- o inimigo perde nível de vida conforme o dano da torre
      projeteisInimigo = junçaoProjetil (projetilTorre t) (projeteisInimigo i)}         -- lista dos novos projeteis que atingiram o inimigo
