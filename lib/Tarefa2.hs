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


----------------------------------------------------------------------------------------------------------------------------------------


{- | a função `atingeInimigo` atualiza o estado de um inimigo sempre que este é atingido por um projétil. 

=== Exemplos de Uso:

* `inimigo` = Inimigo (3.0, 4.0) Norte 100 1 10 20 [Projetil Fogo (Finita 5)] (0,0) 0
* `torre` = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))

>>> atingeInimigo torre inimigo 
Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 75.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 12.0}], posInicial = (0.0,0.0), tempoInimigo = 0.0}
-}

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo t i = i
     {vidaInimigo = max 0 (vidaInimigo i - danoTorre t),                                -- o inimigo perde nível de vida conforme o dano da torre
      projeteisInimigo = junçaoProjetil (projetilTorre t) (projeteisInimigo i)}         -- lista dos novos projeteis que atingiram o inimigo



{- | a função `junçaoProjetil` junta os projeteis com base no seu tipo, utilizando as funções `iguais` e `resolveConflito`. -}

junçaoProjetil :: Projetil -> [Projetil] -> [Projetil]
junçaoProjetil p [] = [p]
junçaoProjetil pnovo lpantigos = iguais lpconflitoTransformada

    where lpconflitoTransformada = resolveConflito pnovo lpantigos



{- | a função `iguais` soma as suas duracoes, caso o tipo do projetil novo e um dos projeteis do inimigo sejam iguais. 

=== Exemplos de Uso:

* `projeteis` = [Projetil Gelo (Finita 3), Projetil Gelo (Finita 5), Projetil Resina Infinita]

>>> iguais projeteis
[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 8.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}]

-}

iguais :: [Projetil] -> [Projetil]
iguais [] = []
iguais [p] = [p]
iguais (pn:l:ls) | tipopn == tipol = pn {duracaoProjetil = somaduracao duracaopn duracaol} : ls
                 | otherwise = l : iguais (pn:ls)

        where duracaopn = duracaoProjetil pn
              duracaol = duracaoProjetil l
              tipopn = tipoProjetil pn
              tipol = tipoProjetil l


{- | a função `somaduracao` soma as duracoes de dois projeteis. -}

somaduracao :: Duracao -> Duracao -> Duracao
somaduracao Infinita _ = Infinita
somaduracao _ Infinita = Infinita
somaduracao (Finita a) (Finita b) = Finita (a+b)


{- | a função `resolveConflito` resolve conflitos entre projéteis: Fogo e Gelo cancelam-se mutuamente, Fogo e Resina dobra a duração do fogo.

=== Exemplos de Uso:

* `projetil1` = Projetil Gelo (Finita 3)
* `projetil2` = Projetil Resina Infinita
* `projetil3` = Projetil Fogo (Finita 5) 

>>> resolveConflito projetil3 [projetil1]
[]
>>> resolveConflito projetil3 [projetil2]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0}]
>>> resolveConflito projetil3 [projetil1, projetil2]
[Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}]

-}