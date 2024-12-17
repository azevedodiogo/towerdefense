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

resolveConflito :: Projetil -> [Projetil] -> [Projetil]

-- a lista de projeteis está vazia
resolveConflito p [] = [p]

-- a lista contem apenas um elemento
resolveConflito pn [x]  | tipoProjetil pn == Fogo && tipoProjetil x == Resina = [pn {duracaoProjetil = Finita (tempo (duracaoProjetil pn)*2)}]
                        | tipoProjetil pn == Resina && tipoProjetil x == Fogo = [x {duracaoProjetil = Finita (tempo (duracaoProjetil x)*2)}]
                        | tipoProjetil pn == Gelo && tipoProjetil x == Fogo = []
                        | tipoProjetil pn == Fogo && tipoProjetil x == Gelo = []
                        | otherwise = pn : [x]

-- a lista contem dois elementos (gelo e resina ou resina e gelo)
resolveConflito pn [x,y]  | tipoProjetil pn == Fogo && tipoProjetil x == Gelo  = [y]
                          | tipoProjetil pn == Fogo && tipoProjetil y == Gelo = [x]
                          | otherwise = pn:[x,y]



{-| a funçao `tempo` extrai o tempo de uma duracao. -}

tempo :: Duracao -> Float
tempo (Finita n) = n
tempo _ = 0


----------------------------------------------------------------------------------------------------------------------------------------


{- | a função `ativaInimigo` coloca o próximo inimigo a ser lançado por um portal em jogo. 


=== Exemplos de Uso:

* `portal1` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda1]}

* `onda1` = Onda {inimigosOnda = [inimigo1, inimigo2], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}

* `inimigo1` = Inimigo (3.0, 4.0) Norte 100 1 10 20 [Projetil Fogo (Finita 5)] (0,0) 0
* `inimigo2` = Inimigo (7.0, 5.0) Sul 80 1 15 30 [Projetil Gelo (Finita 3)] (0,0) 0
* `inimigo3` = Inimigo (10.0, 10.0) Este 50 1.5 20 40 [Projetil Resina Infinita] (0,0) 0

>>> ativaInimigo portal1 [inimigo3]
(Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}], cicloOnda = 5.0, tempoOnda = 5.0, entradaOnda = 0.0}]},
[Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0},Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}], posInicial = (0.5,0.5), tempoInimigo = 0.0}])

-}


ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo p li | length ondasp == 0 = (p,li)
                  | podelançar primeiraonda = (novoportal, proxi:li)
                  | otherwise = (p,li)

    where ondasp = ondasPortal p
          primeiraonda = head ondasp
          (proxi, novaonda) = removeInimigo primeiraonda
          novasOndas = if ondaVazia novaonda then tail ondasp else novaonda : tail ondasp
          novoportal = p {ondasPortal = novasOndas}



{- | a função `podelançar` verifica se a onda está ativa e se ainda tem inimigos para lançar. 

=== Exemplos de Uso:

* `onda1` = Onda {inimigosOnda = [inimigo1, inimigo2], cicloOnda = 5, tempoOnda = 0, entradaOnda = 0}
* `onda2` = Onda {inimigosOnda = [inimigo3], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}

>>> podelançar onda1
True

>>> podelançar onda2
False

-}

podelançar :: Onda -> Bool
podelançar o = entradaOnda o <= 0 && length (inimigosOnda o) > 0 && tempoOnda o <= 0



{- | a função `removeInimigo` remove o primeiro inimigo da onda ativa. 

=== Exemplos de Uso:

* `onda1` = Onda {inimigosOnda = [inimigo1, inimigo2], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}

>>> removeInimigo onda1
(Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (2.0,2.0), tempoInimigo = 0.0},
Onda {inimigosOnda = [Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (2.0,2.0), tempoInimigo = 0.0}], cicloOnda = 5.0, tempoOnda = 5.0, entradaOnda = 1.0})

-}

removeInimigo :: Onda -> (Inimigo, Onda)
removeInimigo o = let i = inimigosOnda o
                      primeiroi = head i
                      ondaAtualizada = o {inimigosOnda = tail i,
                                          tempoOnda = cicloOnda o}
                  in (primeiroi, ondaAtualizada)



{- | a função `ondaVazia` verifica se a onda está vazia, isto é se não tem inimigos. -} 

ondaVazia :: Onda -> Bool
ondaVazia o = length (inimigosOnda o) == 0


----------------------------------------------------------------------------------------------------------------------------------------