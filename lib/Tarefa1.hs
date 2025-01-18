{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Diogo Matos Azevedo <a109727@alunos.uminho.pt>
              Vera da Silva Almeida <a110723@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.

O objetivo desta tarefa é a implementação de funções que verifiquem se um estado de jogo é válido, ou seja, se cumpre com todos os requisitos do enunciado.
-}
module Tarefa1 where

import LI12425
import Data.List

-------------------------------------------------------------------------------------------------------------------------------------------------------------

-- PORTAIS 

{- | a função `verificaPortalNumero` verifica o número de portais existentes, neste caso, se existe pelo menos um. -}

verificaPortalNumero :: Jogo -> Bool
verificaPortalNumero jogo = length (portaisJogo jogo) > 0



{- | a função `verificaPosiPortal` verifica se as posições de todos os portais estão sobre Terra, utilizando a função `verificaPosiPortal2` (é a mesma lógica que a `verificaBaseTerra`, mas para os portais). -}

verificaPosiPortal :: Jogo -> Bool
verificaPosiPortal j = let mapa = mapaJogo j
                       in all (verificaPosiPortal2 mapa) (portaisJogo j)


{- | a função `verificaPosiPortal2` verifica se a posição de um Portal coincide com a posição de um terreno de Terra. -}

verificaPosiPortal2 :: Mapa -> Portal -> Bool
verificaPosiPortal2 mapa portal = let (coluna, linha) = posicaoPortal portal
                                      colunaNova = floor coluna
                                      linhaNova = floor linha

                                  in linhaNova >= 0 && linhaNova < length mapa && mapa !! linhaNova !! colunaNova == Terra



{- | a função `verificaCaminhoTerra` verifica se existe no jogo pelo menos um caminho de terra ligando um portal à base, utilizando a função `existeCaminho`.

=== Exemplo de Uso:

* `jogo` = Jogo {baseJogo = base, portaisJogo = [portal], torresJogo = [], mapaJogo = mapa, inimigosJogo = [], lojaJogo = [], nivelJogo = Um}

* `base` = Base {vidaBase = 0, posicaoBase = (5.3, 4.2), creditosBase = 50}
* `portal` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = []}
* `mapa` = [ [t, t, r, a, a, a], [r, t, r, a, r, r], [r, t, r, a, r, t], [r, t, r, a, r, t], [r, t, t, t, t, t], [a, a, a, a, r, r] ]
 
>>> verificaCaminhoTerra jogo 
True

-}

verificaCaminhoTerra :: Jogo -> Bool
verificaCaminhoTerra jogo = let mapa = mapaJogo jogo
                                base = arredondaPosicao (posicaoBase (baseJogo jogo))
                                portais = map arredondaPosicao (map posicaoPortal (portaisJogo jogo))

                            in existeCaminho portais mapa base


{- | a função `arredondaPosicao` arredonda por defeito uma posicao para inteiros, utilizando o floor. -}

arredondaPosicao :: Posicao -> (Int, Int)
arredondaPosicao (x, y) = (floor x, floor y)


{- | a função `existeCaminho` verifica se existe um caminho de terra entre os portais e a base. -}

existeCaminho :: [(Int, Int)] -> Mapa -> (Int, Int) -> Bool
existeCaminho [] _ _ = False -- Se a lista de portais estiver vazia, não existe caminho.
existeCaminho (portal:resto) mapa base = caminhoPortalBase portal base mapa || existeCaminho resto mapa base


{- | a função `caminhoPortalBase` verifica se existe um caminho de terra entre duas posições no mapa, utilizando a `procuraCaminho`. -}

caminhoPortalBase :: (Int, Int) -> (Int, Int) -> Mapa -> Bool
caminhoPortalBase portal = procuraCaminho [portal] []


{- | a função `procuraCaminho` verifica se é possivel encontrar um caminho de terra. -}

procuraCaminho :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Mapa -> Bool
procuraCaminho [] _ _ _ = False
procuraCaminho (pos:resto) visitados base mapa | pos == base = True
                                               | otherwise = let vizinhos = encontraVizinhos pos mapa                                   -- Encontra os vizinhos de uma determinada posição.
                                                                 naoVisitados = removeVisitados vizinhos visitados                      -- Remove os vizinhos que já foram visitados.
                                                             in procuraCaminho (resto ++ naoVisitados) (pos : visitados) base mapa      


{- | a função `encontraVizinhos` encontra os vizinhos de uma posição, considerando apenas as posições de Terra, utilizando a função `filtraTerras`. 

=== Exemplo de Uso:

* `mapa` = [ [t, t, r, a, a, a], [r, t, r, a, r, r], [r, t, r, a, r, t], [r, t, r, a, r, t], [r, t, t, t, t, t], [a, a, a, a, r, r] ] 

>>> encontraVizinhos (1, 1) mapa
[(1,2),(1,0)]

-}

encontraVizinhos :: (Int, Int) -> Mapa -> [(Int, Int)]
encontraVizinhos (x, y) mapa = let vizinhos = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
                               in filtraTerras vizinhos mapa


{- | a função `filtraTerras` filtra as posições que são de Terra. -}

filtraTerras :: [(Int, Int)] -> Mapa -> [(Int, Int)]
filtraTerras [] _ = []
filtraTerras ((x, y):resto) mapa | existeTerra (x, y) mapa = (x, y) : filtraTerras resto mapa
                                 | otherwise = filtraTerras resto mapa


{- | a função `existeTerra` verifica se uma posição é de Terra. -}

existeTerra :: (Int, Int) -> Mapa -> Bool
existeTerra (x, y) mapa = x >= 0 && y >= 0 && y < length mapa && x < length (mapa !! y) && (mapa !! y !! x == Terra)


{- | a função `removeVisitados` remove as posições já visitadas da lista de vizinhos. -}

removeVisitados :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
removeVisitados [] _ = []
removeVisitados (pos:resto) visitados | pos `elem` visitados = removeVisitados resto visitados
                                      | otherwise = pos : removeVisitados resto visitados




{- | a função `verificaPortalSobreposicao` verifica se nenhum portal está sobreposto à base ou a alguma torre definida no jogo (é a mesma lógica que a `verificaBaseSobreposicao`, mas para os portais). -}

verificaPortalSobreposicao :: Jogo -> Bool
verificaPortalSobreposicao j = all verificaPortalSobreposicao2 portais

    where portais = map floorPosicao (map posicaoPortal (portaisJogo j))
          base = floorPosicao (posicaoBase (baseJogo j))
          torres = map floorPosicao (map posicaoTorre (torresJogo j))
          floorPosicao (x, y) = (fromIntegral (floor x), fromIntegral (floor y))

          verificaPortalSobreposicao2 portal = (portal /= base) && notElem portal torres



{- | a função `existeOndaPortal` verifica se no máximo há uma onda ativa por portal no jogo. 

=== Exemplos de uso:

* `portal1` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda1, onda2]}
* `portal2` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda3, onda2]}
* `onda1` = Onda {inimigosOnda = [], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}
* `onda2` = Onda {inimigosOnda = [], cicloOnda = 5, tempoOnda = 0, entradaOnda = 0}
* `onda3` = Onda {inimigosOnda = [], cicloOnda = 5, tempoOnda = 0, entradaOnda = 0}

* `jogo1` = Jogo {baseJogo = Base 100 (3,3) 50, torresJogo = [], portaisJogo = [portal1], mapaJogo = [], inimigosJogo = [], lojaJogo = [], nivelJogo = Um}
* `jogo2` = Jogo {baseJogo = Base 100 (3,3) 50, torresJogo = [], portaisJogo = [portal2], mapaJogo = [], inimigosJogo = [], lojaJogo = [], nivelJogo = Um}

>>> verificaOndaPortal jogo1
True

>>> verificaOndaPortal jogo2
False

-}

verificaOndaPortal :: Jogo -> Bool
verificaOndaPortal j = all verificaOndaPortal2 portais

    where portais = portaisJogo j

          verificaOndaPortal2 portal = length (verificaOndasAtivas (ondasPortal portal)) <= 1       -- o número de ondas ativas de um portal tem de ser <= 1

          verificaOndasAtivasFiltro onda = entradaOnda onda <= 0
          verificaOndasAtivas = filter verificaOndasAtivasFiltro                                    -- devolve uma lista com as ondas ativas de um portall



-------------------------------------------------------------------------------------------------------------------------------------------------------


-- INIMIGOS


{- | a função `verificaInimigosPorLancar` verifica se todos os inimigos por lançar têm a posição do respetivo portal, nível de vida positivo e lista de projéteis ativos vazia, 
     utilizando as funcoes `verificaPortal`, `verificaOnda` e `verificaInimigo`. 


=== Exemplo de Uso:

* `jogo1` = Jogo {baseJogo = base, portaisJogo = [portal1], torresJogo = [], mapaJogo = mapa, inimigosJogo = [], lojaJogo = [], nivelJogo = Um}
* `jogo2` = Jogo {baseJogo = base, portaisJogo = [portal2], torresJogo = [], mapaJogo = mapa, inimigosJogo = [], lojaJogo = [], nivelJogo = Um}
* `jogo3` = Jogo {baseJogo = base, portaisJogo = [portal3], torresJogo = [], mapaJogo = mapa, inimigosJogo = [], lojaJogo = [], nivelJogo = Um}

* `portal1` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda1]}
* `portal2` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda2]}
* `portal3` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda3]}
* `onda1` = Onda {inimigosOnda = [Inimigo (0.5, 0.5) Norte 100.0 1.0 10.0 20 [] (0.5, 0.5) 0], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}
* `onda2` = Onda {inimigosOnda = [Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (2.0, 2.0) 0], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}
* `onda3` = Onda {inimigosOnda = [Inimigo (0.5, 0.5) Norte (-100) 1.0 10.0 20 [] (0.5, 0.5) 0], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}


>>> verificaInimigosPorLancar jogo1
True

>>> verificaInimigosPorLancar jogo2
False

>>> verificaInimigosPorLancar jogo3
False

-}

verificaInimigosPorLancar :: Jogo -> Bool
verificaInimigosPorLancar j = let portais = portaisJogo j
                              in all verificaPortal portais


{- | a função `verificaPortal` verifica se todos os inimigos de um portal cumprem com as condições de serem lançados. -}

verificaPortal :: Portal -> Bool
verificaPortal portal = let posPortal = posicaoPortal portal
                            ondas = ondasPortal portal
                        in all (verificaOnda posPortal) ondas


{- | a função `verificaOnda` verifica se todos os inimigos de uma onda cumprem com as condições de serem lançados.  -}

verificaOnda :: Posicao -> Onda -> Bool
verificaOnda posPortal onda = let inimigos = inimigosOnda onda
                              in all (verificaInimigo posPortal) inimigos


{- | a função `verificaInimigo` se um determinado inimigo cumpre com as condições de ser lançado, ou seja, vida positiva, posição igual à do portal e sem projéteis ativos. -}

verificaInimigo :: Posicao -> Inimigo -> Bool
verificaInimigo posPortal inimigo = posicaoInimigo inimigo == posPortal && vidaInimigo inimigo > 0 && length (projeteisInimigo inimigo) == 0




{- | a função `verificaInimigosTerra` verifica se todos os inimigos estão sobre Terra no mapa, utilizando a função `verificaInimigosTerra2`. 

=== Exemplo de Uso:

* `jogo1` = Jogo {baseJogo = base, portaisJogo = [], torresJogo = [], mapaJogo = mapa, inimigosJogo = [Inimigo (2,0) Sul 80.0 1.0 15.0 30 [] (0, 0) 0], lojaJogo = [], nivelJogo = Um}
* `jogo2` = Jogo {baseJogo = base, portaisJogo = [], torresJogo = [], mapaJogo = mapa, inimigosJogo = [Inimigo (1, 1) Sul 80.0 1.0 15.0 30 [] (0, 0) 0], lojaJogo = [], nivelJogo = Um}

* `mapa` = [ [t, t, r, a, a, a], [r, t, r, a, r, r], [r, t, r, a, r, t], [r, t, r, a, r, t], [r, t, t, t, t, t], [a, a, a, a, r, r] ]
 
>>> verificaInimigosTerra jogo1
False

>>> verificaInimigosTerra jogo2
True

-}

verificaInimigosTerra :: Jogo -> Bool
verificaInimigosTerra j = let mapa = mapaJogo j
                          in all (verificaInimigosTerra2 mapa) (inimigosJogo j)


{- | a função `verificaInimigosTerra2` verifica se a posição de um inimigo está sobre Terra. -}

verificaInimigosTerra2 :: Mapa -> Inimigo -> Bool
verificaInimigosTerra2 mapa inimigo = let (coluna, linha) = posicaoInimigo inimigo

                                      in floor linha >= 0 && floor linha < length mapa && floor coluna >= 0 && floor coluna < length (mapa !! floor linha) && (mapa !! floor linha) !! floor coluna == Terra || 
                                         ceiling linha >= 0 && ceiling linha < length mapa && floor coluna >= 0 && floor coluna < length (mapa !! ceiling linha) && (mapa !! ceiling linha) !! floor coluna == Terra ||
                                         floor linha >= 0 && floor linha < length mapa && ceiling coluna >= 0 && ceiling coluna < length (mapa !! floor linha) && (mapa !! floor linha) !! ceiling coluna == Terra || 
                                         ceiling linha >= 0 && ceiling linha < length mapa && ceiling coluna >= 0 && ceiling coluna < length (mapa !! ceiling linha) && (mapa !! ceiling linha) !! ceiling coluna == Terra 


{- | a função `verificaInimigosSobreposicao` verifica se todos os inimigos em jogo não estão sobrepostos a uma torre, utilizando a funcao `verificaInimigosSobreposicao2`. 

=== Exemplo de Uso:

* `jogo1` = Jogo {baseJogo = base, portaisJogo = [], torresJogo = [Torre (1, 1) 25 3 3 2 0 (Projetil Fogo (Finita 7))], mapaJogo = [], inimigosJogo = [Inimigo (2,0) Sul 80.0 1.0 15.0 30 [] (0, 0) 0], lojaJogo = [], nivelJogo = Um}
* `jogo2` = Jogo {baseJogo = base, portaisJogo = [], torresJogo = [Torre (1, 1) 25 3 3 2 0 (Projetil Fogo (Finita 7))], mapaJogo = [], inimigosJogo = [Inimigo (1, 1) Sul 80.0 1.0 15.0 30 [] (0, 0) 0], lojaJogo = [], nivelJogo = Um}

>>> verificaInimigosTerra jogo1
True

>>> verificaInimigosTerra jogo2
False

-}

verificaInimigosSobreposicao :: Jogo -> Bool
verificaInimigosSobreposicao j = let inimigos = map posicaoInimigo (inimigosJogo j)
                                     torres = map posicaoTorre (torresJogo j)
                                 in verificaInimigosSobreposicao2 inimigos torres


{- | a função `verificaInimigosSobreposicao2` verifica se há sobreposição entre inimigos e torres. -}

verificaInimigosSobreposicao2 :: [Posicao] -> [Posicao] -> Bool
verificaInimigosSobreposicao2 [] _ = True
verificaInimigosSobreposicao2 (i:is) torres | i `elem` torres = False                               -- Se o inimigo está em cima de uma torre há sobreposição.
                                            | otherwise = verificaInimigosSobreposicao2 is torres   -- Recursividade de modo a continuar a procurar possível sobreposição.




{- | a função `verificaVelocidadeInimigo` verifica se a velocidade de todos os inimigos é maior ou igual a 0, utilizando a função `verificaVelocidadeInimigo2`. 


=== Exemplo de Uso:

* `jogo1` = Jogo {baseJogo = base, portaisJogo = [], torresJogo = [], mapaJogo = [], inimigosJogo = [Inimigo (2,0) Sul 80.0 (-2) 15.0 30 [] (0, 0) 0], lojaJogo = [], nivelJogo = Um}
* `jogo2` = Jogo {baseJogo = base, portaisJogo = [], torresJogo = [], mapaJogo = [], inimigosJogo = [Inimigo (1,1) Sul 80.0 1.5 15.0 30 [] (0, 0) 0], lojaJogo = [], nivelJogo = Um}

>>> verificaInimigosTerra jogo1
False

>>> verificaInimigosTerra jogo2
True

-}

verificaVelocidadeInimigo :: Jogo -> Bool
verificaVelocidadeInimigo j =  all verificaVelocidadeInimigo2 (inimigosJogo j)


{- | a função `verificaVelocidadeInimigo2` verifica se a velocidade de tde um inimigo é válida (>= 0). -}

verificaVelocidadeInimigo2 :: Inimigo -> Bool
verificaVelocidadeInimigo2 inimigo = velocidadeInimigo inimigo >= 0




{- | a função `verificaProjeteisAtivos` verifica se os projéteis de todos os inimigos estão normalizados no jogo, utilizando as funcoes `verificaProjetilAtivos`, `verificaProjetil`, `verificaSemDuplicados`, `verificaSemIncompativeis`. -}

verificaProjeteisAtivos :: Jogo -> Bool
verificaProjeteisAtivos jogo = all verificaProjetilAtivos (inimigosJogo jogo)




{- | a função `verificaProjetilAtivos` verifica se os projéteis de um inimigo estão normalizados. 

=== Exemplo de Uso:

>>> verificaProjetilAtivos (Inimigo (1, 0) Norte 100.0 1.0 10.0 20 [] (0, 0) 0)
True

>>> verificaProjetilAtivos (Inimigo (1, 0) Norte 100.0 1.0 10.0 20 [Projetil Fogo (Finita 5)] (0, 0) 0)
True

>>> verificaProjetilAtivos (Inimigo (1, 0) Norte 100.0 1.0 10.0 20 [Projetil Fogo (Finita 5), Projetil Fogo (Finita 3)] (0, 0) 0)
False

-}

verificaProjetilAtivos :: Inimigo -> Bool
verificaProjetilAtivos inimigo = verificaProjetil (projeteisInimigo inimigo)


{- | a função `verificaProjetil` verifica se se uma lista de projéteis está normalizada. -}

verificaProjetil :: [Projetil] -> Bool
verificaProjetil projeteis =  let tiposAtuais = map tipoProjetil projeteis
                                  semDuplicados = verificaSemDuplicados tiposAtuais
                                  semIncompativeis = verificaSemIncompativeis tiposAtuais

                              in semDuplicados && semIncompativeis




{- | a função `verificaSemDuplicados` verifica se não há projéteis duplicados. 

=== Exemplo de Uso:

>>> verificaSemDuplicados [Fogo, Fogo]       
False 

>>> verificaSemDuplicados [Fogo, Resina]   
True 

-}

verificaSemDuplicados :: [TipoProjetil] -> Bool
verificaSemDuplicados tipos = length tipos == length (nub tipos) -- 'nub' é uma função que faz parte da biblioteca 'Data.List' importada no ínicio que remove duplicados de uma lista.



{- | a função `verificaSemIncompativeis` verifica se não há projéteis incompatíveis. 

=== Exemplo de Uso:

>>> verificaSemIncompativeis [Fogo, Gelo]       
False

>>> verificaSemIncompativeis [Fogo, Resina]    
False

>>> verificaSemIncompativeis [Gelo, Resina]   
True 

-}

verificaSemIncompativeis :: [TipoProjetil] -> Bool
verificaSemIncompativeis tipos = not (elem Fogo tipos && elem Resina tipos) && not (elem Fogo tipos && elem Gelo tipos)
                                 -- Verifica se não há projéteis incompatíveis entre si, neste caso, Fogo e Resina e Fogo e Gelo.



-----------------------------------------------------------------------------------------------------------------------------------
