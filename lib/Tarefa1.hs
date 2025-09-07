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

{- | a função `validaJogo` verifica se um jogo é válido, ou seja, se cumpre com todos os requisitos, neste caso, se todas as funções desenvolvidas dão como resultado 'True'. -}

validaJogo :: Jogo -> Bool
validaJogo j = verificaPortalNumero j &&
              verificaPosiPortal j &&
              verificaCaminhoTerra j &&
              verificaPortalSobreposicao j &&
              verificaOndaPortal j &&
              verificaInimigosPorLancar j &&
              verificaInimigosTerra j &&
              verificaInimigosSobreposicao j &&
              verificaVelocidadeInimigo j &&
              verificaProjeteisAtivos j &&
              verificaPosiTorre j &&
              verificaAlcanceTorre j &&
              verificaRajadaTorre j &&
              verificaCicloTorre j &&
              verificaNotSobreposicaoTorre j &&
              verificaBaseTerra j &&
              verificaBaseCredito j &&
              verificaBaseSobreposicao j


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
