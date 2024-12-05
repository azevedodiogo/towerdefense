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
