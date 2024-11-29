{-|
Module      : LI12425
Description : Definições base do jogo
Copyright   : Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco   <omp@di.uminho.pt>
              Pedro Peixoto  <d14110@di.uminho.pt>
              Xavier Pinho   <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2024/25.
-}
module LI12425 (
    -- * Tipos de dados
    -- ** Básicos
    Creditos, Direcao(..), Distancia, Duracao(..), Posicao, Semente, Tempo,
    -- ** Mapas
    Mapa, Terreno(..),
    -- ** Entidades
    Base(..), Torre(..), Portal(..), Inimigo(..), TipoProjetil(..), Projetil(..),
    -- ** Jogo
    Jogo(..), Onda(..), Loja,
    -- * Funções auxiliares
    geraAleatorios,
    NivelJogo(..),
    ) where

import System.Random (mkStdGen, randoms)

-- | Tipo de terrenno do mapa.
data Terreno