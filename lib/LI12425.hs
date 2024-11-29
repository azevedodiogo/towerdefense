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
  = -- | Torres constroem-se sobre o relvado do mapa.
    Relva
  | -- | A base e os portais constroem-se sobre caminhos de terra do mapa. Além disso, inimigos movem-se sobre estes terrenos.
    Terra
  | -- | Água para efeito decorativo, mas onde não se pode construir, nem os inimigos se podem mover.
    Agua
  deriving (Eq, Show)

-- | Mapa do jogo composto por uma matriz de terrenos.
type Mapa = [[Terreno]]

-- | Coordenada bilateral de uma entidade no jogo, representante do seu centro.
-- O referencial tem origem no canto superior esquerdo, com eixos x e y positivos para a direita e para baixo, respectivamente.