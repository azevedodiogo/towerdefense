module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

-- MAPA

-- CORES

-- | Cor da Terra
corTerra :: Color
corTerra = makeColor (169 / 255) (122 / 255) (61 / 255) 1.0

-- | Cor da Água
corAgua :: Color
corAgua = makeColor (98/255) (189/255) (203/255) 1.0

--(141/255) (222/255) (235/255) 1.0

-- | Cor para a sombra da terra
castanhoEscuro :: Color
castanhoEscuro = makeColor (110 / 255) (80 / 255) (40 / 255) 1.0

-- | Cor para a sombra da relva
verdeEscuro :: Color
verdeEscuro = makeColor (53/255) (121/255) (51/255) 1.0

-- | Desenha o fundo de relva

fundoRelva :: Int -> Int -> Picture -> Picture
fundoRelva largura altura img = translate (-910) 450 $
                                pictures [translate (fromIntegral x * 60) (fromIntegral (-y) * 60) $ scale 0.1 0.1 img | x <- [0..(largura-1)], y <- [0..(altura-1)]]
