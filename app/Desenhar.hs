module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

-- MAPA

-- CORES

-- | Cor da Terra.
corTerra :: Color
corTerra = makeColor (169 / 255) (122 / 255) (61 / 255) 1.0

-- | Cor da Água.
corAgua :: Color
corAgua = makeColor (98/255) (189/255) (203/255) 1.0

--(141/255) (222/255) (235/255) 1.0

-- | Cor para a sombra da terra.
castanhoEscuro :: Color
castanhoEscuro = makeColor (110 / 255) (80 / 255) (40 / 255) 1.0

-- | Cor para a sombra da relva.
verdeEscuro :: Color
verdeEscuro = makeColor (53/255) (121/255) (51/255) 1.0

-- | Desenha o fundo de relva.

fundoRelva :: Int -> Int -> Picture -> Picture
fundoRelva largura altura img = translate (-910) 450 $
                                pictures [translate (fromIntegral x * 60) (fromIntegral (-y) * 60) $ scale 0.1 0.1 img | x <- [0..(largura-1)], y <- [0..(altura-1)]]


-- PONTES - As pontes são posições de terra no mapa, mas desenhadas com uma cor diferente para distinguir.

-- | Desenha uma ponte de madeira horizontal.

ponteDeMadeiraH :: Float -> Float -> Picture
ponteDeMadeiraH x y = pictures (ponte ++ agua)

    where ponte = [translate x y $ color castanhoEscuro $ rectangleSolid 66 66]

          -- (3 retângulos com 15px cada e 7.5px de espaçamento)
          agua = [ translate x (y + 18.75) $ color corAgua $ rectangleSolid 60 7.5,       -- Tábua 1
                   translate x y $ color corAgua $ rectangleSolid 60 7.5,                 -- Tábua 2
                   translate x (y - 18.75) $ color corAgua $ rectangleSolid 60 7.5  ]     -- Tábua 3

-- | Desenha uma ponte de madeira vertical.

ponteDeMadeiraV :: Float -> Float -> Picture
ponteDeMadeiraV x y = pictures (ponte ++ agua)

    where ponte = [translate x y $ color castanhoEscuro $ rectangleSolid 60 66]

          -- Acrescenta a água para dar o efeito de tábuas de madeira.
          agua = [ translate (x - 18.75) y $ color corAgua $ rectangleSolid 7.5 60,       -- Tábua 1
                   translate x y $ color corAgua $ rectangleSolid 7.5 60,                 -- Tábua 2
                   translate (x + 18.75) y $ color corAgua $ rectangleSolid 7.5 60  ]     -- Tábua 3

-- | Verifica se a célula de terra está rodeada por água (em cima e em baixo ou à esquerda e à direita)

terraComAgua :: Mapa -> Int -> Int -> (Bool, Bool)
terraComAgua mapa x y = (aguaCimaBaixo, aguaEsquerdaDireita)
  where
    aguaCimaBaixo = (y > 0 && (mapa !! (y - 1) !! x == Agua)) && (y < length mapa - 1 && (mapa !! (y + 1) !! x == Agua))
    aguaEsquerdaDireita = (x > 0 && (mapa !! y !! (x - 1) == Agua)) && (x < length (mapa !! y) - 1 && (mapa !! y !! (x + 1) == Agua))
