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

-- | Verifica se a célula de terra está rodeada por água (em cima e em baixo ou à esquerda e à direita).

terraComAgua :: Mapa -> Int -> Int -> (Bool, Bool)
terraComAgua mapa x y = (aguaCimaBaixo, aguaEsquerdaDireita)
  where
    aguaCimaBaixo = (y > 0 && (mapa !! (y - 1) !! x == Agua)) && (y < length mapa - 1 && (mapa !! (y + 1) !! x == Agua))
    aguaEsquerdaDireita = (x > 0 && (mapa !! y !! (x - 1) == Agua)) && (x < length (mapa !! y) - 1 && (mapa !! y !! (x + 1) == Agua))


-- | Verifica se a célula de água tem terra por cima ou à sua esquerda. 
--   Esta função serve apenas para criar um limite castanho escuro.

aguaComTerra :: Mapa -> Int -> Int -> (Bool, Bool)
aguaComTerra mapa x y = (terraCima, terraEsquerda)
  where
    terraCima = y > 0 && (mapa !! (y - 1) !! x == Terra)
    terraEsquerda = x > 0 && (mapa !! y !! (x - 1) == Terra)

-- | Verifica se a célula deve ter sombra nas bordas (cima, baixo, esquerda, direita).

sombra :: Mapa -> Int -> Int -> (Bool, Bool, Bool, Bool)
sombra mapa x y = (sombraCima mapa x y, sombraBaixo mapa x y, sombraEsquerda mapa x y, sombraDireita mapa x y)

  where sombraCima mapa x y = y > 0 && (mapa !! (y - 1) !! x == Relva)
        sombraBaixo mapa x y = y < length mapa - 1 && (mapa !! (y + 1) !! x == Relva)
        sombraEsquerda mapa x y = x > 0 && (mapa !! y !! (x - 1) == Relva)
        sombraDireita mapa x y = x < length (mapa !! y) - 1 && (mapa !! y !! (x + 1) == Relva)

-- | Desenha uma célula de água

aguaCelula :: Float -> Float -> (Bool, Bool, Bool, Bool) -> (Bool, Bool) -> Picture
aguaCelula x y (sCima, sBaixo, sEsquerda, sDireita) (terraCima, terraDireita)  = pictures (sombras ++ [centro] ++ limites)

  where centro = translate x y $ color corAgua $ rectangleSolid 60 60   -- agua 

        -- as sombras só são criadas se s... for True
        sombras = concat [  [translate x (y + 30) $ color verdeEscuro $ rectangleSolid 60 6 | sCima],
                            [translate x (y - 30) $ color verdeEscuro $ rectangleSolid 60 4 | sBaixo],
                            [translate (x + 30) y $ color verdeEscuro $ rectangleSolid 4 60 | sDireita],
                            [translate (x - 30) y $ color verdeEscuro $ rectangleSolid 6 60 | sEsquerda] ]

        -- Limites (borda) se houver terra no lado esquerdo ou acima
        limites = ([translate (x-29) y $ color castanhoEscuro $ rectangleSolid 2 67 | terraDireita]) ++
                  ([translate x (y + 29) $ color castanhoEscuro $ rectangleSolid 67 2 | terraCima])

-- | Desenha uma célula de terra.

terraCelula :: Float -> Float -> (Bool, Bool, Bool, Bool) -> (Bool, Bool) -> Picture
terraCelula x y (sCima, sBaixo, sEsquerda, sDireita) (aguaCimaBaixo, aguaEsquerdaDireita) = pictures (sombras ++ [centro] ++ [ponte])

  where centro = translate x y $ color corTerra $ rectangleSolid 60 60   -- Terra

        -- As sombras só são criadas se s... for True.
        sombras = concat [  [translate x (y + 30) $ color castanhoEscuro $ rectangleSolid 60 7 | sCima],
                            [translate x (y - 30) $ color castanhoEscuro $ rectangleSolid 60 4 | sBaixo],
                            [translate (x + 30) y $ color castanhoEscuro $ rectangleSolid 4 60 | sDireita],
                            [translate (x - 30) y $ color castanhoEscuro $ rectangleSolid 7 60 | sEsquerda] ]

        -- Ponte quando a terra está rodeada por água (em cima e em baixo ou à esquerda e à direita).
        ponte | aguaCimaBaixo = ponteDeMadeiraV x y
              | aguaEsquerdaDireita = ponteDeMadeiraH x y
              | otherwise = Blank

-- | Desenha uma célula com base no tipo de terreno.

desenhaCelula :: Mapa -> Terreno -> Int -> Int -> Picture
desenhaCelula mapa tipo x y 
  | tipo == Terra = terraCelula posX posY (sombra mapa x y) (terraComAgua mapa x y)
  | tipo == Agua = aguaCelula posX posY (sombra mapa x y) (aguaComTerra mapa x y)
  | otherwise = Blank

  where
    posX = fromIntegral x * 60
    posY = fromIntegral (-y) * 60  -- O y é negativo, pois as celúlas são desenhadas para baixo e o referencial no Gloss tem a origem no centro da tela.

-- | Desenha o mapa inteiro.

desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = translate (-910) 450 $
                   pictures [desenhaCelula mapa celula x y | (y, linha) <- zip [0..] mapa, (x, celula) <- zip [0..] linha]

                
-------------------------------------------------------------------------------------------------------------------------------------

-- PORTAL E BASE
