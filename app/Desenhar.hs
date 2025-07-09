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

-- | Desenha o portal com a porta no lado esquerdo (com terra do lado direito).

portalesq :: Posicao -> Picture -> Picture
portalesq (x, y) = translate (30 + x*60 - 910)      -- 60: medida de cada célula // -910: valor da translação do mapa // 30: valor, a meu ver, mais correto para ficar harmonioso.
                             (40 - y*60 + 450)      -- 60: medida de cada célula // 450: valor da translação do mapa // 40: valor, a meu ver, mais correto para ficar harmonioso.


-- | Desenha o portal com a porta no lado direito (com terra do lado esquerdo).

portaldir :: Posicao -> Picture -> Picture
portaldir (x, y) = translate (-30 + x*60 - 910)     -- 60: medida de cada célula // -910: valor da translação do mapa // -30: valor, a meu ver, mais correto para ficar harmonioso.
                             (40 - y*60 + 450)      -- 60: medida de cada célula // 450: valor da translação do mapa // 40: valor, a meu ver, mais correto para ficar harmonioso.


-- | Desenha a base (com terra do lado direito).

baseesq :: Posicao -> Picture -> Picture
baseesq (x, y) = translate (-30 + x*60 - 910)       -- 60: medida de cada célula // -910: valor da translação do mapa // -30: valor, a meu ver, mais correto para ficar harmonioso.
                           (- (y*60) + 450)         -- 60: medida de cada célula // 450: valor da translação do mapa .

-- | Desenha a base (com terra do lado esquerdo).

basedir :: Posicao -> Picture -> Picture
basedir (x, y) = translate (10 + x*60 - 910)        -- 60: medida de cada célula // -910: valor da translação do mapa // 10: valor, a meu ver, mais correto para ficar harmonioso.
                           (- (y*60) + 450)         -- 60: medida de cada célula // 450: valor da translação do mapa.


-- | Desenha um portal do jogo.

desenhaPortal :: Portal -> Base -> Picture -> Picture -> Picture
desenhaPortal portal base pDirImg pEsqImg   | fst posPortal < fst posBase = portaldir posPortal (scale 0.8 0.8 pDirImg)
                                            | otherwise = portalesq posPortal (scale 0.8 0.8 pEsqImg)

    where posPortal = posicaoPortal portal
          posBase = posicaoBase base


-- | Desenha todos os portais do jogo.

desenhaPortais :: [Portal] -> Base -> Picture -> Picture -> Picture
desenhaPortais portais base pDirImg pEsqImg = pictures $ map (\portal -> desenhaPortal portal base pDirImg pEsqImg) portais


-- | Desenha a base do jogo.

desenhaBase :: Jogo -> Picture -> Picture
desenhaBase jogo baseImg    | fst posBase < fst posPortal = baseesq posBase (scale 0.7 0.7 baseImg)   -- Base à esquerda
                            | otherwise = basedir posBase (scale 0.7 0.7 baseImg)                     -- Base à direita

    where posBase = posicaoBase (baseJogo jogo)
          posPortal = posicaoPortal (head (portaisJogo jogo))


-- | Combina as funções desenhaBase e desenhaPortais.

desenhaPortaleBase :: Jogo -> Picture -> Picture -> Picture -> Picture
desenhaPortaleBase j pDirImg pEsqImg baseImg = pictures [desenhaPortais portais base pDirImg pEsqImg, desenhaBase j baseImg]

      where portais = portaisJogo j
            base = baseJogo j


--------------------------------------------------------------------------------------------------------------------------------

-- LOJA

-- | Desenha o fundo da loja.

fundoLoja :: Picture
fundoLoja = pictures
            [ color black $ translate 645 0 $ rectangleSolid 610 960,                  -- Limite preto.
              color (greyN 0.7) $ translate 645 0 $ rectangleSolid 600 950,            -- Retângulo cinzento.

              pictures $ map (\(dx, dy) -> translate dx dy $ scale 1 1 $ color black $ text "LOJA")                     -- Letra mais grossa.
              [ (480, 330), (480, 331), (480, 329), (481, 331), (481, 329), (481, 330), (479, 331), (479, 329),         
                (479, 330), (480, 332), (480, 328), (482, 330), (482, 332), (482, 328), (478, 330), (478, 332), (478, 328) ] ]

-- | Desenha a torre de fogo1 para a loja.

tfogo1 :: Picture -> Picture
tfogo1 t = pictures  [ color white $ translate 460 180 $ rectangleSolid 150 170,                      -- Retângulo branco.                     
                      color black $ translate 460 180 $ rectangleWire 150 170,                        -- Limite preto.                          
                      translate 460 183 t ]

-- | Desenha a torre de fogo2 para a loja.

tfogo2 :: Picture -> Picture
tfogo2 t = pictures  [ color white $ translate 800 180 $ rectangleSolid 150 170,                      -- Retângulo branco.                     
                      color black $ translate 800 180 $ rectangleWire 150 170,                        -- Limite preto.                     
                      translate 800 183 t ]

-- | Desenha informações das torres de fogo para a loja.

tfogoinfo :: Color -> Picture
tfogoinfo cor = pictures
      [ pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "Torres")  [(580, 220), (580, 219), (581, 219)],
        pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "de")  [(605, 180), (605, 179), (606, 179)],
        pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "Fogo")  [(590, 140), (590, 139), (591, 139)],
        translate 435 70 $ scale 0.18 0.18 $ color black $ text "150$",
        translate 775 70 $ scale 0.18 0.18 $ color cor $ text "250$" ]

-- | Desenha a torre de gelo1 para a loja.

tgelo1 :: Picture -> Picture
tgelo1 t = pictures  [ color white $ translate 460 (-50) $ rectangleSolid 150 170,                    -- Retângulo branco.
                      color black $ translate 460 (-50) $ rectangleWire 150 170,                      -- Limite preto.
                      translate 460 (-47) t ]                                                         -- Torre gelo (imagem).

-- | Desenha a torre de gelo2 para a loja.

tgelo2 :: Picture -> Picture
tgelo2 t = pictures  [ color white $ translate 800 (-50) $ rectangleSolid 150 170,                    -- Retângulo branco.
                      color black $ translate 800 (-50) $ rectangleWire 150 170,                      -- Limite preto.
                      translate 800 (-47) t ]                                                         -- Torre gelo (imagem).