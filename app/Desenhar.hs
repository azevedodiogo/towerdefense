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

-- | Desenha informações das torres de gelo para a loja.

tgeloinfo :: Color -> Picture
tgeloinfo cor = pictures
      [ pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "Torres")  [(580, -10), (580, -9), (581, -9)],
        pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "de")  [(605, -50), (605, -49), (606, -49)],
        pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "Gelo")  [(595, -90), (595, -89), (596, -89)],
        translate 435 (-160) $ scale 0.18 0.18 $ color black $ text "140$",
        translate 775 (-160) $ scale 0.18 0.18 $ color cor $ text "240$" ]

-- | Desenha a torre de resina1 para a loja.

tresina1 :: Picture -> Picture
tresina1 t = pictures  [ color white $ translate 460 (-280) $ rectangleSolid 150 170,                   -- Retângulo branco.
                        color black $ translate 460 (-280) $ rectangleWire 150 170,                     -- Limite preto.
                        translate 460 (-277) t ]                                                        -- Torre resina (imagem).

-- | Desenha a torre de resina2 para a loja

tresina2 :: Picture -> Picture
tresina2 t = pictures  [ color white $ translate 800 (-280) $ rectangleSolid 150 170,                   -- Retângulo branco.
                        color black $ translate 800 (-280) $ rectangleWire 150 170,                     -- Limite preto.
                        translate 800 (-277) t ]                                                        -- Torre resina (imagem).

-- | Desenha informações das torres de resina para a loja.

tresinainfo :: Color -> Picture
tresinainfo cor = pictures
      [ pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "Torres")  [(580, -240), (580, -239), (581, -239)],
        pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "de")  [(605, -280), (605, -279), (606, -279)],
        pictures $ map (\(dx, dy) -> translate dx dy $ scale 0.25 0.25 $ color black $ text "Resina")  [(579, -320), (579, -319), (580, -319)],
        translate 435 (-390) $ scale 0.18 0.18 $ color black $ text "120$",
        translate 775 (-390) $ scale 0.18 0.18 $ color cor $ text "220$" ]

-- | Desenha um cadeado (para simbolizar que aquela torre ainda está bloqueada).

bloqueado :: Picture -> Posicao -> Picture
bloqueado block (x,y) = pictures [ color white $ translate x y $ rectangleSolid 150 170,               -- Retângulo branco.
                                  color black $ translate x y $ rectangleWire 150 170,                -- Limite preto.
                                  translate x (y+3) block ]                                         -- Cadeado (imagem).

-- | Junta as torres.

torresLoja :: Jogo -> [Picture] -> Picture
torresLoja jogo [tf1, tg1, tr1, tf2, tg2, tr2, block]

      | length (lojaJogo jogo) == 3 = pictures [tfogo1 tf1, tfogoinfo (greyN 0.7), tgelo1 tg1, tgeloinfo (greyN 0.7), tresina1 tr1, tresinainfo (greyN 0.7), bloqueado block (800, 180), bloqueado block (800, -50), bloqueado block (800, -280)]
      | length (lojaJogo jogo) == 4 = pictures [tfogo1 tf1, tfogo2 tf2, tfogoinfo black, tgelo1 tg1, tgeloinfo (greyN 0.7), tresina1 tr1, tresinainfo (greyN 0.7), bloqueado block (800, -50), bloqueado block (800, -280)]
      | length (lojaJogo jogo) == 6 = pictures [tfogo1 tf1, tfogo2 tf2, tfogoinfo black, tgelo1 tg1, tgelo2 tg2, tgeloinfo black, tresina1 tr1, tresina2 tr2, tresinainfo black]
      | otherwise = Blank

-- | Exibe no ecrã a vida da base (que é atualizada ao longo do tempo).

vida :: Jogo -> Picture
vida j = pictures $ map (\(x, y) -> translate x y $ scale 0.25 0.25 $ color black $ text ("Vida: " ++ show (vidaBase (baseJogo j))))
         [(705, -450), (705, -449), (706, -449)]         -- Para a letra ficar mais grossa.

-- | Exibe no ecrã os creditos da base.

creditosLoja :: Jogo -> Picture
creditosLoja j = pictures $ map (\(x, y) -> translate x y $ scale 0.25 0.25 $ color black $ text ("Creditos: " ++ show (creditosBase (baseJogo j))))
             [(390, -450), (390, -449), (391, -449)]       -- Para a letra ficar mais grossa.

----------------------------------------------------------------------------------------------------------------------------------------


-- TORRES (para o mapa)


-- De modo a comprar torres, um círculo amarelo aparecerá no canto superior esquerdo e é movimentando este que o jogador escolhe o sítio onde irá colocar a torre.


-- | Desenha ou não a referência.

desenhaReferencia :: Compra -> (Int, Int) -> Picture
desenhaReferencia c (x, y) = case c of

      QuerComprar -> translate newx newy (color yellow (circleSolid 10))            -- QuerComprar - significa que o jogador carregou na tecla 'c' (ref. fica amarela).
      NaoQuerComprar -> translate newx newy (color verdeEscuro (circleSolid 10))    -- NaoQuerComprar - significa que o jogador não quer comprar (ref. desaparece).
      NaoTemDinheiro -> translate newx newy (color vermelho (circleSolid 10))       -- NaoTemDinheiro - significa que o jogador não tem dinheiro para comprar a torre (ref. fica vermelha).

      where newx = fromIntegral x * 60 - 910
            newy = fromIntegral (-y) * 60 + 450
            vermelho = makeColor (165/255) (38/255) (38/255) 1.0


-- | Desenha a torre que o jogador comprou, tendo em conta o número (1,2,3 respetivamente).

desenhaTorres ::[Picture] -> [Torre] -> Picture
desenhaTorres [tf1, tg1, tr1, tf2, tg2, tr2, block] torres = pictures


      [ case projetilTorre of
                              Projetil {tipoProjetil = Fogo} -> if danoTorre == 10 then translate (x * 60 - 911) ((- y) * 60 + 473) $ scale 0.55 0.55 tf1
                                                                else translate (x * 60 - 911) ((- y) * 60 + 473) $ scale 0.55 0.55 tf2

                              Projetil {tipoProjetil = Gelo} -> if danoTorre == 12 then translate (x * 60 - 911) ((- y) * 60 + 473) $ scale 0.55 0.55 tg1
                                                                else translate (x * 60 - 911) ((- y) * 60 + 473) $ scale 0.55 0.55 tg2

                              Projetil {tipoProjetil = Resina} -> if danoTorre == 8 then translate (x * 60 - 911) ((- y) * 60 + 473) $ scale 0.55 0.55 tr1
                                                                  else translate (x * 60 - 911) ((- y) * 60 + 473) $ scale 0.55 0.55 tr2

    | Torre (x, y) danoTorre _ _ _ _ projetilTorre <- torres ]

--------------------------------------------------------------------------------------------------------------------------------------

-- INIMIGOS

-- | Verifica se o inimigo tem gelo.

temGelo :: Inimigo -> Bool
temGelo ini = let tpProjeteis = map tipoProjetil (projeteisInimigo ini)
              in elem Gelo tpProjeteis

-- | Verifica se o inimigo tem resina.

temResina :: Inimigo -> Bool
temResina ini = let tpProjeteis = map tipoProjetil (projeteisInimigo ini)
                in elem Resina tpProjeteis


-- | Calcula o índice das imagens com base no tempo, na velocidade e nos projéteis.

calculaIndice :: Inimigo -> [Picture] -> Int
calculaIndice ini imgs  | temGelo ini = 1                                                                           -- Se o inimigo tem gelo, exibe uma imagem fixa.
                        | temResina ini = floor (tempoInimigo ini * 4 * velocidadeInimigo ini * 0.7) `mod` 4        -- Se o inimigo tem resina, as imgs são mais lentas a mudar.
                        | otherwise =  floor (tempoInimigo ini * 4 * velocidadeInimigo ini) `mod` 4                 -- Caso contrário, a animação é de acordo com a velocidade.


-- | Calcula a posição no mapa do inimigo.

calculaPos :: Inimigo -> Posicao
calculaPos ini = (x * 60 - 910, -(y * 60) + 470)

      where (x,y) = posicaoInimigo ini


-- | Desenha o inimigo para virado para este.

iniEste :: Inimigo -> [Picture] -> Picture
iniEste ini imgs = translate x' y' $ scale 1.05 1.05 (imgsEste !! calculaIndice ini imgsEste)

      where imgsEste = take 4 imgs
            (x', y') = calculaPos ini


-- | Desenha o inimigo para virado para oeste.

iniOeste :: Inimigo -> [Picture] -> Picture
iniOeste ini imgs = translate x' y' $ scale 1.05 1.05 (imgsOeste !! calculaIndice ini imgsOeste)

      where imgsOeste = take 4 (drop 4 imgs)
            (x', y') = calculaPos ini
            

-- | Desenha o inimigo para virado para norte.

iniNorte :: Inimigo -> [Picture] -> Picture
iniNorte ini imgs = translate x' y' $ scale 1.05 1.05 (imgsNorte !! calculaIndice ini imgsNorte)

      where imgsNorte = take 4 (drop 8 imgs)
            (x', y') = calculaPos ini
            

-- | Desenha o inimigo para virado para sul.

iniSul :: Inimigo -> [Picture] -> Picture
iniSul ini imgs = translate x' y' $ scale 1.05 1.05 (imgsSul !! calculaIndice ini imgsSul)

      where imgsSul = drop 12 imgs
            (x', y') = calculaPos ini
            
            
-- | Desenha o inimigo com base na sua direção.

desenhaCorpoIni :: Inimigo -> [Picture] -> Picture
desenhaCorpoIni ini imgs = case direcaoInimigo ini of
    Norte -> iniNorte ini imgs
    Sul   -> iniSul ini imgs
    Este  -> iniEste ini imgs
    Oeste -> iniOeste ini imgs


-- | Determina a cor da vida com base nos projéteis

determinaCor :: [Projetil] -> Color
determinaCor projeteis

    | Gelo `elem` tiposProjetil = blue                      -- Cor azul para gelo.
    | Resina `elem` tiposProjetil = castanhoEscuro          -- Cor castanha para resina.
    | Fogo `elem` tiposProjetil = red                       -- Cor vermelha para fogo.
    | otherwise = makeColor 1.0 0.75 0.8 1.0                -- Cor rosa claro se não há projeteis.

    where tiposProjetil = map tipoProjetil projeteis


-- | Desenha uma barra que representa a vida do inimigo.

desenhaVida :: Inimigo -> Picture
desenhaVida ini = translate x y $ color (determinaCor projeteis) $ rectangleSolid vidaAtual alturaBarra

    where alturaBarra = 3
          vidaAtual = vidaInimigo ini
          projeteis = projeteisInimigo ini
          pos = posicaoInimigo ini
          (x,y) = (fst pos * 60 - 910, - (snd pos * 60) + 515)


-- | Desenha o inimigo com a respetiva vida.

desenhaInimigo :: Inimigo -> [Picture] -> Picture
desenhaInimigo ini imgs = pictures [desenhaCorpoIni ini imgs, desenhaVida ini]


-- | Desenha todos os inimigos em jogo.

desenhaInimigos :: [Inimigo] -> [Picture] -> Picture
desenhaInimigos inimigos imgs = pictures $ map (`desenhaInimigo` imgs) inimigos


--------------------------------------------------------------------------------------------------------------------------------------


-- | Desenha o Jogo.

desenhaJogo :: Imagens -> ImmutableTowers -> Picture
desenhaJogo imgs it = case it of

      MenuPrincipal pz -> scale 1 0.95 (menuInicial imgs)

      RegrasJogo pz -> translate 0 (-2) $ scale 1.05 1 (regras imgs)

      Creditos pz -> scale 1 0.95 (creditos imgs)

      SelecaoNiveis pz -> scale 1 0.95 (niveis imgs)

      Pausa jogo pz -> scale 1 0.95 (pausa imgs)

      Proezas pz -> desenhaProezas pz (proezas imgs)

      JogoRun jogo refePos b pz -> desenhaJogoAux imgs jogo refePos b

      Vitoria jogo pz -> scale 1 0.95 (vitoria imgs)

      Derrota jogo pz -> scale 1 0.95 (derrota imgs)




-- | Função auxiliar para desenhar as Proezas.

desenhaProezas :: [Proezas] -> [Picture] -> Picture
desenhaProezas proezas imagensProezas

      | elem Nivel1Concluido proezas && elem Nivel2Concluido proezas && elem Nivel3Concluido proezas = imgsProezas !! 7        -- Todos os níveis concluídos.
      | elem Nivel2Concluido proezas && elem Nivel3Concluido proezas = imgsProezas !! 6                                        -- Níveis 2 e 3 concluídos.
      | elem Nivel1Concluido proezas && elem Nivel3Concluido proezas = imgsProezas !! 5                                        -- Níveis 1 e 3 concluídos.
      | elem Nivel1Concluido proezas && elem Nivel2Concluido proezas = imgsProezas !! 4                                        -- Níveis 1 e 2 concluídos.
      | elem Nivel3Concluido proezas = imgsProezas !! 3                                                                        -- Apenas nível 3 concluído.
      | elem Nivel2Concluido proezas = imgsProezas !! 2                                                                        -- Apenas nível 2 concluído.
      | elem Nivel1Concluido proezas = imgsProezas !! 1                                                                        -- Apenas nível 1 concluído
      | otherwise = imgsProezas !! 0


    where imgsProezas = map (scale 1 0.94) imagensProezas



-- | Função auxiliar para desenhar o jogoRun.

desenhaJogoAux :: Imagens -> Jogo -> PosRef -> Compra -> Picture
desenhaJogoAux imgs jogo pos compra =

  pictures [ fundoRelva 21 16 (relva imgs),                                   -- Fundo relva.
             desenhaMapa (mapaJogo jogo),                                     -- Resto do mapa.
             desenhaPortaleBase jogo portaldirimg portalesqimg baseimg,       -- Portais e base.
             fundoLoja,                                                       -- Fundo loja.
             torresLoja jogo torresImgs,                                      -- Torres para a loja.
             vida jogo,                                                       -- Vida.
             creditosLoja jogo,                                               -- Créditos.
             desenhaReferencia compra pos,                                    -- Referência.
             desenhaInimigos (inimigosJogo jogo) iniImgs,                     -- Inimigos em jogo.
             desenhaTorres torresImgs (torresJogo jogo)  ]                    -- Torres para o mapa.


    where torresImgs = torres imgs
          (portaldirimg, portalesqimg) = portais imgs
          baseimg = base imgs
          iniImgs = inimigos imgs
