module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425


-- | Altera o estado do jogo conforme a tecla / botão do rato pressionado

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers



-- Menu Principal: Reage ao clique do botão esquerdo do rato, redirecionando para um dos seguinte estados do jogo: SeleçãoNiveis, Creditos, Proezas, Instruções ou Sair

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (MenuPrincipal pz) | x >= -130 && x <= 130 && y >= -60 && y <= 40 = SelecaoNiveis pz
                                                                                 | x >= 840 && x <= 930 && y >= 135 && y <= 215 = Creditos pz
                                                                                 | x >= 820 && x <= 940 && y >= 280 && y <= 340 = RegrasJogo pz
                                                                                 | x >= 850 && x <= 930 && y >= 377 && y <= 483 = Proezas pz
                                                                                 | sairMenu (x,y) = undefined -- Obriga o jogo a fechar, uma vez que 'crasha' o jogo.
                                                                                 | otherwise = MenuPrincipal pz


-- Instruções: Reage ao clique do botão esquerdo do rato, redirecionando para o MenuPrincipal

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (RegrasJogo pz) | x >= -105 && x <= 91 && y >= -421 && y <= -347 = MenuPrincipal pz
                                                                              | otherwise = RegrasJogo pz


-- Creditos: Reage ao clique do botão esquerdo do rato, redirecionando para o MenuPrincipal

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (Creditos pz) | x >= -85 && x <= 85 && y >= -428 && y <= -364 = MenuPrincipal pz
                                                                            | otherwise = Creditos pz


-- Proezas: Reage ao clique do botão esquerdo do rato, redirecionando para o MenuPrincipal

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (Proezas pz) | x >= -60 && x <= 120 && y >= -428 && y <= -352 = MenuPrincipal pz
                                                                           | otherwise = Proezas pz


-- Seleção de Niveis: Reage ao clique do botão esquerdo do rato, redirecionando para um dos seguinte estados do jogo: NivelUm, NivelDois, NivelTres ou MenuPrincipal

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (SelecaoNiveis pz) | x >= -445 && x <= -215 && y >= -90 && y <= 140 = JogoRun nivelUm (-1000,1000) NaoQuerComprar pz
                                                                                 | x >= -110 && x <= 120 && y >= -90 && y <= 140 = JogoRun nivelDois (-1000,1000) NaoQuerComprar pz
                                                                                 | x >= 230 && x <= 460 && y >= -90 && y <= 140 = JogoRun nivelTres (-1000,1000) NaoQuerComprar pz
                                                                                 | x >= -90 && x <= 90 && y >= -210 && y <= -140 = MenuPrincipal pz
                                                                                 | otherwise = SelecaoNiveis pz


-- Pausa: Reage ao clique do botão esquerdo do rato, redirecionando para um dos seguinte estados do jogo: MenuPrincipal, JogoRun do nível 

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (Pausa jogo pz) | x >= -93 && x <= 93 && y >= -70 && y <= 0 = MenuPrincipal pz
                                                                              | x >= -93 && x <= 93 && y >= 29 && y <= 159 = JogoRun jogo (-1000,1000) NaoQuerComprar pz
                                                                              | sairPausa (x,y) = undefined -- Obriga o jogo a fechar, uma vez que 'crasha' o jogo.
                                                                              | otherwise = Pausa jogo pz


-- Derrota: Reage ao clique do botão esquerdo do rato, redirecionando para o menu principal ou reiniciando o jogo atual

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (Derrota jogo pz) | x >= -107 && x <= 123 && y >= -220 && y <= -130 = MenuPrincipal pz
                                                                                | dentroRestart (x, y) = JogoRun (reiniciarJogo (nivelJogo jogo)) (1000,1000) NaoQuerComprar pz
                                                                                | otherwise = Derrota jogo pz


-- Vitoria: Reage ao clique do botão esquerdo do rato, redirecionando para o menu principal ou avançando para o próximo nível

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (Vitoria jogo pz) | x >= -107 && x <= 123 && y >= -220 && y <= -130 = MenuPrincipal pz
                                                                                | dentroNextLevel (x, y) = JogoRun (proximoNivel (nivelJogo jogo)) (1000,1000) NaoQuerComprar pz
                                                                                | otherwise = Vitoria jogo pz



-- JogoRun

-- Clicar na tecla 'P' implica o desejo de o jogador querer pausar o jogo.

reageEventos (EventKey (Char 'p') Down _ _) (JogoRun jogo p b pz) = Pausa jogo pz


-- Clicar na tecla 'R' implica o desejo de o jogador querer reiniciar o jogo.

reageEventos (EventKey (Char 'r') Down _ _) (JogoRun jogo p b pz) = JogoRun nivel (1000,1000) NaoQuerComprar pz
                                                                    where nivel = case nivelJogo jogo of  Um   -> nivelUm
                                                                                                          Dois -> nivelDois
                                                                                                          Tres -> nivelTres


-- Clicar na tecla 'C' implica o desejo de o jogador querer comprar uma torre.

reageEventos (EventKey (Char 'c') Down _ _) (JogoRun jogo pos bool pz) = JogoRun jogo (fstPosRelva jogo) QuerComprar pz



-- Move a referência para que o jogador possa escolher a posição da torre que vai comprar.

-- Move a referência para cima 

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (JogoRun jogo pos compra pz) = let novaPos = calculaNovaPosicao pos (0, -1) (posicoesTorres jogo) (mapaJogo jogo)
                                                                                   in JogoRun jogo novaPos QuerComprar pz

-- Move a referência para baixo

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (JogoRun jogo pos compra pz) = let novaPos = calculaNovaPosicao pos (0, 1) (posicoesTorres jogo) (mapaJogo jogo)
                                                                                     in JogoRun jogo novaPos QuerComprar pz

-- Move a referência para a esquerda

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (JogoRun jogo pos compra pz) = let novaPos = calculaNovaPosicao pos (-1, 0) (posicoesTorres jogo) (mapaJogo jogo)
                                                                                     in JogoRun jogo novaPos QuerComprar pz

-- Move a referência para a direita

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (JogoRun jogo pos compra pz) = let novaPos = calculaNovaPosicao pos (1, 0) (posicoesTorres jogo) (mapaJogo jogo)
                                                                                      in JogoRun jogo novaPos QuerComprar pz


-- Coloca uma torre na posição escolhida, caso o jogador tenha dinheiro suficiente.

-- Desenha uma torre de fogo quando a tecla '1' é pressionada

reageEventos (EventKey (Char '1') Down _ _) (JogoRun jogo (x,y) compra pz) = let torrePos = (fromIntegral x, fromIntegral y)
                                                                                 jogoAtualizado = adicionaTorre jogo 1 torrePos

                                                                             in if podeComprar jogo 1 then JogoRun jogoAtualizado (1000,1000) NaoQuerComprar pz
                                                                                else JogoRun jogo (x,y) NaoTemDinheiro pz

-- Desenha uma torre de gelo quando a tecla '2' é pressionada

reageEventos (EventKey (Char '2') Down _ _) (JogoRun jogo (x,y) compra pz) = let torrePos = (fromIntegral x, fromIntegral y)
                                                                                 jogoAtualizado = adicionaTorre jogo 2 torrePos

                                                                             in if podeComprar jogo 2 then JogoRun jogoAtualizado (1000,1000) NaoQuerComprar pz
                                                                                else JogoRun jogo (x,y) NaoTemDinheiro pz

-- Desenha uma torre de resina quando a tecla '3' é pressionada

reageEventos (EventKey (Char '3') Down _ _) (JogoRun jogo (x,y) compra pz) = let torrePos = (fromIntegral x, fromIntegral y)
                                                                                 jogoAtualizado = adicionaTorre jogo 3 torrePos

                                                                             in if podeComprar jogo 3 then JogoRun jogoAtualizado (1000,1000) NaoQuerComprar pz
                                                                                else JogoRun jogo (x,y) NaoTemDinheiro pz

-- Desenha uma torre de fogo 2.0 quando a tecla '4' é pressionada

reageEventos (EventKey (Char '4') Down _ _) (JogoRun jogo (x,y) compra pz) = let torrePos = (fromIntegral x, fromIntegral y)
                                                                                 jogoAtualizado = adicionaTorre jogo 4 torrePos

                                                                             in if podeComprar jogo 4 then JogoRun jogoAtualizado (1000,1000) NaoQuerComprar pz
                                                                                else JogoRun jogo (x,y) NaoTemDinheiro pz

-- Desenha uma torre de gelo 2.0 quando a tecla '5' é pressionada

reageEventos (EventKey (Char '5') Down _ _) (JogoRun jogo (x,y) compra pz) = let torrePos = (fromIntegral x, fromIntegral y)
                                                                                 jogoAtualizado = adicionaTorre jogo 5 torrePos

                                                                             in if podeComprar jogo 5 then JogoRun jogoAtualizado (1000,1000) NaoQuerComprar pz
                                                                                else JogoRun jogo (x,y) NaoTemDinheiro pz

-- Desenha uma torre de resina 2.0 quando a tecla '6' é pressionada

reageEventos (EventKey (Char '6') Down _ _) (JogoRun jogo (x,y) compra pz) = let torrePos = (fromIntegral x, fromIntegral y)
                                                                                 jogoAtualizado = adicionaTorre jogo 6 torrePos

                                                                             in if podeComprar jogo 6 then JogoRun jogoAtualizado (1000,1000) NaoQuerComprar pz
                                                                                else JogoRun jogo (x,y) NaoTemDinheiro pz


reageEventos _ it = it



--------------------------------------------------------------------------------------------------------------------------------------------------------------


-- funções auxiliares


-- | Devolve a primeira pos de relva do mapa 

fstPosRelva :: Jogo -> (Int, Int)
fstPosRelva j = head (posicoesRelva (mapaJogo j))

-- | Encontra as posicoes de relva

posicoesRelva :: Mapa -> [(Int, Int)]
posicoesRelva mapa = [ (x, y) | (y, linha) <- zip [0..] mapa, (x, celula) <- zip [0..] linha, celula == Relva ]


-- | Devolve as posicoes das torres no mapa

posicoesTorres :: Jogo -> [Posicao]
posicoesTorres jogo = map posicaoTorre (torresJogo jogo)


-- | Verifica se a posição (x, y) está dentro dos limites do mapa

posicaoDentroDoMapa :: (Int, Int) -> Mapa -> Bool
posicaoDentroDoMapa (x, y) mapa = y >= 0 && y < length mapa && x >= 0 && x < length (head mapa)


-- | Calcula a próxima posição válida de Relva

calculaNovaPosicao :: (Int, Int) -> (Int, Int) -> [Posicao] -> Mapa -> (Int, Int)
calculaNovaPosicao (x,y) (dx, dy) postorres mapa

      | novaPos `elem` posValidas && novaPos `notElem` posTorresInt = novaPos                       -- a novaPos é de relva e não está sobreposta a nenhuma das torres
      | posicaoDentroDoMapa novaPos mapa = calculaNovaPosicao novaPos (dx, dy) postorres mapa       -- a novaPos não é relva, logo volta a chamar a funcao
      | otherwise = (x,y)                                                                           -- não há mais pos de relva na direção da seta pressionada

      where novaPos = (x + dx, y + dy)                                                              -- (dx, dy) é o deslocamento, neste caso 1 unidade
            posValidas = posicoesRelva mapa
            posTorresInt = map (\(z, w) -> (floor z, floor w)) postorres



-- | Adiciona uma torre e atualiza o dinheiro do jogador

adicionaTorre :: Jogo -> Int -> Posicao -> Jogo
adicionaTorre jogo indice pos = jogo {torresJogo = novasTorres, baseJogo = novaBase}

    where base = baseJogo jogo
          nivel = nivelJogo jogo

          loja = case nivel of Um   -> lojaJogo jogo
                               Dois -> lojaJogo jogo
                               Tres -> lojaJogo jogo

          (custo, torre) = loja !! (indice - 1)
          novasTorres = (torre {posicaoTorre = pos}) : torresJogo jogo
          novaBase = base {creditosBase = creditosBase base - custo}


-- | Verifica se o jogador tem dinheiro para comprar uma torre

podeComprar :: Jogo -> Int -> Bool
podeComprar jogo indice = dinheiro >= custo

    where dinheiro = creditosBase (baseJogo jogo)
          custo = if indice > length (lojaJogo jogo) || indice <= 0 then 1000000
                  else fst (lojaJogo jogo !! (indice - 1))





-- | Verifica se o clique está dentro do botao 'sair' na img do MenuPrincipal

sairMenu :: (Float, Float) -> Bool
sairMenu (x, y) = ((x+900)^2 + (y-454)^2) <= 35^2


-- | Verifica se o clique está dentro do botao 'sair' na img da Pausa

sairPausa :: (Float, Float) -> Bool
sairPausa (x, y) = ((x+912)^2 + (y-462)^2) <= 30^2