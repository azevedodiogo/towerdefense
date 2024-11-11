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