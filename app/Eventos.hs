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