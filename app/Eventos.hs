module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425


-- | Altera o estado do jogo conforme a tecla / botão do rato pressionado

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers



-- Menu Principal: Reage ao clique do botão esquerdo do rato, redirecionando para um dos seguinte estados do jogo: SeleçãoNiveis, Creditos, Proezas, Instruções ou Sair

reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (MenuPrincipal pz) | x >= -130 && x <= 130 && y >= -60 && y <= 40 = SelecaoNiveis pz
                                                                                 | x >= 840 && x <= 930 && y >= 135 && y <= 215 = Creditos pz