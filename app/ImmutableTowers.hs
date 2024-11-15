module ImmutableTowers where

import Graphics.Gloss
import LI12425


-- | data ImmutableTowers

data ImmutableTowers = MenuPrincipal [Proezas]
                      | RegrasJogo [Proezas]
                      | Creditos [Proezas]                  
                      | SelecaoNiveis [Proezas] 
                      | Pausa Jogo [Proezas]                
                      | Proezas [Proezas] 
                      | JogoRun Jogo PosRef Compra [Proezas] 
                      | Vitoria Jogo [Proezas] 
                      | Derrota Jogo [Proezas] 