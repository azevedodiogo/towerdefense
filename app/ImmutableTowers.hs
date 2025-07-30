module ImmutableTowers where

import Graphics.Gloss
import LI12425


-- | data ImmutableTowers

data ImmutableTowers = MenuPrincipal [Proezas]
                      | RegrasJogo [Proezas]
                      | Creditos [Proezas]                  
                      | SelecaoNiveis [Proezas] 