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
                      

-- | data Imagens (agrupa as imgs que serão utilizadas no jogo)

data Imagens = Imagens { 
                    relva :: Picture,
                    menuInicial :: Picture,                                                         -- Imagem do menu inicial
                    regras      :: Picture,                                                         -- Imagem das instruções para o jogo
                    creditos    :: Picture,                                                         -- Imagem dos creditos
                    niveis      :: Picture,                                                         -- Imagem para seleção dos níveis
                    pausa       :: Picture,                                                         -- Imagem pausa
                    proezas     :: [Picture],                                                       -- Proezas: 0, 1, 2, 3, 1 2, 1 3, 2 3, 1 2 3 
                    vitoria     :: Picture,                                                         -- Imagem de vitória
                    derrota     :: Picture,                                                         -- Imagem de derrota
                    torres      :: [Picture],                                                       -- Torres: fogo, gelo, resina
                    portais     :: (Picture, Picture),                                              -- Portais: direita, esquerda
                    base        :: Picture,                                                         -- Base 
                    inimigos    :: [Picture]  
                     }

