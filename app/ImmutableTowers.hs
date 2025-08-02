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



-- | Posição da referência que irá servir para comprar as torres
type PosRef = (Int, Int) 

-- | Indica se o jogador quer ou não comprar, e se tem ou não dinheiro
data Compra = QuerComprar | NaoQuerComprar | NaoTemDinheiro 

-- | Indica aquilo que o jogador já conquistou
data Proezas = Nivel1Concluido | Nivel2Concluido | Nivel3Concluido  deriving (Show, Eq)



--------------------------------------------------------------------------------------------------------------------------------------------------------


-- | NIVEL UM

nivelUm :: Jogo
nivelUm = Jogo {baseJogo = base01, portaisJogo = [portal01], torresJogo = [], mapaJogo = mapa01, inimigosJogo = [], lojaJogo = loja01, nivelJogo = Um}



-- | Mapa01  (21 largura / 16 altura)

mapa01 :: Mapa
mapa01 =
   [  [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, t, t, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r],
      [r, r, r, t, t, t, t, t, r, r, r, r, r, r, r, r, r, t, r, r, r],
      [r, r, r, t, r, r, r, t, r, r, r, r, r, r, r, r, r, t, r, r, r],
      [r, r, r, t, r, r, r, t, r, r, r, r, r, r, r, r, r, t, r, r, r],
      [a, a, a, t, a, a, a, t, a, a, a, a, a, a, a, r, r, t, r, r, r],
      [r, r, r, t, r, r, r, t, r, r, r, r, r, r, a, r, r, t, r, r, r],
      [r, r, r, t, r, r, r, t, r, r, r, r, t, t, t, t, t, t, r, r, r],
      [r, r, r, t, r, r, r, t, r, r, r, r, t, r, a, r, r, r, r, r, r],
      [r, r, r, t, r, r, r, t, r, r, r, r, t, r, a, r, r, r, r, r, r],
      [r, r, r, t, r, r, r, t, r, r, r, r, t, r, a, r, r, r, r, r, r],
      [r, r, r, t, r, r, r, t, t, t, t, t, t, r, a, r, r, r, r, r, r],
      [r, r, r, t, r, r, r, r, r, r, r, r, r, r, a, a, a, a, a, a, a],
      [r, t, t, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r] ]

    where t = Terra
          a = Agua
          r = Relva
