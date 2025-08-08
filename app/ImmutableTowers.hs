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


-- | Base01
base01 :: Base
base01 = Base {vidaBase = 200, posicaoBase = (19,1), creditosBase = 200}



-- | primeira onda (portal01): inimigos tem 20 de vida, 0.8 de velocidade, 10 de dano

iniOnda01 :: [Inimigo]                                  
iniOnda01 = 
  -- pos, dir, vida, v, dano, creditos, projeteis, posi, tempo
  [ Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0, 
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,  
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0, 
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0, 
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0,
    Inimigo (1,14) Este 20 0.8 10 10 [] (1,14) 0 ]


-- | segunda onda (portal01): inimigos tem 30 de vida, 1.0 de velocidade, 12 de dano

iniOnda02 :: [Inimigo]                                  
iniOnda02 =
  -- pos, dir, vida, v, dano, creditos, projeteis, posi, tempo
  [ Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0,
    Inimigo (1,14) Este 30 1.0 12 12 [] (1,14) 0 ]


-- | terceira onda (portal01): inimigos tem 40 de vida, 1.0 de velocidade, 12 de dano

iniOnda03 :: [Inimigo]                                  
iniOnda03 = 
  -- pos, dir, vida, v, dano, creditos, projeteis, posi, tempo
  [ Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0,
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0, 
    Inimigo (1,14) Este 40 1.0 15 15 [] (1,14) 0 ]


-- | quarta onda (portal01): inimigos tem 50 de vida, 1.2 de velocidade, 20 de dano

iniOnda04 :: [Inimigo]                                  
iniOnda04 = 
  -- pos, dir, vida, v, dano, creditos, projeteis, posi, tempo
  [ Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0,
    Inimigo (1,14) Este 50 1.2 20 20 [] (1,14) 0 ]


-- | Onda01
onda01 :: Onda
onda01 = Onda {inimigosOnda = iniOnda01, cicloOnda = 3, tempoOnda = 0, entradaOnda = 10}

-- | Onda02
onda02 :: Onda
onda02 = Onda {inimigosOnda = iniOnda02, cicloOnda = 3, tempoOnda = 0, entradaOnda = 15}

-- | Onda03
onda03 :: Onda
onda03 = Onda {inimigosOnda = iniOnda03, cicloOnda = 2.5, tempoOnda = 0, entradaOnda = 15}

-- | Onda04
onda04 :: Onda
onda04 = Onda {inimigosOnda = iniOnda04, cicloOnda = 2.5, tempoOnda = 0, entradaOnda = 15}


-- | Portal01
portal01 :: Portal
portal01 = Portal {posicaoPortal = (1,14), ondasPortal = [onda01, onda02, onda03, onda04]}


-- | Loja01
loja01 :: Loja
loja01 = [tFogo01, tGelo01, tResina01]


-- | Torre01 fogo 
tFogo01 :: (Creditos,Torre)
tFogo01 = (150, Torre {posicaoTorre = (0, 0), danoTorre = 10, alcanceTorre = 2.5, rajadaTorre = 1, cicloTorre = 3, 
                tempoTorre = 2, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2}})

-- | Torre01 gelo
tGelo01 :: (Creditos, Torre)
tGelo01 = (140, Torre {posicaoTorre = (0, 0), danoTorre = 12, alcanceTorre = 3.5, rajadaTorre = 2, cicloTorre = 4, 
                tempoTorre = 3, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2}})

-- | Torre01 resina
tResina01 :: (Creditos, Torre)
tResina01 = (120, Torre {posicaoTorre = (0, 0), danoTorre = 8, alcanceTorre = 4, rajadaTorre = 2, cicloTorre = 4.5, 
                  tempoTorre = 3, projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}})

-- (as posições são fictícias)

--------------------------------------------------------------------------------------------------------------------------------------------------------


-- | NIVEL DOIS

nivelDois :: Jogo
nivelDois = Jogo {baseJogo = base02, portaisJogo = [portal02, portal03], torresJogo = [], mapaJogo = mapa02, inimigosJogo = [], lojaJogo = loja02, nivelJogo = Dois}
 

-- | Mapa (21 largura / 16 altura)
mapa02 :: Mapa
mapa02 =
   [  [r, r, r, r, r, r, r, r, r, r, r, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, a, r, r, t, t, t, t, t, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, a, r, r, t, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, t, t, t, t, t, t, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, t, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, t, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, t, t, t, t, t, t, t, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, t, r, r, r, r, r, r, r, a, r, r, r, r, r, r, r, r],
      [r, t, t, t, t, r, r, r, r, r, r, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, t, r, r, r, r, r, r, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, t, t, t, t, t, t, t, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, t, r, a, r, r, r, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, t, t, t, t, t, t, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, a, r, r, t, r, r, r, r, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, a, r, r, t, t, t, t, t, r],
      [r, r, r, r, r, r, r, r, r, r, r, r, a, r, r, r, r, r, r, r, r] ]

    where t = Terra
          a = Agua
          r = Relva


-- | Base02
base02 :: Base
base02 = Base {vidaBase = 150, posicaoBase = (1,8), creditosBase = 400}


-- | primeira onda (portal02): inimigos tem 30 de vida, 1.0 de velocidade, 10 de dano

iniOnda05 :: [Inimigo]                                  
iniOnda05 = 
  -- pos, dir, vida, v, dano, creditos, projeteis, posi, tempo
  [ Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0,
    Inimigo (19,1) Oeste 30 1.0 10 7 [] (19,1) 0 ]


-- | segunda onda (portal02): inimigos tem 50 de vida, 1.0 de velocidade, 12 de dano