module Tarefa3Spec where

import Tarefa3
import LI12425
import Test.HUnit

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Base para teste da função atualizaJogo.
baseAtualiza :: Base
baseAtualiza = Base {vidaBase = 100, posicaoBase = (2, 2), creditosBase = 50}

-- | Portal para teste da função atualizaJogo.
portalAtualiza :: Portal
portalAtualiza = Portal {posicaoPortal = (0, 0), ondasPortal = [Onda [] 5 0 0]}

-- | Torre para teste da função atualizaJogo.
torreAtualiza :: Torre
torreAtualiza = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))

-- | Mapa para teste da função atualizaJogo.
mapaAtualiza :: Mapa
mapaAtualiza = [ [Terra, Relva, Agua, Relva, Terra], [Terra, Terra, Terra, Relva, Agua], [Agua, Relva, Terra, Relva, Relva] ]

-- | Inimigo para teste da função atualizaJogo.
inimigoAtualiza :: Inimigo
inimigoAtualiza = Inimigo (1, 1) Norte 100 1.0 10 20 [] (0, 0) 0

-- | Jogo para teste da função atualizaJogo.
jogoAtualiza :: Jogo
jogoAtualiza = Jogo baseAtualiza [portalAtualiza] [torreAtualiza] mapaAtualiza [inimigoAtualiza] [] Um

-- | Torre para teste da função disparaTorre.
torreDispara :: Torre
torreDispara = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))

-- | Lista de inimigos para teste da função disparaTorre.
listaInimigosDispara :: [Inimigo]
listaInimigosDispara = [Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0,
                        Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0,
                        Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (10.0, 10.0) 0
                        ]