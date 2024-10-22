module Tarefa1Spec (testesTarefa1) where

import Tarefa1
import LI12425
import Test.HUnit

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Torre 1 para testes.
torre01 :: Torre
torre01 = Torre
  { posicaoTorre = (3.0, 2.0), danoTorre = 50.0, alcanceTorre = 5.0, rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 1.5, projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 } }

-- | Inimigo 1 para testes.
inimigo01 :: Inimigo
inimigo01 = Inimigo
  { posicaoInimigo = (1.0, 1.0), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 2.0 }], posInicial = (0.0, 0.0), tempoInimigo = 0.0 }

-- | Base 1 para testes.
base01 :: Base
base01 = Base
  { vidaBase = 100.0, posicaoBase = (2.0, 0), creditosBase = 100 }

-- | Onda 1 para testes.
onda01 :: Onda
onda01 = Onda
  { inimigosOnda = [inimigo01], cicloOnda = 3.0, tempoOnda = 3.0, entradaOnda = 5.0 }

-- | Portal 1 para testes.
portal01 :: Portal
portal01 = Portal
  { posicaoPortal = (0.0, 0.0), ondasPortal = [onda01] }

-- | Mapa 1 para testes.
mapa01 :: Mapa
mapa01 = Mapa
    [ [Terra, Terra, Terra, Terra, Relva], [Relva, Terra, Terra, Relva, Relva], [Agua, Terra, Terra, Relva, Terra], [Relva, Relva, Relva, Terra, Terra], [Relva, Terra, Relva, Relva, Agua] ]

-- | Loja 1 para testes.
loja01 :: Loja
loja01 = Loja
    [(50, torre01), (30, torre01)]

-- | Jogo 1 para testes.
jogo01 :: Jogo
jogo01 = Jogo
    { baseJogo = base01, portaisJogo = [portal01], torresJogo = [torre01], mapaJogo = mapa01, inimigosJogo = [inimigo01], lojaJogo = loja01, nivelJogo = Um }

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Testa se o tipo de terreno da posição (3, 2) no mapa é Relva
testaTerrenoMapa :: Test
testaTerrenoMapa = "Teste 1" ~: Relva ~?= (mapaJogo jogo01) !! 2 !! 3