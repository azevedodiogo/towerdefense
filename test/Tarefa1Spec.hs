module Tarefa1Spec (testesTarefa1) where

import Tarefa1
import LI12425
import Test.HUnit

---------------------------------------------------------------------------------------------------------------------------------------


-- | Torre 1 para testes.
torre01 :: Torre
torre01 = Torre
  { posicaoTorre = (3.0, 2.0),
    danoTorre = 50.0,
    alcanceTorre = 5.0,
    rajadaTorre = 3,
    cicloTorre = 2.0,
    tempoTorre = 1.5,
    projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 }
  }

-- | Inimigo 1 para testes.
inimigo01 :: Inimigo
inimigo01 = Inimigo
  { posicaoInimigo = (1.0, 1.0),
    direcaoInimigo = Sul,
    vidaInimigo = 100.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 10.0,
    butimInimigo = 20,
    projeteisInimigo = [Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 2.0 }],
    posInicial = (0.0, 0.0),
    tempoInimigo = 0.0
  }

-- | Base 1 para testes.
base01 :: Base
base01 = Base
  { vidaBase = 100.0,
    posicaoBase = (2.0, 0),
    creditosBase = 100
  }
