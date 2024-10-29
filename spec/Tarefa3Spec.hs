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