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