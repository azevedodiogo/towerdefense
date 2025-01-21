module Tarefa2Spec (testesTarefa2) where
  
import Tarefa2
import LI12425
import Test.HUnit

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Inimigo 1 para testes.
inimigo01 :: Inimigo
inimigo01 = Inimigo (3.0, 4.0) Norte 100 1 10 20 [Projetil Fogo (Finita 5)] (0.5,0.5) 0

-- | Inimigo 2 para testes.
inimigo02 :: Inimigo
inimigo02 = Inimigo (7.0, 5.0) Sul 80 1 15 30 [Projetil Gelo (Finita 3)] (0.5,0.5) 0

-- | Inimigo 3 para testes.
inimigo03 :: Inimigo
inimigo03 = Inimigo (10.0, 10.0) Este 50 1.5 20 40 [Projetil Resina Infinita] (0,0) 0

-- | Inimigo 4 para testes.
inimigo04 :: Inimigo
inimigo04 = Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (2.0, 2.0) 0