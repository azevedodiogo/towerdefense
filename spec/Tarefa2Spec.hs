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

-- | Inimigo 5 para testes.
inimigo05 :: Inimigo
inimigo05 = Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (2.0, 2.0) 0

-- | Inimigo 6 para testes.
inimigo06 :: Inimigo
inimigo06 = Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (2.0, 2.0) 0

-- | Torre 1 para testes.
torre01 :: Torre
torre01 = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))

-- | Portal 1 para testes.
portal01 :: Portal
portal01 = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda01]}

-- | Portal 2 para testes.
portal02 :: Portal
portal02 = Portal {posicaoPortal = (2.0, 2.0), ondasPortal = [onda03, onda04]}

-- | Onda 1 para testes.
onda01 :: Onda
onda01 = Onda {inimigosOnda = [inimigo01, inimigo02], cicloOnda = 5, tempoOnda = 0, entradaOnda = 0}

-- | Onda 2 para testes.
onda02 :: Onda
onda02 = Onda {inimigosOnda = [inimigo03], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}

-- | Onda 3 para testes.
onda03 :: Onda
onda03 = Onda {inimigosOnda = [inimigo04, inimigo05], cicloOnda = 5, tempoOnda = 0, entradaOnda = 1}

-- | Onda 4 para testes.
onda04 :: Onda
onda04 = Onda {inimigosOnda = [inimigo06], cicloOnda = 3, tempoOnda = 0, entradaOnda = 2}