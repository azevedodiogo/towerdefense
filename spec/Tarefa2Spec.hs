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

-- | Lista de inimigos para testes.
listaInimigos01 :: [Inimigo]
listaInimigos01 = [ Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0,
                    Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0,
                    Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (10.0, 10.0) 0
                    ]

-- | Lista de projéteis para testes.
listaProjeteis01 :: [Projetil]
listaProjeteis01 = [Projetil Gelo (Finita 3), Projetil Gelo (Finita 5), Projetil Resina Infinita]

-- | Projétil 1 para testes.
projetil01 :: Projetil
projetil01 = Projetil Gelo (Finita 3)

-- | Projétil 2 para testes.
projetil02 :: Projetil
projetil02 = Projetil Resina Infinita

-- | Projétil 3 para testes.
projetil03 :: Projetil
projetil03 = Projetil Fogo (Finita 5)

-- | Jogo 1 para testes.
jogo01 :: Jogo
jogo01 = Jogo {baseJogo = base01, portaisJogo = [portal01], torresJogo = [], mapaJogo = mapa01, inimigosJogo = [], lojaJogo = loja01, nivelJogo = Um}

-- | Jogo 2 para testes.
jogo02 :: Jogo
jogo02 = Jogo {baseJogo = base02, portaisJogo = [], torresJogo = [], mapaJogo = mapa01, inimigosJogo = [], lojaJogo = loja01, nivelJogo = Um}

-- | Jogo 3 para testes.
jogo03 :: Jogo
jogo03 = Jogo {baseJogo = base02, portaisJogo = [], torresJogo = [], mapaJogo = mapa01, inimigosJogo = [], lojaJogo = [], nivelJogo = Um}

-- | Base 1 para testes.
base01 :: Base
base01 = Base {vidaBase = 100, posicaoBase = (5.5, 2.5), creditosBase = 50}

-- | Base 2 para testes.
base02 :: Base
base02 = Base {vidaBase = 0, posicaoBase = (5.5, 2.5), creditosBase = 50}

-- | Loja 1 para testes.
loja01 :: Loja
loja01 = [tFogo01, tGelo01, tResina01]

-- | Torre de fogo 1 para testes.
tFogo01 :: (Creditos,Torre)
tFogo01 = (150, Torre {posicaoTorre = (0, 0), danoTorre = 10, alcanceTorre = 2.5, rajadaTorre = 1, cicloTorre = 3, 
                tempoTorre = 2, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2}})

-- | Torre de gelo 1 para testes.
tGelo01 :: (Creditos, Torre)
tGelo01 = (140, Torre {posicaoTorre = (0, 0), danoTorre = 12, alcanceTorre = 3.5, rajadaTorre = 2, cicloTorre = 4, 
                tempoTorre = 3, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2}})

-- | Torre de resina 1 para testes.
tResina01 :: (Creditos, Torre)
tResina01 = (120, Torre {posicaoTorre = (0, 0), danoTorre = 8, alcanceTorre = 4, rajadaTorre = 2, cicloTorre = 4.5, 
                  tempoTorre = 3, projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}})

-- | Mapa 1 para testes.
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

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Testa se o inimigo está no alcance da torre.
testaInimigosAlcanceTorre :: Test
testaInimigosAlcanceTorre = "Teste 1" ~: [Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (3.0,4.0), tempoInimigo = 0.0}, Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (7.0,5.0), tempoInimigo = 0.0}] ~?= inimigosNoAlcance torre01 listaInimigos01


-- | Testa se a função atualiza o estado de um inimigo sempre que este é atingido por um projétil.
testaAtingeInimigo :: Test
testaAtingeInimigo = "Teste 2" ~: Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 75.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 12.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0} ~?= atingeInimigo torre01 inimigo01


-- | Testa se a função junta os projéteis iguais.
testaIguais :: Test
testaIguais = "Teste 3" ~: [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 8.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}] ~?= iguais listaProjeteis01


-- | Testa se a função resolve conflitos entre projéteis.
testaResolveConflitoProjetil :: Test
testaResolveConflitoProjetil = "Teste 4" ~: [] ~?= resolveConflito projetil03 [projetil01]


-- | Testa se a função ativa o inimigo.
testaAtivaInimigo :: Test
testaAtivaInimigo = "Teste 5" ~: (Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}], cicloOnda = 5.0, tempoOnda = 5.0, entradaOnda = 0.0}]}, [Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0},Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}], posInicial = (0,0), tempoInimigo = 0.0}]) ~?= ativaInimigo portal01 [inimigo03]


-- | Testa se a função pode lançar um projétil.
testaPodeLançar :: Test
testaPodeLançar = "Teste 6" ~: True ~?= podelançar onda01


-- | Testa se a função remove um inimigo.
testaRemoveInimigo :: Test
testaRemoveInimigo = "Teste 7" ~: (Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}, Onda {inimigosOnda = [Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}], cicloOnda = 5.0, tempoOnda = 5.0, entradaOnda = 0.0}) ~?= removeInimigo onda01


-- | Testa se a função terminou o jogo.
testaTerminouJogo :: Test
testaTerminouJogo = "Teste 8" ~: False ~?= terminouJogo jogo01


-- | Testa se o jogador perdeu o jogo.
testaPerdeuJogo :: Test
testaPerdeuJogo = "Teste 9" ~: True ~?= terminouJogo jogo03


-- | Testa se a função filtra os inimigos.
testaFiltraInimigos :: Test
testaFiltraInimigos = "Teste 10" ~: [Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (2.0, 2.0), tempoInimigo = 0.0}, Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (2.0, 2.0), tempoInimigo = 0.0}, Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [], posInicial = (2.0, 2.0), tempoInimigo = 0.0}] ~?= filtrainimigos [portal02]


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Testes da Tarefa 2.
testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ TestLabel "Teste 1" testaInimigosAlcanceTorre,
        TestLabel "Teste 2" testaAtingeInimigo,
        TestLabel "Teste 3" testaIguais,
        TestLabel "Teste 4" testaResolveConflitoProjetil,
        TestLabel "Teste 5" testaAtivaInimigo,
        TestLabel "Teste 6" testaPodeLançar,
        TestLabel "Teste 7" testaRemoveInimigo,
        TestLabel "Teste 8" testaTerminouJogo,
        TestLabel "Teste 9" testaPerdeuJogo,
        TestLabel "Teste 10" testaFiltraInimigos
      ]