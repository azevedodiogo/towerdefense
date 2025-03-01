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

-- | Lista de inimigos para teste da função geraID.
listaInimigosID :: [Inimigo]
listaInimigosID = [Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0, Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0] 

-- | Lista de inimigos para teste da função filtraInimigoID.
listaInimigosFiltraID :: [(Int, Inimigo)]
listaInimigosFiltraID = [(0, Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0),
                         (1, Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0),
                         (2, Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (10.0, 10.0) 0)]

-- | Lista de inimigos para teste da função filtraInimigoID.
listaInimigosAtingidosFiltraID :: [(Int, Inimigo)]          
listaInimigosAtingidosFiltraID = [(0, Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0), (1, Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0)]



-- | Inimigo para teste da função atualizaOnda.
inimigo1Atualiza :: Inimigo
inimigo1Atualiza = Inimigo {posicaoInimigo = (1.3,1.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (0.0,0.0), tempoInimigo = 0.0}

-- | Inimigo para teste da função atualizaOnda.
inimigo2Atualiza :: Inimigo
inimigo2Atualiza = Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (7.0,5.0), tempoInimigo = 0.0}

-- | Onda para teste da função atualizaOnda.
ondaAtualiza1 :: Onda
ondaAtualiza1 = Onda {inimigosOnda = [inimigo1Atualiza, inimigo2Atualiza], cicloOnda = 10, tempoOnda = 5, entradaOnda = 0}

-- | Onada para teste da função atualizaOnda.
ondaAtualiza2 :: Onda
ondaAtualiza2 = Onda {inimigosOnda = [], cicloOnda = 10, tempoOnda = 5, entradaOnda = 0}



-- | Portal para teste da função atualizaPortal.
portalAtualizaAtualiza :: Portal
portalAtualizaAtualiza = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [ondaAtualizaAtualiza]}

-- | Onda para teste da função atualizaPortal.
ondaAtualizaAtualiza :: Onda
ondaAtualizaAtualiza = Onda {inimigosOnda = [inimigoAtualizaAtualiza], cicloOnda = 10, tempoOnda = 0, entradaOnda = 0}

-- | Inimigo para teste da função atualizaPortal.
inimigoAtualizaAtualiza :: Inimigo
inimigoAtualizaAtualiza = Inimigo (0.5, 0.5) Norte 100.0 1.0 10.0 20 [] (0.5, 0.5) 0

-- | Inimigo para teste da função atualizaPortal.
inimigosAtualizaAtualiza :: [Inimigo]
inimigosAtualizaAtualiza = [Inimigo (2.5, 3.0) Oeste 100 4 30 23 [Projetil Gelo (Finita 4), Projetil Resina Infinita] (0.5, 0.5) 0]



-- | Portal para teste da função caminho.
portalCaminho :: Portal
portalCaminho = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [ondaCaminho]}

-- | Onda para teste da função caminho.
ondaCaminho :: Onda
ondaCaminho = Onda {inimigosOnda = [inimigoAtualizaAtualiza], cicloOnda = 10, tempoOnda = 5, entradaOnda = 0}

-- | Inimigo para teste da função caminho.
baseCaminho :: Base
baseCaminho = Base {vidaBase = 100, posicaoBase = (2.2, 2.5), creditosBase = 50}

-- | Mapa para teste da função caminho.
mapaCaminho :: Mapa
mapaCaminho = [ [Terra, Relva, Agua, Relva, Terra], [Terra, Terra, Terra, Relva, Agua], [Agua, Relva, Terra, Relva, Relva] ]



-- | Base para teste da função atualizaEstadoInimigo.
baseAtualizaEstadoInimigo :: Base
baseAtualizaEstadoInimigo = Base {vidaBase = 100, posicaoBase = (2.2, 2.5), creditosBase = 50}

-- | Mapa para teste da função atualizaEstadoInimigo.
mapaAtualizaEstadoInimigo :: Mapa
mapaAtualizaEstadoInimigo = [ [Terra, Relva, Agua, Relva, Terra], [Terra, Terra, Terra, Relva, Agua], [Agua, Relva, Terra, Relva, Relva] ]

-- | Inimigo para teste da função atualizaEstadoInimigo.
inimigo1AtualizaEstadoInimigo :: Inimigo
inimigo1AtualizaEstadoInimigo = Inimigo (0.5, 1.0) Este 100.0 1.0 5.0 25 [Projetil Resina Infinita, Projetil Gelo (Finita 2)] (0.5, 0.5) 0

-- | Inimigo para teste da função atualizaEstadoInimigo.
inimigo2AtualizaEstadoInimigo :: Inimigo
inimigo2AtualizaEstadoInimigo = Inimigo (0.5, 1.0) Sul 100.0 1.0 5.0 25 [Projetil Resina Infinita] (0.5, 0.5) 0



-- | Base para teste da função removeInimigoHitBase.
baseHitBase :: Base
baseHitBase = Base {vidaBase = 80.0, posicaoBase = (2.2, 2.2), creditosBase = 50}

-- | Lista de inimigos para teste da função removeInimigoHitBase.
inimigosHitBase :: [Inimigo]
inimigosHitBase = [Inimigo (2, 2) Este 100 1.0 5 25 [Projetil Resina Infinita] (0.5, 0.5) 0, Inimigo (0.5, 1.0) Sul 100 1.0 5 25 [Projetil Gelo (Finita 2)] (0.5, 0.5) 0]



-- | Lista de inimigos para teste da função removeInimigosSemVida.
baseIniSemVida :: Base
baseIniSemVida = Base {vidaBase = 100, posicaoBase = (2.2, 2.2), creditosBase = 50}

-- | Lista de inimigos para teste da função removeInimigosSemVida.
inimigosIniSemVida :: [Inimigo]
inimigosIniSemVida = [Inimigo (2, 2) Este 0 1.0 5 25 [Projetil Resina Infinita] (0.5, 0.5) 0, Inimigo (0.5, 1.0) Sul 100 1.0 5 25 [Projetil Gelo (Finita 2)] (0.5, 0.5) 0]

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Teste da função atualizaJogo.
testeAtualizaJogo :: Test
testeAtualizaJogo = "Teste 1" ~: Jogo {baseJogo = Base {vidaBase = 100.0, posicaoBase = (2.0,2.0), creditosBase = 50}, portaisJogo = [Portal {posicaoPortal = (0.0,0.0), ondasPortal = []}], torresJogo = [Torre {posicaoTorre = (5.0,5.0), danoTorre = 25.0, alcanceTorre = 3.0, rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 0.0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}}], mapaJogo = [[Terra,Relva,Agua,Relva,Terra],[Terra,Terra,Terra,Relva,Agua],[Agua,Relva,Terra,Relva,Relva]], inimigosJogo = [Inimigo {posicaoInimigo = (1.3,1.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (0.0,0.0), tempoInimigo = 0.0}], lojaJogo = [], nivelJogo = Um } ~=? atualizaJogo 0.3 jogoAtualiza

-- | Teste da função disparaTorre.
testeDisparaTorre :: Test
testeDisparaTorre = "Teste 2" ~: (Torre {posicaoTorre = (5.0,5.0), danoTorre = 25.0, alcanceTorre = 3.0, rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 2.0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}}, [Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 75.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}], posInicial = (3.0,4.0), tempoInimigo = 0.0},Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 55.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}], posInicial = (7.0,5.0), tempoInimigo = 0.0},Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [], posInicial = (10.0,10.0), tempoInimigo = 0.0}]) ~=? disparaTorre 0.2 torreDispara listaInimigosDispara

-- | Teste da função geraID.
testeGeraID :: Test
testeGeraID = "Teste 3" ~: [(0,Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (3.0,4.0), tempoInimigo = 0.0}), (1,Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (7.0,5.0), tempoInimigo = 0.0})] ~=? geraID listaInimigosID

-- | Teste da função filtraInimigoID.
testeFiltraID :: Test
testeFiltraID = "Teste 4" ~: [(2,Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [], posInicial = (10.0,10.0), tempoInimigo = 0.0})] ~=? filtraInimigoID listaInimigosFiltraID listaInimigosAtingidosFiltraID

-- | Teste da função atualizaOnda.
testeAtualizaOnda :: Test
testeAtualizaOnda = "Teste 5" ~: [Onda {inimigosOnda = [inimigo1Atualiza, inimigo2Atualiza], cicloOnda = 10, tempoOnda = 4.8, entradaOnda = 0}] ~=? atualizaOnda 0.2 [ondaAtualiza1]

-- | Teste da função atualizaPortal.
testeAtualizaPortal :: Test
testeAtualizaPortal = "Teste 6" ~: (Portal {posicaoPortal = (0.5,0.5), ondasPortal = []}, [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (0.5, 0.5), tempoInimigo = 0.0}, Inimigo {posicaoInimigo = (2.5,3.0), direcaoInimigo = Oeste, vidaInimigo = 100.0, velocidadeInimigo = 4.0, ataqueInimigo = 30.0, butimInimigo = 23, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}], posInicial = (0.5,0.5), tempoInimigo = 0.0}]) ~=? atualizaPortal 0.2 portalAtualizaAtualiza inimigosAtualizaAtualiza

-- | Teste da função caminho.
testeCaminho :: Test
testeCaminho = "Teste 7" ~: [(0,0),(0,1),(1,1),(2,1),(2,2)] ~=? caminho (0, 0) (2, 2) mapaCaminho

-- | Teste da função direcaoToOeste.
testaDirecaoToOeste :: Test
testaDirecaoToOeste = "Teste 8" ~: Sul ~=? direcaoToOeste Norte (3, 2) [(3, 3), (4, 3)]

-- | Teste da função atualizaPosicao.
testeAtualizaPosicao :: Test
testeAtualizaPosicao = "Teste 9" ~: (4.0, 2.0) ~=? atualizaPosicao [] Este (1.0, 2.0) 2.0 1.5

-- | Teste da função atualizaProjetil.
testeAtualizaProjetil :: Test
testeAtualizaProjetil = "Teste 10" ~: [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}] ~=? atualizaProjetil 1.0 [Projetil Gelo (Finita 5.0), Projetil Resina Infinita]

-- | Teste da função atualizaEstadoInimigo.
testeAtualizaEstadoInimigo :: Test
testeAtualizaEstadoInimigo = "Teste 11" ~: Inimigo {posicaoInimigo = (0.5,1.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.5}], posInicial = (0.5,0.5), tempoInimigo = 0.0} ~=? atualizaEstadoInimigo 0.5 mapaAtualizaEstadoInimigo (2.2, 2.5) inimigo1AtualizaEstadoInimigo

-- | Teste da função removeInimigoHitBase.
testeHitBase :: Test
testeHitBase = "Teste 12" ~: (Base {vidaBase = 75.0, posicaoBase = (2.2,2.2), creditosBase = 50},[Inimigo {posicaoInimigo = (0.5,1.0), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}]) ~=? removeInimigoHitBase baseHitBase inimigosHitBase

-- | Teste da função removeInimigosSemVida.
testeRemoveInimigosSemVida :: Test
testeRemoveInimigosSemVida = "Teste 13" ~: ([Inimigo {posicaoInimigo = (0.5,1.0), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}],75) ~=? removeInimigosSemVida inimigosIniSemVida 50

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
