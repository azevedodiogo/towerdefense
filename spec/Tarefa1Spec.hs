{-|
Module      : Tarefa1Spec
Description : Testes da tarefa 1
Copyright   : Diogo Matos Azevedo <a109727@alunos.uminho.pt>
              Vera da Silva Almeida <a110723@alunos.uminho.pt>


Módulo para a realização dos testes da Tarefa 1 de LI1 em 2024/25.

O objetivo deste ficheiro é validar as funções implementadas na Tarefa 1.
-}
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

-- | Testa se o número de portais no jogo está correto
testaPortaisJogo :: Test
testaPortaisJogo = "Teste 2" ~: 1 ~?= length (portaisJogo jogo01)

-- | Teste que verifica a validação geral do jogo
testaValidaJogo :: Test
testaValidaJogo = "Teste 3" ~: False ~?= validaJogo jogo01

-- | Testa se a verificação das posições dos portais está correta
testaVerificaPosiPortal :: Test
testaVerificaPosiPortal = "Teste 4" ~: True ~?= verificaPosiPortal jogo01

-- | Testa se a verificação do caminho de terra está correta
testaVerificaCaminhoTerra :: Test
testaVerificaCaminhoTerra = "Teste 5" ~: True ~?= verificaCaminhoTerra jogo01

-- | Testa se a verificação da sobreposição de portais está correta
testaVerificaPortalSobreposicao :: Test
testaVerificaPortalSobreposicao = "Teste 6" ~: True ~?= verificaPortalSobreposicao jogo01

-- | Testa se a verificação do número de ondas ativas por portal está correta
testaVerificaOndaPortal :: Test
testaVerificaOndaPortal = "Teste 7" ~: True ~?= verificaOndaPortal jogo01

-- | Testa se a verificação de inimigos sobre terra está correta
testaVerificaInimigosTerra :: Test
testaVerificaInimigosTerra = "Teste 8" ~: True ~?= verificaInimigosTerra jogo01

-- | Testa se a verificação de sobreposição de inimigos está correta
testaVerificaInimigosSobreposicao :: Test
testaVerificaInimigosSobreposicao = "Teste 9" ~: True ~?= verificaInimigosSobreposicao jogo01

-- | Testa se a verificação de velocidade de inimigos está correta
testaVerificaVelocidadeInimigo :: Test
testaVerificaVelocidadeInimigo = "Teste 10" ~: True ~?= verificaVelocidadeInimigo jogo01

-- | Testa se a verificação de projéteis ativos nos inimigos está correta
testaVerificaProjeteisAtivos :: Test
testaVerificaProjeteisAtivos = "Teste 11" ~: True ~?= verificaProjeteisAtivos jogo01

-- | Testa se a verificação da posição das torres está correta
testaVerificaPosiTorre :: Test
testaVerificaPosiTorre = "Teste 12" ~: True ~?= verificaPosiTorre jogo01

-- | Testa se a verificação do alcance das torres está correta
testaVerificaAlcanceTorre :: Test
testaVerificaAlcanceTorre = "Teste 13" ~: True ~?= verificaAlcanceTorre jogo01

-- | Testa se a verificação da rajada das torres está correta
testaVerificaRajadaTorre :: Test
testaVerificaRajadaTorre = "Teste 14" ~: True ~?= verificaRajadaTorre jogo01

-- | Testa se a verificação do ciclo das torres está correta
testaVerificaCicloTorre :: Test
testaVerificaCicloTorre = "Teste 15" ~: True ~?= verificaCicloTorre jogo01

-- | Testa se a verificação de sobreposição de torres está correta
testaVerificaNotSobreposicaoTorre :: Test
testaVerificaNotSobreposicaoTorre = "Teste 16" ~: True ~?= verificaNotSobreposicaoTorre jogo01

-- | Testa se a verificação de a base estar sobre terra está correta
testaVerificaBaseTerra :: Test
testaVerificaBaseTerra = "Teste 17" ~: True ~?= verificaBaseTerra jogo01

-- | Testa se a verificação de sobreposição da base está correta
testaVerificaBaseSobreposicao :: Test
testaVerificaBaseSobreposicao = "Teste 18" ~: True ~?= verificaBaseSobreposicao jogo01

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Lista de testes da Tarefa 1
testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ TestLabel "Teste 1" testaTerrenoMapa,
        TestLabel "Teste 2" testaPortaisJogo,
        TestLabel "Teste 3" testaValidaJogo,
        TestLabel "Teste 4" testaVerificaPosiPortal,
        TestLabel "Teste 5" testaVerificaCaminhoTerra,
        TestLabel "Teste 6" testaVerificaPortalSobreposicao,
        TestLabel "Teste 7" testaVerificaOndaPortal,
        TestLabel "Teste 8" testaVerificaInimigosTerra,
        TestLabel "Teste 9" testaVerificaInimigosSobreposicao,
        TestLabel "Teste 10" testaVerificaVelocidadeInimigo,
        TestLabel "Teste 11" testaVerificaProjeteisAtivos,
        TestLabel "Teste 12" testaVerificaPosiTorre,
        TestLabel "Teste 13" testaVerificaAlcanceTorre,
        TestLabel "Teste 14" testaVerificaRajadaTorre,
        TestLabel "Teste 15" testaVerificaCicloTorre,
        TestLabel "Teste 16" testaVerificaNotSobreposicaoTorre,
        TestLabel "Teste 17" testaVerificaBaseTerra,
        TestLabel "Teste 18" testaVerificaBaseSobreposicao
      ]