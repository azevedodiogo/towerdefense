{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Diogo Matos Azevedo <a109727@alunos.uminho.pt>
              Vera da Silva Almeida <a110723@alunos.uminho.pt>

-- O propósito desta tarefa é simular um modelo de jogo, com um foco nos movimentos dos inimigos e na interação destes com as torres e a base.
Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}

module Tarefa3 where

import Tarefa1
import Tarefa2
import LI12425
import Data.List

--------------------------------------------------------------------------------------------------------------------------------------------------------


{- | a função `atualizaJogo` simula a evolução de um jogo após um intervalo de tempo. 

As atualizações são realizadas em três etapas principais:
   
1. Atualização dos portais, adicionando novos inimigos ao jogo.
2. Atualização dos inimigos (movimento, vida, posição, etc.) e da base (vida e créditos).
3. Atualização das torres e dos seus efeitos sobre os inimigos.

=== Exemplos de Uso:

* `base` = Base {vidaBase = 100, posicaoBase = (2, 2), creditosBase = 50}
* `portal` = Portal {posicaoPortal = (0, 0), ondasPortal = [Onda [] 5 0 0]}
* `torre` = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))
* `mapa` = [ [Terra, Relva, Agua, Relva, Terra], [Terra, Terra, Terra, Relva, Agua], [Agua, Relva, Terra, Relva, Relva] ]
* `inimigo` = Inimigo (1, 1) Norte 100 1.0 10 20 [] (0, 0) 0
* `jogo` = Jogo base [portal] [torre] mapa [inimigo] [] Um

>>>
atualizaJogo 0.3 jogo
Jogo {
baseJogo = Base {vidaBase = 100.0, posicaoBase = (2.0,2.0), creditosBase = 50}, 
portaisJogo = [Portal {posicaoPortal = (0.0,0.0), ondasPortal = []}], 
torresJogo = [Torre {posicaoTorre = (5.0,5.0), danoTorre = 25.0, alcanceTorre = 3.0, rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 0.0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}}], 
mapaJogo = [[Terra,Relva,Agua,Relva,Terra],[Terra,Terra,Terra,Relva,Agua],[Agua,Relva,Terra,Relva,Relva]], 
inimigosJogo = [Inimigo {posicaoInimigo = (1.3,1.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (0.0,0.0), tempoInimigo = 0.0}], 
lojaJogo = [], 
nivelJogo = Um }

-}

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t (Jogo base portais torres mapa inimigos loja n) =

    let (portaisAtualizados, novosInimigos) = atualizaPortais t portais inimigos
        (baseAtualizada, inimigosAtualizados) = atualizaInimigos t mapa base novosInimigos
        (torresAtualizadas, inimigosAtingidos) = atualizaTorres t torres inimigosAtualizados


    in Jogo baseAtualizada portaisAtualizados torresAtualizadas mapa inimigosAtingidos loja n


---------------------------------------------------------------------------------------------


-- COMPORTAMENTO DAS TORRES


{- | A função `disparaTorre` faz a torre disparar projeteis contra os inimigos detetados. Utiliza funções auxiliares como a `inimigosNoAlcance` e a `atingeInimigo` da Tarefa 2.

=== Exemplos de Uso:

* `torre` = Torre (5, 5) 25 3 3 2 0 (Projetil Fogo (Finita 7))

* `listaInimigos` = [
            Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0,
            Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0,
            Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (10.0, 10.0) 0
          ]

>>> disparaTorre 0.2 torre1 listaInimigos
(Torre {posicaoTorre = (5.0,5.0), danoTorre = 25.0, alcanceTorre = 3.0, rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 2.0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}},
[Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 75.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}, tempoInimigo = 0.0], posInicial = (3.0,4.0)},Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 55.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 7.0}], posInicial = (7.0,5.0), tempoInimigo = 0.0},Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [], posInicial = (10.0,10.0), tempoInimigo = 0.0}])

-}

disparaTorre :: Tempo -> Torre -> [Inimigo] -> (Torre, [Inimigo])
disparaTorre tempo torre inimigos

            -- A torre ainda não pode disparar (cooldown).
            | tempoTorre torre > 0 = (torre {tempoTorre = max 0 (tempoTorre torre - tempo)}, inimigos)

            -- Sem inimigos no alcance.
            | null (inimigosNoAlcance torre inimigos) = (torre, inimigos)                       -- Utiliza a funcao 'inimigosNoAlcance' da Tarefa2.

            -- A torre pode disparar e tem inimigos no alcance.
            | otherwise = let ini = geraID inimigos                                             -- Lista dos inimigos com ID.
                              iniIDalcance = iniNoAlcanceID torre ini                           -- Seleciona os inimigos que estão ao alcance da torre.
                              iniRajada = take (rajadaTorre torre) iniIDalcance                 -- Seleciona os inimigos que a torre consegue atacar de uma só vez.
                              iniAtingidos = atualizaInimigoID torre iniRajada                  -- Atualiza os inimigos atingidos, utilizando a funcao 'atingeInimigo' da tarefa2.
                              iniRestantes = filtraInimigoID ini iniRajada                      -- Retira os inimigos atingidos, ficando apenas os outros.
                              lista = iniAtingidos ++ iniRestantes                              -- Lista atualizada com todos os inimigos em jogo e respetivas ids.
                              lordenada = sortBy (\(x, _) (y, _) -> compare x y) lista          -- Lista por ordem.


                          in (torre {tempoTorre = cicloTorre torre}, map snd lordenada)




{- | a função `geraID` gera uma id para cada inimigo da lista. 

Implementamos esta função porque os inimigos de uma onda inicialmente têm todos as mesmas características, logo se utilizassemos a 'elem' para retirar, da lista de inimigos, os inimigos que vão ser atingidos pela torre (iniRajada) iriamos ter uma lista no final com menos inimigos do que o suposto. 
Assim, a nossa solução foi inventar uma identidade (atribuindo-lhes um número) para os inimigos, pelo que em vez de comprarmos o inimigo em si, comparamos apenas as suas identidades. 

=== Exemplos de Uso:

* `inimigos` = [Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0, Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0)] 0

>>> geraID inimigos
[(0,Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (3.0,4.0), tempoInimigo = 0.0}),
 (1,Inimigo {posicaoInimigo = (7.0,5.0), direcaoInimigo = Sul, vidaInimigo = 80.0, velocidadeInimigo = 1.0, ataqueInimigo = 15.0, butimInimigo = 30, projeteisInimigo = [], posInicial = (7.0,5.0), tempoInimigo = 0.0})]

-}

geraID :: [Inimigo] -> [(Int, Inimigo)]
geraID = zip [0..]



{- | a função `iniNoAlcanceID` faz o mesmo que a 'inimigosNoAlcance', mas atribuindo id aos inimigos. -}

iniNoAlcanceID :: Torre -> [(Int, Inimigo)] -> [(Int, Inimigo)]
iniNoAlcanceID _ [] = []
iniNoAlcanceID t ((id, ini):l) | dist (posicaoTorre t) (posicaoInimigo ini) <= alcanceTorre t = (id,ini) : iniNoAlcanceID t l
                               | otherwise = iniNoAlcanceID t l

    where dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2


{- | a função `atualizaInimigoID` faz o mesmo que a `atingeInimigo`, mas mantem id dos inimigos. -}

atualizaInimigoID :: Torre -> [(Int, Inimigo)] -> [(Int, Inimigo)]
atualizaInimigoID t = map (\(id, ini) -> (id, atingeInimigo t ini))



{- | a função `filtraInimigoID` retira, da lista inicial, todos os inimigos que foram atingidos pela torre. 

=== Exemplos de Uso:

* `listaInimigos` = [(0, Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0),
                     (1, Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0),
                     (2, Inimigo (10.0, 10.0) Este 50.0 1.5 20.0 40 [] (10.0, 10.0) 0) ]
          
* `listaInimigosAtingidos` = [(0, Inimigo (3.0, 4.0) Norte 100.0 1.0 10.0 20 [] (3.0, 4.0) 0),
                              (1, Inimigo (7.0, 5.0) Sul 80.0 1.0 15.0 30 [] (7.0, 5.0) 0) ]

>>> filtraInimigoID listaInimigos listaInimigosAtingidos
[(2,Inimigo {posicaoInimigo = (10.0,10.0), direcaoInimigo = Este, vidaInimigo = 50.0, velocidadeInimigo = 1.5, ataqueInimigo = 20.0, butimInimigo = 40, projeteisInimigo = [], posInicial = (10.0,10.0), tempoInimigo = 0.0})]

-}

filtraInimigoID :: [(Int, Inimigo)] -> [(Int, Inimigo)] -> [(Int, Inimigo)]
filtraInimigoID linicial latingidos = filter (\(id, _) -> id `notElem` map fst latingidos) linicial



-- função principal

{- | a função `atualizaTorres` faz o mesmo que a 'disparaTorre' mas para todas as torres em jogo. -}

atualizaTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizaTorres _ [] inimigos = ([], inimigos)
atualizaTorres tempo (t:ts) inimigos = let (novaTorre, iniAtual) = disparaTorre tempo t inimigos
                                           (novasTorres, ini) = atualizaTorres tempo ts iniAtual
                                       in (novaTorre : novasTorres, ini)


--------------------------------------------------------------------------------------------------------------------------------------------------------


-- COMPORTAMENTO DOS PORTAIS 


{- | a função `atualizaOnda` atualiza a primeira onda com inimigos. 

=== Exemplos de Uso:

* `onda1` = Onda {inimigosOnda = [inimigo1, inimigo2], cicloOnda = 10, tempoOnda = 5, entradaOnda = 0}
* `onda2` = Onda {inimigosOnda = [], cicloOnda = 10, tempoOnda = 5, entradaOnda = 0}

>>> atualizaOnda 0.2 [onda1]
[Onda {inimigosOnda = [inimigo1, inimigo2], cicloOnda = 10, tempoOnda = 4.8, entradaOnda = 0}]

>>> atualizaOnda 0.2 [onda2]
[]

-}


atualizaOnda :: Tempo -> [Onda] -> [Onda]
atualizaOnda _ [] = []
atualizaOnda tempo (onda:rOndas)

            -- Atualiza a entrada da primeira onda, se ela estiver maior que zero.
            | entradaOnda onda > 0 = let ondaAtual = onda {entradaOnda = entradaOnda onda - tempo}
                                     in ondaAtual : rOndas

            -- Remove a onda, se não tiver mais inimigos.
            | null (inimigosOnda onda) = atualizaOnda tempo rOndas

            -- Atualiza o tempo para o próximo inimigo. 
            | tempoOnda onda > 0 = let ondaAtualizada = onda {tempoOnda = tempoOnda onda - tempo}
                                   in ondaAtualizada : rOndas

             -- Mantém a onda como está.
            | otherwise = onda : rOndas



{- | a função `atualizaPortal` atualiza o portal e a lista de inimigos ativos, utilizando a função `ativaInimigo` da Tarefa 2. 

=== Exemplos de Uso:

* `portal` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda]}
* `onda` = Onda {inimigosOnda = [inimigo], cicloOnda = 10, tempoOnda = 0, entradaOnda = 0}
* `inimigo` = Inimigo (0.5, 0.5) Norte 100.0 1.0 10.0 20 [] (0.5, 0.5) 0
* `inimigos` = [Inimigo (2.5, 3.0) Oeste 100 4 30 23 [Projetil Gelo (Finita 4), Projetil Resina Infinita] (0.5, 0.5) 0]

>>> atualizaPortal 0.2 portal inimigos
(Portal {posicaoPortal = (0.5,0.5), ondasPortal = []},
[Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [], posInicial = (0.5,0.5), tempoInimigo = 0.0},Inimigo {posicaoInimigo = (2.5,3.0), direcaoInimigo = Oeste, vidaInimigo = 100.0, velocidadeInimigo = 4.0, ataqueInimigo = 30.0, butimInimigo = 23, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}], posInicial = (0.5,0.5), tempoInimigo = 0.0}])

-}

atualizaPortal :: Tempo -> Portal -> [Inimigo] -> (Portal, [Inimigo])
atualizaPortal tempo portal iniAtivos =

                let ondasAtual = atualizaOnda tempo (ondasPortal portal)
                    portalAtualizado = portal {ondasPortal = ondasAtual}
                    (novoPortal, novosInimigos) = ativaInimigo portalAtualizado iniAtivos   -- a 'ativaInimigo' é da Tarefa2

                in (novoPortal, novosInimigos)


-- função principal

{- | a função `atualizaPortais` faz o mesmo que a `atualizaPortal`, mas para todos os portais do jogo. -}

atualizaPortais :: Tempo -> [Portal] -> [Inimigo] -> ([Portal], [Inimigo])
atualizaPortais _ [] iniAtivos = ([], iniAtivos)
atualizaPortais tempo (portal:restoPortais) iniAtivos = let (portalAtualizado, novosInimigos) = atualizaPortal tempo portal iniAtivos
                                                            (portaisAtualizados, inimigosFinais) = atualizaPortais tempo restoPortais novosInimigos

                                                        in (portalAtualizado : portaisAtualizados, inimigosFinais)


--------------------------------------------------------------------------------------------------------------------------------------------------------


-- COMPORTAMENTO DOS INIMIGOS


{- | a função `Caminho` devolve um caminho do portal à base. 

=== Exemplos de Uso:

* `portal` = Portal {posicaoPortal = (0.5, 0.5), ondasPortal = [onda]}
* `base` = Base {vidaBase = 100, posicaoBase = (2.2, 2.5), creditosBase = 50}
* `mapa` = [ [Terra, Relva, Agua, Relva, Terra], [Terra, Terra, Terra, Relva, Agua], [Agua, Relva, Terra, Relva, Relva] ]

>>> caminho (0, 0) (2, 2) mapa
[(0,0),(0,1),(1,1),(2,1),(2,2)]

-}

caminho :: (Int, Int) -> (Int, Int) -> Mapa -> [(Int, Int)]
caminho portal base mapa = auxCaminho portal [] base mapa


{- | a função `auxCaminho` auxilia a função 'caminho', utilizando as funções `encontraVizinhos` e `removeVisitados` da Tarefa 1. -}

auxCaminho :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Mapa -> [(Int, Int)]
auxCaminho posAtual visitados base mapa

    -- Chegou à base
    | posAtual == base = [posAtual]

    -- Encontra a próxima posição do caminho e repete o processo até chegar à base.                                           
    | otherwise = let vizinhos = encontraVizinhos posAtual mapa                                            -- Encontra os vizinhos válidos.
                      naoVisitados = removeVisitados vizinhos visitados                                    -- Remove as posições já visitadas.          

                  in posAtual : auxCaminho (head naoVisitados) (posAtual : visitados) base mapa            -- Gera o caminho. 



{- | a função `direcaoToOeste` atualiza a direção do inimigo conforme a sua posição e o mapa, caso o portal esteja à direita da base. 

=== Exemplos de Uso:

>>> direcaoToOeste Norte (3, 2) [(3, 3), (4, 3)]
Sul

>>> direcaoToOeste Oeste (0.5, 1) [(0,1), (0,2)]
Oeste

-}

direcaoToOeste :: Direcao -> Posicao -> [(Int, Int)] -> Direcao
direcaoToOeste d pos p' = case d of

    Norte -> if (z, y) `elem` p' then Norte
             else if (z+1, w) `elem` p' then Este
             else if (z-1, w) `elem` p' then Oeste
             else Sul

    Sul -> if (z, y+1) `elem` p' then Sul
           else if (z+1, y) `elem` p' then Este
           else if (z-1, y) `elem` p' then Oeste
           else Norte

    Este -> if (z+1, y) `elem` p' then Este
            else if (z, y-1) `elem` p' then Norte
            else if (z, y+1) `elem` p' then Sul
            else Oeste

    Oeste -> if (z-1, y) `elem` p' || (z-1, w) `elem` p' then Oeste
             else if (z, y-1) `elem` p' then Norte
             else if (z, y+1) `elem` p' then Sul
             else Este

    where (x,y) = (floor (fst pos), floor (snd pos))            -- Arredonda por defeito.
          (z,w) = (ceiling (fst pos), ceiling (snd pos))        -- Arredonda por excesso.



{- | a função `direcaoToEste` atualiza a direção do inimigo conforme a sua posição e o mapa, caso o portal esteja à esquerda da base. -}

direcaoToEste :: Direcao -> Posicao -> [(Int, Int)] -> Direcao
direcaoToEste d pos p' = case d of

    Norte -> if (x, w-1) `elem` p' then Norte
             else if (x+1, w) `elem` p' then Este
             else if (x-1, w) `elem` p' then Oeste
             else Sul

    Sul -> if (x, y+1) `elem` p' then Sul
           else if (x+1, y) `elem` p' then Este
           else if (x-1, y) `elem` p' then Oeste
           else Norte

    Este -> if (x+1, w) `elem` p' || (x+1, y) `elem` p' then Este
            else if (x, y-1) `elem` p' then Norte
            else if (x, y+1) `elem` p' then Sul
            else Oeste

    Oeste -> if (x-1, y) `elem` p' then Oeste
             else if (x, y-1) `elem` p' then Norte
             else if (x, y+1) `elem` p' then Sul
             else Este

    where (x,y) = (floor (fst pos), floor (snd pos))
          (z,w) = (ceiling (fst pos), ceiling (snd pos))


{- | a função `atualizaPosicao` atualiza a posição do inimigo conforme a sua direcao e velocidade. 

=== Exemplos de Uso:

>>> atualizaPosicao [Projetil Resina Infinita] Norte (2.0, 3.0) 1.0 1.0
(2.0, 2.3)

>>> atualizaPosicao [] Este (1.0, 2.0) 2.0 1.5
(4.0, 2.0)

-}

atualizaPosicao :: [Projetil] -> Direcao -> Posicao -> Tempo -> Float -> Posicao
atualizaPosicao p d (x,y) t v = case d of
    Norte -> if elem Resina tp then (x, y - v * t * 0.7) else (x, y - v * t)
    Sul -> if elem Resina tp then (x, y + v * t * 0.7) else (x, y + v * t)
    Este -> if elem Resina tp then (x + v * t * 0.7, y) else (x + v * t, y)
    Oeste -> if elem Resina tp then (x - v * t * 0.7, y) else (x - v * t, y)

    where tp = map tipoProjetil p 


{- | a função `atualizaProjetil` atualiza o estado dos projéteis com base no tempo e na duração de cada um. 

=== Exemplos de Uso:

>>> atualizaProjetil 1.0 [Projetil Gelo (Finita 5.0), Projetil Resina Infinita]
[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}]

-}

atualizaProjetil :: Tempo -> [Projetil] -> [Projetil]
atualizaProjetil _ [] = []
atualizaProjetil t ((Projetil tipo duracao):ps) = case duracao of

    Finita d -> if d > t then Projetil tipo (Finita (d-t)) : atualizaProjetil t ps
                else atualizaProjetil t ps
    Infinita -> Projetil tipo Infinita : atualizaProjetil t ps



{- | a função `proxMovimento` devolve o proximo movimento do inimigo, isto é, a sua direção e posicao, utilizando as funçoes `atualizaPosicao` e `direcaoToEste` / `direcaoToOeste`. -}

proxMovimento :: Tempo -> Mapa -> Posicao -> Posicao -> Inimigo -> Inimigo
proxMovimento t mapa portal base (Inimigo pos direcao vida velocidade dano dinheiro projeteis posi tempo) =

    let caminhoPortalBase = caminho posPortalInt posBaseInt mapa                               -- Obtém o caminho do portal à base.
        novaDirecao | isPortalesq = direcaoToEste direcao pos caminhoPortalBase
                    | otherwise = direcaoToOeste direcao pos caminhoPortalBase                 -- Nova direção do Inimigo, utilizando a 'atualizaDirecao'.
        novaPosicao = atualizaPosicao projeteis novaDirecao pos t velocidade                   -- Nova posicao do Inimigo, utilizando a 'atualizaPosicao' e a nova direção.        

    in Inimigo novaPosicao novaDirecao vida velocidade dano dinheiro projeteis posi tempo

                where posPortalInt = arredondaPosicao portal
                      posBaseInt = arredondaPosicao base
                      isPortalesq = fst posPortalInt < fst posBaseInt



{- | a função `atualizaEstadoInimigo` atualiza o estado do inimigo (vida, direcao, posicao, velocidade, projeteis) utilizando as funçoes `proxMovimento` e `atualizaProjetil`. 


=== Efeitos dos Projéteis:
   
1. **Gelo**: O inimigo fica parado durante o tempo em que está sob o efeito do gelo.
2. **Fogo**: O inimigo perde 5 de vida por segundo enquanto está sob o efeito do fogo.
3. **Resina**: O inimigo tem a velocidade reduzida 30% (até sair da lista de projeteisAtivos) -- efeito é produzido na `atualizaPosicao`.


=== Exemplos de Uso:

* `base` = Base {vidaBase = 100, posicaoBase = (2.2, 2.5), creditosBase = 50}
* `mapa` = [ [Terra, Relva, Agua, Relva, Terra], [Terra, Terra, Terra, Relva, Agua], [Agua, Relva, Terra, Relva, Relva] ]
* `inimigo1` = Inimigo (0.5, 1.0) Este 100 1.0 5 25 [Projetil Resina Infinita, Projetil Gelo (Finita 2)] (0.5, 0.5) 0
* `inimigo2` = Inimigo (0.5, 1.0) Sul 100 1.0 5 25 [Projetil Resina Infinita] (0.5, 0.5) 0

>>> atualizaEstadoInimigo 0.5 mapa (2.2, 2.5) inimigo1
Inimigo {posicaoInimigo = (0.5,1.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.5}], posInicial = (0.5,0.5), tempoInimigo = 0.0}

>>> atualizaEstadoInimigo 0.5 mapa (2.2, 2.5) inimigo2
Inimigo {posicaoInimigo = (0.85,1.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 0.8, ataqueInimigo = 5.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}], posInicial = (0.5,0.5), tempoInimigo = 0.0}

-}

atualizaEstadoInimigo :: Tempo -> Mapa -> Posicao -> Inimigo -> Inimigo
atualizaEstadoInimigo t mapa basepos inimigo@(Inimigo pos direcao vida velocidade dano dinheiro projeteis posPortal tempo) =

    case projeteis of
        -- O inimigo não tem projéteis.
        [] -> proxMovimento t mapa posPortal basepos inimigo

        -- Caso em que o projétil é o gelo.
        [Projetil Gelo _] -> inimigo {projeteisInimigo = atualizaProjetil t projeteis} -- Fica parado devido ao gelo.

        -- Caso em que o projétil é o fogo.
        [Projetil Fogo _] -> let novaVida = vida - (5 * t) -- Perde 5 de vida por segundo devido ao fogo.
                             in proxMovimento t mapa posPortal basepos inimigo {vidaInimigo = novaVida, projeteisInimigo = atualizaProjetil t projeteis}

        -- Caso em que o projétil é a resina.
        [Projetil Resina _] -> proxMovimento t mapa posPortal basepos inimigo {projeteisInimigo = atualizaProjetil t projeteis}

        -- Caso em que os projéteis são o gelo e a resina.
        ps -> inimigo {projeteisInimigo = atualizaProjetil t projeteis}



{- | a função `removeInimigoHitBase` elimina os inimigos que atingiram a base. 

=== Exemplos de Uso:

* `base` = Base {vidaBase = 100, posicaoBase = (2.2, 2.2), creditosBase = 50}
* `inimigos` = [Inimigo (2, 2) Este 100 1.0 5 25 [Projetil Resina Infinita] (0.5, 0.5) 0, Inimigo (0.5, 1.0) Sul 100 1.0 5 25 [Projetil Gelo (Finita 2)] (0.5, 0.5) 0]

>>> removeInimigoHitBase base inimigos
(Base {vidaBase = 75.0, posicaoBase = (2.2,2.2), creditosBase = 50},
[Inimigo {posicaoInimigo = (0.5,1.0), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}])

-}

removeInimigoHitBase :: Base -> [Inimigo] -> (Base, [Inimigo])
removeInimigoHitBase base [] = (base, [])
removeInimigoHitBase b (i:is)

    | sqrt ((x-x')^2 + (y-y')^2) <= 0.5 = removeInimigoHitBase b {vidaBase = vida - dano} is

    | otherwise = let (b',is') = removeInimigoHitBase b is
                  in (b', i:is')

    where (x,y) = posicaoBase b
          (x',y') = posicaoInimigo i
          vida = vidaBase b
          dano = ataqueInimigo i



{- | a função `removeInimigosSemVida` elimina os inimigos que já não têm vida e adiciona o seu butim aos creditos da base. 

=== Exemplos de Uso:

* `base` = Base {vidaBase = 100, posicaoBase = (2.2, 2.2), creditosBase = 50}
* `inimigos` = [Inimigo (2, 2) Este 0 1.0 5 25 [Projetil Resina Infinita] (0.5, 0.5) 0, Inimigo (0.5, 1.0) Sul 100 1.0 5 25 [Projetil Gelo (Finita 2)] (0.5, 0.5) 0]

>>> removeInimigosSemVida inimigos 50
([Inimigo {posicaoInimigo = (0.5,1.0), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}], posInicial = (0.5,0.5), tempoInimigo = 0.0}],75)

-}

removeInimigosSemVida :: [Inimigo] -> Creditos -> ([Inimigo], Creditos)
removeInimigosSemVida [] c = ([], c)
removeInimigosSemVida (i:is) c | vida <= 0 = removeInimigosSemVida is (c+dinheiro)
                               | otherwise = let (is',c') = removeInimigosSemVida is c
                                             in (i : is', c')

        where vida = vidaInimigo i
              dinheiro = butimInimigo i



-- função principal

{- | a função `atualizaInimigos` atualiza todos os inimigos em jogo, utilizando as funções `removeInimigosSemVida`, `removeInimigoHitBase` e `atualizaEstadoInimigo`. 

Adicionamos um parâmetro (posicão inicial) no datatype do Inimigo. Essa posição corresponde à posição do portal que origina o inimigo,
e serve para saber a que portal (dos vários que podem haver no jogo) corresponde o inimigo, para que este siga o caminho desse portal à base.

-}

atualizaInimigos :: Tempo -> Mapa -> Base -> [Inimigo] -> (Base, [Inimigo])
atualizaInimigos tempo mapa base inimigos = (baseAtualizada, inimigosAtualizados)

    where (inimigosVivos, creditosAtualizados) = removeInimigosSemVida inimigos (creditosBase base)
          (baseAtualizada, inimigosAtingidos) = removeInimigoHitBase (Base (vidaBase base) (posicaoBase base) creditosAtualizados) inimigosVivos

          inimigosAtualizados = map (atualizaEstadoInimigo tempo mapa (posicaoBase base)) inimigosAtingidos
