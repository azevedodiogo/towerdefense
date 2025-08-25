module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3
import Tarefa1


-- | É chamada automaticamente em intervalos definidos no frame rate. Tem comportamentos específicos para os diferentes tipos do ImmutableTowers.

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers

reageTempo t (JogoRun jogo pos compra pz) | ganhouJogo jogo = Vitoria jogo pz
                                          | perdeuJogo jogo = Derrota jogo pz
                                          | otherwise = if validaJogo jogoAtualizado then JogoRun jogoAtualizado pos compra pz  -- verifica se o jogoAtualizado é valido, com a validaJogo da tarefa1
                                                        else error "Estado inválido detectado após atualização de tempo."
                                            
                                          where jogoAtualizado = atualizaJogo t (jogo {inimigosJogo = atualizaTempoInimigos t (inimigosJogo jogo)})

