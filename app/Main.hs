module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425



       
-- | Responsável por carregar todas as imagens

carregarImagens :: IO Imagens
carregarImagens = do
                    
                -- menu inicial
                menuInicial <- loadBMP "imagens/menuInicial.bmp"

                -- regras
                regras <- loadBMP "imagens/regras.bmp"

                -- creditos
                creditos <- loadBMP "imagens/creditos.bmp"

                -- seleção dos níveis
                niveis <- loadBMP "imagens/niveis.bmp"

                -- pausa
                pausa <- loadBMP "imagens/pausa.bmp"

                -- vitoria
                vitoria <- loadBMP "imagens/vitoria.bmp"

                -- derrota
                derrota <- loadBMP "imagens/derrota.bmp"

                -- proezas
                proeza <- loadBMP "imagens/proeza.bmp"
                proeza1 <- loadBMP "imagens/proeza1.bmp"
                proeza2 <- loadBMP "imagens/proeza2.bmp"