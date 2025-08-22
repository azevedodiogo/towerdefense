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