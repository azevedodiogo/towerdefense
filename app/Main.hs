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