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
                proeza3 <- loadBMP "imagens/proeza3.bmp"
                proeza12 <- loadBMP "imagens/proeza12.bmp"
                proeza13 <- loadBMP "imagens/proeza13.bmp"
                proeza23 <- loadBMP "imagens/proeza23.bmp"
                proeza123 <- loadBMP "imagens/proeza123.bmp"


                -- torres
                tfogo1 <- loadBMP "imagens/tfogo1.bmp"
                tgelo1 <- loadBMP "imagens/tgelo1.bmp"
                tresina1 <- loadBMP "imagens/tresina1.bmp"
                tfogo2 <- loadBMP "imagens/tfogo2.bmp"
                tgelo2 <- loadBMP "imagens/tgelo2.bmp"
                tresina2 <- loadBMP "imagens/tresina2.bmp"
                block <- loadBMP "imagens/block.bmp"

                -- portais
                pdir <- loadBMP "imagens/portaldir.bmp"
                pesq <- loadBMP "imagens/portalesq.bmp"

                -- base
                base <- loadBMP "imagens/base.bmp"

                -- inimigos
                iniLdpa <- loadBMP "imagens/iniLdpa.bmp"
                iniLdpp <- loadBMP "imagens/iniLdpp.bmp"
                iniLdpf <- loadBMP "imagens/iniLdpf.bmp"
                iniLepa <- loadBMP "imagens/iniLepa.bmp"
                iniLepp <- loadBMP "imagens/iniLepp.bmp"
                iniLepf <- loadBMP "imagens/iniLepf.bmp"
                iniUppa <- loadBMP "imagens/iniUppa.bmp"
                iniUppp <- loadBMP "imagens/iniUppp.bmp"
                iniUppf <- loadBMP "imagens/iniUppf.bmp"
                iniDownpa <- loadBMP "imagens/iniDownpa.bmp"
                iniDownpp <- loadBMP "imagens/iniDownpp.bmp"
                iniDownpf <- loadBMP "imagens/iniDownpf.bmp"

                -- relva