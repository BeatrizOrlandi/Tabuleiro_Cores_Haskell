module Lib where

import Data.Maybe

{- 
Para todos os exercícios, você pode criar funções
auxiliares se achar conveniente. Não se esqueça de dar
nome aos parâmetros que for utilizar.

Considere o seguinte tipo de dado algébrico
que representa 4 cores.
-}


data Color = Red | Green | Blue | Yellow
            deriving (Eq)

-- Exemplo de valor
c1 :: Color
c1 = Yellow

{-
Por mera questão visual, definiremos que a forma 
de apresentação delas se dará pela primeira letra da cor, 
em maiúsculo. Red será "R", Green será "G", e assim por diante.

Exercício 1: Termine a instância de Show abaixo.
Não se esqueça de apagar o undefined.
-}

instance Show Color where
    show Red    = "R"
    show Green  = "G"
    show Blue   = "B"
    show Yellow = "Y"

{-
Considere o seguinte sinônimo de tipo
que representa uma espécie de tabuleiro
de cores. Este "tabuleiro" não necessariamente é
uma matriz quadrada.
-}

type Board = [[Color]]

-- Exemplo de tabuleiro
t1 :: Board
t1 = [[Red, Blue, Blue, Green], 
      [Yellow, Red], 
      [Blue, Green, Red]]

{-
Exercício 2: Implemente a seguinte função
que deve trocar todas as ocorrências da primeira
cor no tabuleiro pela segunda cor, mantendo todas 
as outras cores inalteradas.
-}

fill :: Color -> Color -> Board -> Board
fill x y [] = []
fill x y (t:tab) = comparaLinha t x y : fill x y tab

comparaLinha :: [Color] -> Color -> Color -> [Color]
comparaLinha [] _ _ = []
comparaLinha (t:tab) x y | (t == x) = y : comparaLinha tab x y
                         | otherwise = t : comparaLinha tab x y

{-
Exercício 3: Implemente a seguinte função que deve 
retornar o número de ocorrências de uma cor no tabuleiro.


countColor :: Color -> Board -> Int
countColor _ [] = 0
countColor x (t:tab) | (t == x) = 1 + countColor x tab
                     | otherwise = countColor x tab


Exercício 4: Implemente a seguinte função que deve 
converter uma letra na cor correspondente. Estaremos 
considerando a possibilidade do caractere informado
não representar uma cor.


readColor :: Char -> Maybe Color
readColor x | (x == Color) = 


Exercício 5: Implemente a seguinte função que deve 
converter uma sequência de caracteres numa lista de 
possíveis cores correspondentes.


readColors :: String -> [Maybe Color]
readColors = undefined

-- readColors "BBHYGB" ~= [Just B, Just B, Nothing, Just Y, Just G, Just B] 


Exercício 6: Implemente a seguinte função que deve converter
uma lista de sequências de caracteres num tabuleiro de possíveis
cores.


readColorLines :: [String] -> [[Maybe Color]]
readColorLines = undefined


    readColorLines ["BBHYGB", "JYG", "BKKGBGY"]
       ~= [[Just B,Just B,Nothing,Just Y,Just G,Just B],
           [Nothing,Just Y,Just G],
           [Just B,Nothing,Nothing,Just G,Just B,Just G,Just Y]]



Exercício 7: Implemente a seguinte função que deve converter
um tabuleiro de possíveis cores em um tabuleiro comum, simplesmente
eliminando todas as cores invalidadas no processo.


createBoard :: [[Maybe Color]] -> Board
createBoard = undefined


    createBoard (readColorLines ["BBHYGB", "JYG", "BKKGBGY"])
        ~= [[B,B,Y,G,B],
            [Y,G],
            [B,G,B,G,Y]]



Exercício 8: Implemente a seguinte função que lê um número n 
digitado do teclado e depois lê n linhas, retornando-as em uma lista.


readLines :: IO [String]
readLines = undefined


Exercício 9: Implemente a seguinte função que mostra na tela
a contagem de cada uma das cores, exibindo inclusive as cores
cuja contagem for zero.


printCounters :: Board -> IO ()
printCounters = undefined


Exercício 10: Vá ao arquivo Main.hs e faça o que se pede.
-}