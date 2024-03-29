module Main (main) where

import Lib

{-
Faça os 9 exercícios no arquivo Lib.hs e depois volte à este.

EXERCÍCIO 10
Combine tudo que fez neste projeto e implemente
a seguinte função que deve: 
  1. Ler um número n do usuário.
  2. Ler n linhas.
  3. Mostrar o tabuleiro correspondente, 
     que ignorará os caracteres inválidos.
  4. Mostrar a contagem das cores no tabuleiro.
  5. Mostrar o tabuleiro correspondente trocando todos 
     os vermelhos por amarelos.
-}

main :: IO ()
main = do
          x <- readLines
          print (createBoard (readColorLines x))
          printCounters (createBoard (readColorLines x))
          print(fill Red Yellow (createBoard (readColorLines x)))
          


{-
EXERCÍCIO OPCIONAL
Incremente esta aplicação com funcionalidades adicionais. 
Seja criativo e mantenha a boa qualidade do seu código 
e do estilo funcional. Escreva as funções no Lib.hs
e adapte a interação com o usuário no Main.hs.
-}