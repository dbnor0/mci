module Main where

import Introduction.Syntax
import Introduction.Interpreter (maxPrintArgs)


main :: IO ()
main = do
  let prog = CompoundStmt (AssignStmt "a" (OpExp (NumExp 5) Plus (NumExp 3))) (CompoundStmt (AssignStmt "b" (SeqExp (PrintStmt [IdentExp "a", OpExp (IdentExp "a")  Minus (NumExp 1)]) (OpExp (NumExp 10) Times (IdentExp "a")))) (PrintStmt [IdentExp "b"]))
  print $ maxPrintArgs prog
