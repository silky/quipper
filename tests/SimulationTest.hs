-- This file is part of Quipper. Copyright (C) 2011-2016. Please see the
-- file COPYRIGHT for a list of authors, copyright holders, licensing,
-- and other details. All rights reserved.
-- 
-- ======================================================================

import Quipper
import QuipperLib.Simulation
import System.Random

circuit :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
circuit (q,r) = do
  with_computed code $ \x -> do
    qnot r `controlled` [r,x]
    hadamard r
  return (q,r)

  where
    code = do
      s <- cinit False
      s <- prepare s
      hadamard q
      qnot s `controlled` q
      

circuit2 :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
circuit2 (s,q,r) = do
  circuit (q,r) `controlled` s
  return (s,q,r)

main =
  print_simple Preview circuit2
  
main2 = do
  g <- newStdGen
  print $ run_generic g (0.0 :: Double) circuit2 (True,True,True)
