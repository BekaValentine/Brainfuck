module BFCoalgebra where

import BFTypes

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8






-- | To `step` a `Machine`, we look at the current `Instruction`, and proceed
-- accordingly. E.g., if it's `MoveRight`, we say that the next state is the
-- one derived by moving the `CellFocus` right one.

step :: Machine -> MaybeT IO Machine
step (instrs, cells) =
  case focus instrs of
    MoveRight -> 
      (,) <$> maybeTLift (right instrs)
          <*> maybeTLift (right cells)
    MoveLeft ->
      (,) <$> maybeTLift (right instrs)
          <*> maybeTLift (left cells)
    Increment ->
      (,) <$> maybeTLift (right instrs)
          <*> return (onFocus (+1) cells)
    Decrement ->
      (,) <$> maybeTLift (right instrs)
          <*> return (onFocus (subtract 1) cells)
    Output ->
      do liftIO $ putStr (C8.unpack (BS.pack [focus cells]))
         (,) <$> maybeTLift (right instrs)
             <*> return cells
    Input ->
      do x <- liftIO getLine
         (,) <$> maybeTLift (right instrs)
             <*> return (onFocus (const (read x)) cells)
    JumpForwardIfZero ->
      if 0 == focus cells
         then (,) <$> maybeTLift (matchingJumpBack instrs)
                  <*> return cells
         else (,) <$> maybeTLift (right instrs)
                  <*> return cells
    JumpBackIfNonZero ->
      if 0 == focus cells
         then (,) <$> maybeTLift (right instrs)
                  <*> return cells
         else (,) <$> maybeTLift (matchingJumpForward instrs)
                  <*> return cells




runBrainfuck :: String -> IO ()
runBrainfuck instrString =
  do _ <- unfoldTraceM
            (Focused [] (head instrs) (tail instrs), initialCellFocus)
            step
     return ()-- (map fst ms)
  where
    instrs = instrString >>= charToInstr
    charToInstr '>' = [MoveRight]
    charToInstr '<' = [MoveLeft]
    charToInstr '+' = [Increment]
    charToInstr '-' = [Decrement]
    charToInstr '.' = [Output]
    charToInstr ',' = [Input]
    charToInstr '[' = [JumpForwardIfZero]
    charToInstr ']' = [JumpBackIfNonZero]
    charToInstr _ = []