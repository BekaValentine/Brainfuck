module BFState where

import BFTypes

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8






bfMachine :: StateT Machine IO ()
bfMachine = handleInstruction >> continue
  
  where
    getInstructions :: StateT Machine IO InstructionFocus
    getInstructions = fst <$> get
    
    putInstructions :: InstructionFocus -> StateT Machine IO ()
    putInstructions instrs =
      do cells <- getCells
         put (instrs,cells)
    
    getCells :: StateT Machine IO CellFocus
    getCells = snd <$> get
    
    putCells :: CellFocus -> StateT Machine IO ()
    putCells cells =
      do instrs <- getInstructions
         put (instrs,cells)
    
    currentInstruction :: StateT Machine IO Instruction
    currentInstruction =
      do instrs <- getInstructions
         return (focus instrs)
    
    handleInstruction :: StateT Machine IO ()
    handleInstruction =
      do instr <- currentInstruction
         case instr of
           MoveRight -> moveRight
           MoveLeft -> moveLeft
           Increment -> increment
           Decrement -> decrement
           Output -> output
           Input -> input
           JumpForwardIfZero -> jfiz
           JumpBackIfNonZero -> jbinz
    
    continue :: StateT Machine IO ()
    continue =
      do instrs <- getInstructions
         case right instrs of
           Nothing -> return ()
           Just instrs' ->
             do putInstructions instrs'
                bfMachine
    
    moveRight :: StateT Machine IO ()
    moveRight =
      do cells <- getCells
         let Just cells' = right cells
         putCells cells'
    
    moveLeft :: StateT Machine IO ()
    moveLeft =
      do cells <- getCells
         let Just cells' = left cells
         putCells cells'
    
    increment :: StateT Machine IO ()
    increment =
      do cells <- getCells
         putCells (onFocus (+1) cells)
    
    decrement :: StateT Machine IO ()
    decrement =
      do cells <- getCells
         putCells (onFocus (subtract 1) cells)
    
    output :: StateT Machine IO ()
    output =
      do cells <- getCells
         liftIO $ putStr (C8.unpack (BS.pack [focus cells]))
    
    input :: StateT Machine IO ()
    input =
      do x <- liftIO $ getLine
         cells <- getCells
         putCells (onFocus (const (read x)) cells)
    
    jfiz :: StateT Machine IO ()
    jfiz =
      do cells <- getCells
         if 0 == focus cells
            then do
              instrs <- getInstructions
              case matchingJumpBack instrs of
                Nothing -> error "Whoops. No matching ]!"
                Just instrs' -> putInstructions instrs'
            else return ()
    
    jbinz :: StateT Machine IO ()
    jbinz =
      do cells <- getCells
         if 0 == focus cells
            then return ()
            else do
              instrs <- getInstructions
              case matchingJumpForward instrs of
                Nothing -> error "Whoops. No matching [!"
                Just instrs' -> putInstructions instrs'


runBrainfuck :: String -> IO ()
runBrainfuck instrString =
  do _ <- runStateT bfMachine machine
     return ()
  where
    instrs = instrString >>= charToInstr
    machine = (Focused [] (head instrs) (tail instrs), initialCellFocus)