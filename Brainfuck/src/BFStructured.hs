module BFStructured where

import BFTypes (unfoldTraceM)

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8





-- | An `Instruction` in this version is basically the same as in the normal
-- versions, except for the looping construct, which is a single unified
-- instruction here, rather than two delimiaters as in the normal version.

data Instruction = MoveRight
                 | MoveLeft
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | Loop Sequence
  deriving (Show)


-- | A `Sequence` is just a sequence of simple instructions. We don't have
-- sequences of sequences because that's just a sequence.

data Sequence = Sequence [Instruction]
  deriving (Show)


-- | An `InstructionStack` is either the top of a program that would be an
-- `Instruction`, or it's in a `Loop` with a `SequenceStack` for the program
-- above it.

data InstructionStack = InstructionTop
                      | InLoop SequenceStack
  deriving (Show)


-- | A `SequenceStack` is either the top of a program that would be a
-- sequence, or it's in a `Sequence` with some previously visited
--  `Instruction`s, the `InstructionStack` for the program above it, and then
-- some to-be-visited `Instruction`s.

data SequenceStack = SequenceTop
                   | InSequence [Instruction] InstructionStack [Instruction]
  deriving (Show)


-- A `ProgramFocus` is either an `Instruction` in the context of some
-- `SequenceStack`, or it's a `Sequence` in the context of some
-- `InstructionStack`.

data ProgramFocus = AtInstruction SequenceStack Instruction
                  | AtSequence InstructionStack Sequence
  deriving (Show)




-- | In a bidirectional zipper, we can move in one of two `Direction`s:
-- `Down` into an expression to execution, or `Up` out of an expression just
-- executed.

data Direction = Down | Up
  deriving (Show)


-- The `Memory` of the machine is a sequence of bytes, with one in focus.

data Memory = Memory [Word8] Word8 [Word8]
  deriving (Show)



-- | We can get the focused byte of the machine's memory.

memoryFocus :: Memory -> Word8
memoryFocus (Memory _ x _) = x


-- | We can set the focused byte of the machine's memory.

memorySet :: Word8 -> Memory -> Memory
memorySet x (Memory ls _ rs) = Memory ls x rs


-- | We can move the focus right one. If there's nothing to the right, we can
-- still move right by adding a new byte set to 0.

memoryRight :: Memory -> Memory
memoryRight (Memory ls x []) = Memory (x:ls) 0 []
memoryRight (Memory ls x (r:rs)) = Memory (x:ls) r rs


-- | We can move the focus left one. If there's nothing to the right, we can
-- still move right by adding a new byte set to 0.

memoryLeft :: Memory -> Memory
memoryLeft (Memory [] x rs) = Memory [] 0 (x:rs)
memoryLeft (Memory (l:ls) x rs) = Memory ls l (x:rs)


-- | We can increment the byte in focus.

memoryIncrement :: Memory -> Memory
memoryIncrement (Memory ls x rs) = Memory ls (x + 1) rs


-- | We can decrement the byte in focus.

memoryDecrement :: Memory -> Memory
memoryDecrement (Memory ls x rs) = Memory ls (x - 1) rs


-- | The `MachineState` of Brainfuck is just a triple consisting of the
-- direction that we're moving in the program, together with the focused part
-- of the program, and the memory to manipulate.

type MachineState = (Direction, ProgramFocus, Memory)


-- | We can define a transition function for the BF abstract machine by giving
-- a stepper that produces a possibly-failing IO action for the next state.

step :: MachineState -> MaybeT IO MachineState
step (Down, AtInstruction ps MoveRight, mem) =
  return (Up, AtInstruction ps MoveRight, memoryRight mem)
step (Down, AtInstruction ps MoveLeft, mem) =
  return (Up, AtInstruction ps MoveLeft, memoryLeft mem)
step (Down, AtInstruction ps Increment, mem) =
  return (Up, AtInstruction ps Increment, memoryIncrement mem)
step (Down, AtInstruction ps Decrement, mem) =
  return (Up, AtInstruction ps Decrement, memoryDecrement mem)
step (Down, AtInstruction ps Output, mem) =
  do liftIO $ putStr (C8.unpack (BS.pack [memoryFocus mem]))
     return (Up, AtInstruction ps Output, mem)
step (Down, AtInstruction ps Input, mem) =
  do i <- liftIO $ getLine
     return (Up, AtInstruction ps Input, memorySet (read i) mem)
step (Down, AtInstruction ps (Loop p), mem) =
  return (Down, AtSequence (InLoop ps) p, mem)
step (Down, AtSequence is (Sequence []), mem) =
  return (Up, AtSequence is (Sequence []), mem)
step (Down, AtSequence is (Sequence (i:is')), mem) =
  return (Down, AtInstruction (InSequence [] is is') i, mem)
step (Up, AtInstruction SequenceTop i, mem) =
  MaybeT (return Nothing)
step (Up, AtInstruction (InSequence isl isup []) i, mem) =
  return (Up, AtSequence isup (Sequence (reverse (i:isl))), mem)
step (Up, AtInstruction (InSequence isl isup (i':isr)) i, mem) =
  return (Down, AtInstruction (InSequence (i:isl) isup isr) i', mem)
step (Up, AtSequence InstructionTop p, mem) =
  MaybeT (return Nothing)
step (Up, AtSequence (InLoop ps) p, mem) =
  if 0 == memoryFocus mem
     then return (Up, AtInstruction ps (Loop p), mem)
     else return (Down, AtSequence (InLoop ps) p, mem)




runBrainfuck :: Sequence -> IO ()
runBrainfuck p =
  do _ <- unfoldTraceM machineState step
     return ()
  where
    machineState = (Down, AtSequence InstructionTop p, Memory [] 0 [])