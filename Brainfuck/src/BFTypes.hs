module BFTypes where
  
import Control.Monad
import Control.Monad.Trans.Maybe








import Data.Word



maybeTLift :: Monad m => Maybe a -> MaybeT m a
maybeTLift x = MaybeT (return x)

unfoldTraceM :: Monad m => s -> (s -> MaybeT m s) -> m [s]
unfoldTraceM s f =
  do ms' <- runMaybeT (f s)
     case ms' of
       Nothing -> return [s]
       Just s' ->
         do ss <- unfoldTraceM s' f
            return (s:ss)




-- | A `Focused a` is a (potentially infinite) sequence of `a`s with one
-- in focus. It is a zipper for `[a]`.

data Focused a = Focused { lefts :: [a], focus :: a, rights :: [a] }
  deriving (Show)


-- | We can move a `Focused a` to the right one space precisely when there is
-- an element to move to. Otherwise we crash.

right :: Focused a -> Maybe (Focused a)
right (Focused ls x (r:rs)) = Just (Focused (x:ls) r rs)
right _ = Nothing


-- | We can move a `Focused a` to the left one space precisely when there is
-- an element to move to. Otherwise we crash.

left :: Focused a -> Maybe (Focused a)
left (Focused (l:ls) x rs) = Just (Focused ls l (x:rs))
left _ = Nothing


-- | We can apply a function to transform the focused cell.

onFocus :: (a -> a) -> Focused a -> Focused a
onFocus f (Focused ls x rs) = Focused ls (f x) rs






-- | A `Cell` is an `Integer`. The standard definition of cells in Brainfuck
-- is that it's a byte, but this is more general and elegant, and also
-- more useful in implementing things.

type Cell = Word8





-- | A `CellFocus` is a zipper into a position in a sequence of cells that's
--  infinite in both directions. The standard definition for Brainfuck is to
-- have the memory of the machine be an array of at least 30k cells. Having
-- it be infinite in both directions makes it less ambiguous what should
-- happen when we increment or decrement the data pointer beyond the ends.
-- With infinite-in-both-directions memory, there are no ends, so we can
-- always increment and decrement as much as we'd like. This type takes the
-- place of a data array and data pointer.

type CellFocus = Focused Cell


-- | The initial `CellFocus` is infinite zeros in both directions, focused
-- of course on a zero.

initialCellFocus :: CellFocus
initialCellFocus = Focused (repeat 0) 0 (repeat 0)





-- | We represent Brainfuck's instructions by a custom data type `Instruction`

data Instruction = MoveRight
                 | MoveLeft
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | JumpForwardIfZero
                 | JumpBackIfNonZero
  deriving (Show)


-- | We can convert from a string to the corresponding `Instruction`s

charToInstr :: Char -> [Instruction]
charToInstr '>' = [MoveRight]
charToInstr '<' = [MoveLeft]
charToInstr '+' = [Increment]
charToInstr '-' = [Decrement]
charToInstr '.' = [Output]
charToInstr ',' = [Input]
charToInstr '[' = [JumpForwardIfZero]
charToInstr ']' = [JumpBackIfNonZero]
charToInstr _ = []


-- | An `InstructionFocus` represents the current instruction being processed,
-- together with the rest of the program around it. This takes the place of
-- an instruction array and instruction pointer.

type InstructionFocus = Focused Instruction


-- | We can shift an `InstructionFocus` to the right until it focuses on the
-- right square bracket corresponding to the current focus.

matchingJumpBack :: InstructionFocus -> Maybe InstructionFocus
matchingJumpBack instrs = go 0 instrs
  where
    go :: Integer -> InstructionFocus -> Maybe InstructionFocus
    go 1 instrs@Focused { focus = JumpBackIfNonZero } =
      Just instrs
    go n instrs@Focused { focus = JumpBackIfNonZero } =
      go (n-1) =<< right instrs
    go n instrs@Focused { focus = JumpForwardIfZero } =
      go (n+1) =<< right instrs
    go n instrs = go n =<< right instrs


-- | We can shift an `InstructionFocus` to the left until it focuses on the
-- left square bracket corresponding to the current focus.

matchingJumpForward :: InstructionFocus -> Maybe InstructionFocus
matchingJumpForward instrs = go 0 instrs
  where
    go :: Integer -> InstructionFocus -> Maybe InstructionFocus
    go 1 instrs@Focused { focus = JumpForwardIfZero } =
      Just instrs
    go n instrs@Focused { focus = JumpForwardIfZero } =
      go (n-1) =<< left instrs
    go n instrs@Focused { focus = JumpBackIfNonZero } =
      go (n+1) =<< left instrs
    go n instrs = go n =<< left instrs





-- | A `Machine` is a pair of an `InstructionFocus` and a `CellFocus`.

type Machine = (InstructionFocus, CellFocus)