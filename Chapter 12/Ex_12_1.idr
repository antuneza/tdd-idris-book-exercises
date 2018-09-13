import Control.Monad.State

update: (stateType -> stateType) -> State stateType ()
update f = do
  x <- get
  put (f x)

increase : Nat -> State Nat ()
increase x = update (+x)

data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

countEmpty: Tree a -> State Nat ()
countEmpty Empty = increase 1
countEmpty (Node left val right) = do
  countEmpty left
  countEmpty right

countEmptyNode: Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = update (\(ec, nc) => (S ec,nc))
countEmptyNode (Node left val right) = do
  countEmptyNode left
  update (\(ec, nc) => (ec, S nc))
  countEmptyNode right
