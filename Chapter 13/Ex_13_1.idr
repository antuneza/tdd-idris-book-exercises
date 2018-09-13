data DoorState = DoorClosed | DoorOpen

data DoorCmd : Type -> DoorState -> DoorState -> Type where
  Open : DoorCmd () DoorClosed DoorOpen
  Close : DoorCmd () DoorOpen DoorClosed
  RingBell : DoorCmd () st st
  Pure : ty -> DoorCmd ty state state
  (>>=) : DoorCmd a st1 st2 ->
          (a -> DoorCmd b st2 st3) ->
          DoorCmd b st1 st3

doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do RingBell
              Open
              RingBell
              Close

namespace guessGame
  data GuessCmd : Type -> Nat -> Nat -> Type where
    Try : Integer -> GuessCmd Ordering (S tries) tries
    Pure: ty -> GuessCmd ty st st
    (>>=) : GuessCmd a st1 st2 ->
            (a -> GuessCmd b st2 st3) ->
            GuessCmd b st1 st3

threeGuesses : GuessCmd () 3 0
threeGuesses = do
  Try 10
  Try 20
  Try 25
  Pure ()

{-
noGuesses : GuessCmd () 0 0
noGuesses = do
  Try 10
  Pure ()
-}

data Matter = Solid | Liquid | Gas

namespace matterGame
  data MatterCmd : Type -> Matter -> Matter -> Type where
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    Freeze : MatterCmd () Liquid Solid
    Pure : ty -> MatterCmd ty state state
    (>>=) : MatterCmd a st1 st2 ->
            (a -> MatterCmd b st2 st3) ->
            MatterCmd b st1 st3

iceSteam : MatterCmd () Solid Gas
iceSteam = do
  Melt
  Boil

steamIce : MatterCmd () Gas Solid
steamIce = do
  Condense
  Freeze

{-
overMelt : MatterCmd () Solid Gas
overMelt = do
  Melt
  Melt
-}
