VendState : Type
VendState = (Nat, Nat)

data Input = COIN | VEND | CHANGE | REFILL Nat
data CoinResult = Inserted | Rejected

data MachineCmd : (ty : Type) -> VendState -> (ty -> VendState) -> Type where
  InsertCoin : MachineCmd CoinResult (pounds, chocs)
      (\x => case x of
             Inserted => (S pounds, chocs)
             Rejected => (pounds, chocs))
  Vend : MachineCmd () (S pounds, S chocs) (const (pounds, chocs))
  GetCoins : MachineCmd () (pounds, chocs) (const (Z, chocs))
  Refill : (bars: Nat) -> MachineCmd() (Z, chocs) (const (Z, bars+chocs))
  Display: String -> MachineCmd() st (const st)
  GetInput : MachineCmd (Maybe Input) st (const st)
  Pure : (res: ty) -> MachineCmd ty (st_fn res) st_fn
  (>>=) : MachineCmd a st1 st2_fn -> ((res:a) -> MachineCmd b (st2_fn res) st3_fn) -> MachineCmd b st1 st3_fn

data MachineIO : VendState -> Type where
  Do : MachineCmd a st1 st2_fn -> ((res:a) -> Inf(MachineIO (st2_fn res))) -> MachineIO st1

namespace MachineDo
    (>>=) : MachineCmd a st1 st2_fn -> ((res:a) -> Inf(MachineIO (st2_fn res))) -> MachineIO st1
    (>>=) = Do

mutual
  vend : MachineIO (pounds, chocs)
  vend {pounds = S p} {chocs = S c} = do
    Vend
    Display "Enjoy!"
    machineLoop
  vend {pounds = Z} = do
    Display "Insert a coin"
    machineLoop
  vend {chocs = Z} = do
    Display "Out of stock"
    machineLoop

  refill : (num : Nat) -> MachineIO (pounds, chocs)
  refill {pounds = Z} num = do
    Refill num
    machineLoop
  refill _ = do
    Display "Can't refill: Coins in the machine"
    machineLoop

  machineLoop : MachineIO (pounds, chocs)
  machineLoop = do
    Just x <- GetInput
    | Nothing => do
      Display "Invalid Input"
      machineLoop
    case x of
          COIN => do
            res <- InsertCoin
            case res of
              Inserted => do
                Display "Coin Inserted"
                machineLoop
              Rejected => do
                Display "Coin Rejected"
                machineLoop
          VEND => vend
          CHANGE => do
            GetCoins
            Display "Change Returned"
            machineLoop
          (REFILL k) => refill k
