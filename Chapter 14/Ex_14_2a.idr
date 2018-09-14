data Access = LoggedOut | LoggedIn
data PwdCheck = Correct | Incorrect

data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where
  Password : String -> ShellCmd PwdCheck access_st
             (\pwd => case pwd of
               Correct => LoggedIn
               Incorrect => LoggedOut)
  Logout : ShellCmd () LoggedIn (const LoggedOut)
  GetSecret : ShellCmd String LoggedIn (const LoggedIn)

  PutStr : String -> ShellCmd () state (const state)
  Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn
  (>>=) : ShellCmd a st1 st2_fn -> ((res :a) -> ShellCmd b (st2_fn res) st3_fn) -> ShellCmd b st1 st3_fn

session : ShellCmd () LoggedOut (const LoggedOut)
session = do
  Correct <- Password "wurzerl" | Incorrect => PutStr "Wrong password"
  msg <- GetSecret
  PutStr ("Secret: " ++ show msg ++ "\n")
  Logout

{-
sessionBad : ShellCmd () LoggedOut (const LoggedOut)
sessionBad = do
  Password "wurzerl"
  msg <- GetSecret
  PutStr ("Secret: " ++ show msg ++ "\n")
  Logout

noLogout : ShellCmd () LoggedOut (const LoggedOut)
noLogout = do
  Correct <- Password "wurzerl" | Incorrect => PutStr "Wrong password"
  msg <- GetSecret
  PutStr ("Secret: " ++ show msg ++ "\n")
-}
