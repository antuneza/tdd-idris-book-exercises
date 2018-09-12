module Main

import Data.Vect

infixr 5 .+.

data Schema = SString | SInt | SChar | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store: DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
  SetSchema : (newschema: Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get: Maybe Integer -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input) where
  getQuoted : List Char -> Maybe (String, String)
  getQuoted ('"' :: xs) = case span (/= '"') xs of
                            (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                            _ => Nothing
  getQuoted _ = Nothing

parsePrefix SInt input = case span isDigit input of
                          ("", rest) => Nothing
                          (num, rest) => Just (cast num, ltrim rest)

parsePrefix SChar input = case unpack input of
                            (x::xs) => Just (x, ltrim (pack xs))
                            [] => Nothing

parsePrefix (x .+. y) input = do
  (x_val, input') <- parsePrefix x input
  (y_val, input'') <- parsePrefix y input'
  Just ((x_val, y_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                Just (res, "") => Just res
                                Just _ => Nothing
                                Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                [] => Just SString
                                _ => do
                                  xs_sch <- parseSchema xs
                                  Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
                                [] => Just SInt
                                _ => do
                                  xs_sch <- parseSchema xs
                                  Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs) = case xs of
                                [] => Just SChar
                                _ => do
                                  xs_sch <- parseSchema xs
                                  Just (SChar .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = do
  restok <- parseBySchema schema rest
  Just (Add restok)
parseCommand schema "get" "" = Just (Get Nothing)
parseCommand schema "get" val = case all isDigit (unpack val) of
                          False => Nothing
                          True => Just (Get (Just (cast val)))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = do
  schemaok <- parseSchema (words rest)
  Just (SetSchema schemaok)
parseCommand schema _ _ = Nothing

parse: (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (l, r) = display l ++ ", " ++ display r

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
                      Nothing => Just ("Out of range\n", store)
                      Just idx => Just(display (index idx (items store)) ++ "\n", store)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                           Z => Just (MkData schema _ [])
                           S k => Nothing

displayAll : Nat -> Vect size (SchemaType schema) -> String
displayAll k [] = ""
displayAll k (x :: xs) = let rest = displayAll (k + 1)  xs in
                          show k ++ ": " ++ display x ++ "\n" ++ rest

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                          Nothing => Just ("Invalid command\n", store)
                          Just (SetSchema schema') => case setSchema store schema' of
                                                        Nothing => Just ("Can't update schema\n", store)
                                                        Just store' => Just ("OK\n", store')
                          Just (Add x) => Just(" ID " ++ show (size store) ++ "\n", addToStore store x)
                          Just (Get (Just x)) => getEntry x store
                          Just (Get Nothing) => Just(displayAll 0 (items store), store)
                          Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
