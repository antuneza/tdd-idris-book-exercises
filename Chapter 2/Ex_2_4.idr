palindrome : Nat -> String -> Bool
palindrome n str = let lowstr = toLower str in
                       length lowstr > n && lowstr == (reverse lowstr)

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten str = take 10 (reverse (sort str))

over_length  : Nat -> List String -> Nat
over_length k xs = let ls = map length xs in
                       length (filter (> k) ls)
