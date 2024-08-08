module Stack where


type Stack a = [a]

push :: a -> Stack a -> Stack a
push a s = a:s

-- modify ::a -> Stack a -> Stack a
-- modify a (Push _ s) = Push a s
-- modify _ Empty      = Empty

pop  :: Stack a -> Maybe (a, Stack a)
pop []     = Nothing
pop (x:xs) = Just (x, xs) 

isEmpty :: Stack a -> Bool
isEmpty [] = True
isEmpty _  = False

peek :: Stack a -> Maybe (a, Stack a)
peek s@(x:xs) = Just (x, s)
peek _ = Nothing