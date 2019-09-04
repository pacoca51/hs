and' :: Bool -> Bool -> Bool
and' True True = True
and' False _ = False
and' _ False = False

imp :: Bool -> Bool -> Bool
imp True False = False
imp True _ = True
imp _ True = True