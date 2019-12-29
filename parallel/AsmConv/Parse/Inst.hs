module AsmConv.Parse.Inst (
    Inst(..), InstKind(..), toInsts
) where

data InstKind = IkInst (String, [String]) | IkLabel | IkOther
    deriving (Eq, Show)

data Inst = Inst {
    iInst :: String,
    iKind :: InstKind,
    iRow :: Int
} deriving (Show)

toInsts :: String -> [Inst]
toInsts input = zipWith toInst [0 ..] $ lines input

toInst :: Int -> String -> Inst
toInst row str = Inst {
    iInst = str,
    iKind = toInstKind str,
    iRow = row
}

toInstKind :: String -> InstKind
toInstKind str =
    if length strs == 0 then
        IkOther
    else if isLabelStr then
        IkLabel
    else
        IkInst (head strs, tail strs)
    where
        strs = words $ removeComment str
        isLabelStr = length strs == 1 && last (head strs) == ':'

removeComment :: String -> String
removeComment = takeWhile (/= '#')
