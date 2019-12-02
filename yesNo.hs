{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


class YesNo a where 
    yesno :: a -> Bool

instance YesNo Integer where
    yesno num 
       | num <= 0 = False
       | otherwise = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno x = id x

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesnoTrue yesnoFalse = if yesno yesnoVal
                                           then yesnoTrue 
                                           else yesnoFalse