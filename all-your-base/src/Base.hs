module Base (rebase) where

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 || outputBase < 2 = Nothing
  | any (>=inputBase) inputDigits = Nothing
  | any (<0) inputDigits = Nothing
rebase inputBase outputBase inputDigits = Just $ rebase' outputBase n []
  where n = sum $ zipWith (\a i -> a * inputBase ^ i) (reverse inputDigits) [0..]


rebase' :: Integral a => a -> a -> [a] -> [a]
rebase' toBase 0 digits = digits
rebase' toBase o digits = rebase' toBase (o `div` toBase) ((o `mod` toBase):digits)
