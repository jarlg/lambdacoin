module Haskoin.Helpers where

import Haskoin.Types


blocks :: Blockchain -> [Block]
blocks (Genesis _ _) = []
blocks (Node b _ bc) = b : blocks bc

headers :: Blockchain -> [BlockHeader]
headers (Genesis _ _) = []
headers (Node _ h bc) = h : headers bc

-- octet stream to integer primitive; converts the ByteArray to an Integer
-- a lower integer is more difficult, corresponding to the task of finding
-- a nonce giving a hash whose numerical value is lower than the difficulty
-- should only hash header!
difficulty :: BlockHeader -> Integer
difficulty bh = os2ip (hashlazy (encode bh) :: HaskoinHash)

-- want a block to be mined every 10secs!
-- recalculates target difficulty every N=epochSize blocks
target :: Blockchain -> Integer
target (Genesis _ _ ) = genesisDifficulty
target bc@(Node _ h _)
  | epochN == epochSize = round $ factor * (fromIntegral curTarget)
  | otherwise = curTarget
  where curTarget = _target h -- this might overflow!!
        (epochN, epochStart) = epoch curTarget bc
        dt :: NominalDiffTime
        dt = _time h - epochStart
        factor = max 0.25 (dt / epochTime)

-- # of blocks mined and timestamp back to current epoch's beginning
epoch :: Integer -> Blockchain -> (Int, POSIXTime)
epoch _ (Genesis _ t) = (0, t)
epoch tgt bc@(Node _ h parent)
  | _target h == tgt = let (n, ts) = epoch tgt parent in (n+1, ts)
  | otherwise = (0, _time h)

-- a ledger?
balances :: Blockchain -> M.Map Account Integer
balances bc =
  let txns = concat [ txns | txns <- blocks bc ]
      rewards = map reward (headers bc)
  in M.fromListWith (+) (rewards ++ foldl credit [] txns)
  where credit xs (Transaction f t q) =
          (f, -q) : (t, q) : xs
        reward (BlockHeader m _ _ _ _) = (m, miningReward)

-- phantom type to validate?
validate :: Blockchain -> [Transaction] -> [Transaction]
validate bc txns =
  let ledger = balances bc -- important instead of inlining?!
      valid txn = case M.lookup (_from txn) ledger of
        Nothing -> False
        Just balance -> balance >= _amount txn
  in filter valid txns
