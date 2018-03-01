module Haskoin.Types where


-- Integer is a bignum type; TODO really need newtype wrapper here?
type Account = Integer
type Block = [Transaction]

data Transaction = Transaction
  { _from :: Account
  , _to :: Account
  , _amount :: Integer
  } deriving (Eq, Show, Generic)


-- digest is the output of a hashing function
-- make the case for choosing hash from an Enum vs. string!
type HaskoinHash = Digest SHA1

data BlockHeader = BlockHeader
  { _miner :: Account
  , _parentHash :: HaskoinHash
  , _nonce :: Integer
  , _time :: POSIXTime
  , _target :: Integer
  } deriving (Eq, Show, Generic)

data Blockchain = Genesis Block POSIXTime
                | Node Block BlockHeader Blockchain
                deriving (Generic, Show)

-- read: after performing some IO, I get a list of transactions
type TransactionPool = IO [Transaction]
