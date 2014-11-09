
numNode = 100
delta = 10

type Data = Int
data UpdateOp  = Write Data
type Cid  = Int
type Time = Int
type Ts = Int -- Timestamp
type NodeId = Int
-- TODO(ahsank) : Change to association list
type Mpts = [(NodeId, Ts)]
data UpdateReqTime = FarPast | Past | Current | Future

data UpdateReq = UpdateReq {
      ureqprev :: Ts
      , ureqop :: UpdateOp
      , ureqcid :: Cid
      , ureqtime :: UpdateReqTime
}

data AckReq = AckReq {
     acktime :: Time
} | Reject

data LogRecord = Update {
  prev :: Ts
  , op :: UpdateOp
  , cid :: Cid
  , time :: Time
  , opts :: Mpts
} | Ack { cid :: Cid
  , time :: Time
  , opts :: Mpts
}

data Node = Node {
     node :: NodeId
     , nodelog :: [LogRecord]
     , rep_ts :: Mpts
     , val :: Data
     , val_ts :: Mpts
     , inval :: [Cid]
     , ts_table :: [(NodeId, Mpts)] -- MPTS from NodeId j
}

init n =
  Node { node = n, nodelog = [],
  rep_ts = [], val = 0, val_ts = [],
  inval = [], ts_table = []}

update n@(Node {}) (UpdateReq {ureqtime=FarPast}) =
       (Reject, n)

-- TODO(ahsank): Find better way to find element in list of record
-- TODO(ahsank): How to keep Node state implicit instead of returning
update n@(Node {nodelog=nlog, inval=cidlist}) (UpdateReq {ureqcid=c})
       | elem c cidlist = (Reject, n)
       | elem c [cid x | x <- nlog] = (Reject, n)