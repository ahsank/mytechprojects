import Data.List

numNode = 100
delta = 10

type Data = Int
data UpdateOp  = Write Data
type Cid  = Int
type Time = Int
type Ts = Int -- Timestamp
type NodeId = Int
type NodeTs = (NodeId, Ts)
-- TODO(ahsank) : Change to association list
type Mpts = [NodeTs]
data UpdateReqTime = FarPast | Past | Current | Future

data UpdateReq = UpdateReq {
  ureqPrev :: Mpts
  , ureqop :: UpdateOp
  , ureqcid :: Cid
  , ureqtime :: UpdateReqTime
}

data AckReq = AckReq {
  acktime :: Time
}| Reject | Wait

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
  , selfts :: Ts
  , nodelog :: [LogRecord]
  , rep_ts :: Mpts --for node id != this node id
  , val :: Data
  , val_ts :: Mpts
  , inval :: [Cid]
  , ts_table :: [(NodeId, Mpts)] -- MPTS from NodeId j
}

init n =
  Node { node = n, selfts = 0, nodelog = [],
  rep_ts = [], val = 0, val_ts = [],
  inval = [], ts_table = []}

getTs i a_mpts =
  case find (\x-> fst x == i) a_mpts of
    Just t -> snd t
    Nothing -> 0

replaceTs :: NodeId -> Ts -> Mpts -> Mpts

replaceTs i newts [] = [(i, newts)]

replaceTs i newts (x:rest)
  | i == fst x = (i, newts):rest
  | otherwise = x:replaceTs i newts rest

isTsLE :: Mpts -> Mpts -> Bool

isTsLE [] tsB = True

isTsLE ((n,ts):xs) tsB =
  if getTs n tsB < ts then False else isTsLE xs tsB

mergeTs [] tsB = tsB

update n@(Node {}) (UpdateReq {ureqtime=FarPast}) =
       (Reject, n)

-- TODO(ahsank): Find better way to find element in list of record
-- TODO(ahsank): How to keep Node state implicit instead of returning
update n@(Node {node=ni, selfts=nts, rep_ts=rts, nodelog=nlog, inval=cidlist})
  u@(UpdateReq {ureqcid=c, ureqPrev=uprev})
  | elem c cidlist = (Reject, n) -- Late update
  | elem c [cid x | x <- nlog] = (Reject, n) -- Duplicate request
  | not (isTsLE uprev ((ni,nts):rts)) = (Wait, n) -- Update ts > Node ts
  | otherwise =
    let newts = nts + 1
        newUpdTs = replaceTs ni newts uprev
             -- updRecord = makeUpdateRecord u n newUpdTs
             -- newLog = updRecord : nlog
    in (Reject, n)
