discard """
  matrix: "--gc:orc -d:release"
"""

# bug #18240
import tables

type
  TopicHandler* = proc(topic: string,
                       data: seq[byte]) {.gcsafe, raises: [Defect].}

  PeerID* = object
    data*: seq[byte]

  PeerInfo* = ref object of RootObj
    peerId*: PeerID

  Connection* = ref object of RootObj
    peerInfo*: PeerInfo

  PubSubPeer* = ref object of RootObj
    codec*: string

  PubSub* = ref object of RootObj
    topics*: Table[string, seq[TopicHandler]]
    peers*: Table[PeerID, PubSubPeer]

proc getOrCreatePeer*(myParam: PubSub, peerId: PeerID, protos: seq[string]): PubSubPeer =
  myParam.peers.withValue(peerId, peer):
    return peer[]

method handleConn*(myParam: PubSub,
                  conn: Connection,
                  proto: string) {.base.} =
  myParam.peers.withValue(conn.peerInfo.peerId, peer):
    let peerB = peer[]
