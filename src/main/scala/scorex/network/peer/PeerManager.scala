package scorex.network.peer

import scorex.network._

object PeerManager {

  case class GetRandomPeersToBroadcast(howMany: Int)

  case class ConnectedPeers(peers: Set[ConnectedPeer])

  case object GetAllPeers

  case object GetBlacklistedPeers

  case object GetConnectedPeers
}
