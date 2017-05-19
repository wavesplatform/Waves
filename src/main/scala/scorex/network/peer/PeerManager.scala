package scorex.network.peer

import java.net.InetSocketAddress

import scorex.network._

object PeerManager {

  case class AddPeer(address: InetSocketAddress)

  case class AddToBlacklist(address: InetSocketAddress)

  case class GetRandomPeersToBroadcast(howMany: Int)

  case class ConnectedPeers(peers: Set[ConnectedPeer])

  case object GetAllPeers

  case object GetBlacklistedPeers

  case object GetConnectedPeers

  case object GetConnectedPeersTyped

}
