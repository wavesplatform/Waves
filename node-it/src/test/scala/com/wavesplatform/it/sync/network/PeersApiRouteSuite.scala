package com.wavesplatform.it.sync.network

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.{KnownPeer, Peer}
import com.wavesplatform.it.{BaseFreeSpec, Node}
import org.scalatest.concurrent.Eventually
import org.scalatest.time.Span

import scala.concurrent.duration.DurationInt

class PeersApiRouteSuite extends BaseFreeSpec with Eventually {
  import com.wavesplatform.it.NodeConfigs.*
  override val nodeConfigs: Seq[Config] = Seq(
    BiggestMiner.quorum(0),
    Default.head.notMiner
  )

  private def ofANode(node: Node): PartialFunction[Any, Unit] = {
    case Peer(_, declaredAddress, name) :: Nil if name == node.name && declaredAddress == node.networkAddress.toString =>
  }

  "/peers/connected shows all connected peers" in {
    eventually(timeout(15.seconds), interval(1.second)) {
      nodes(0).connectedPeers should matchPattern(ofANode(nodes(1)))
      nodes(1).connectedPeers should matchPattern(ofANode(nodes(0)))
    }
  }

  "/peers/all should show blacklisted" in {
    nodes(0).blacklist(nodes(1).networkAddress)

    nodes(0).connectedPeers shouldBe Seq()

    nodes(1).connectedPeers shouldBe Seq()
    nodes(1).allPeers should matchPattern { case KnownPeer(address, _) :: Nil if address == nodes(0).networkAddress.toString => }
  }
}
