package com.wavesplatform.it.sync.network

import com.typesafe.config.Config
import com.wavesplatform.it.api.{Peer, KnownPeer}
import com.wavesplatform.it.{BaseFreeSpec, Node, NodeConfigs}
import com.wavesplatform.it.api.SyncHttpApi.*

class PeersApiRouteSuite extends BaseFreeSpec {
  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(0))
    .withDefault(1)
    .withSpecial(_.nonMiner)
    .buildNonConflicting()

  private def ofANode(node: Node): PartialFunction[Any, Unit] = {
    case Peer(_, declaredAddress, name) :: Nil if name == node.name && declaredAddress == node.networkAddress.toString =>
  }


  "/peers/connected shows all connected peers" in {
    nodes(0).connectedPeers should matchPattern(ofANode(nodes(1)))
    nodes(1).connectedPeers should matchPattern(ofANode(nodes(0)))
  }

  "/peers/all should show blacklisted" in {
    nodes(0).blacklist(nodes(1).networkAddress)

//    nodes(0).blacklistedPeers shouldBe Seq(Peer("", "", ""))
    nodes(0).connectedPeers shouldBe Seq()

    nodes(1).connectedPeers shouldBe Seq()
    nodes(1).allPeers should matchPattern { case KnownPeer(address, _) :: Nil if address == nodes(0).networkAddress.toString => }
  }
}
