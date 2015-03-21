package scorex.database

import java.net.InetAddress

import scorex.network.Peer

import scala.collection.mutable


trait PeerDatabase {
  def addPeer(p: Peer)

  def blacklistPeer(p: Peer)

  def knownPeers(): Seq[Peer]

  def isBlacklisted(address: InetAddress): Boolean
}


object PeerDatabaseImpl extends PeerDatabase {
  val whitelist = mutable.Buffer[Peer]()
  val blacklist = mutable.Buffer[Peer]()

  override def addPeer(peer: Peer): Unit = whitelist += peer

  override def blacklistPeer(peer: Peer): Unit = {
    whitelist -= peer
    blacklist += peer
  }

  override def isBlacklisted(address: InetAddress): Boolean = blacklist.exists(_.address == address)

  override def knownPeers(): Seq[Peer] = whitelist.toSeq
}