package network

import java.util.logging.Logger
import network.message.{GetPeersMessage, PeersMessage}
import settings.Settings
import scala.util.{Random, Failure, Success}

class ConnectionCreator(callback: ConnectionCallback) extends Thread {

  override def run() {
    try {
      while (true) {
        //CHECK IF WE NEED NEW CONNECTIONS
        if (Settings.getMinConnections >= callback.getActiveConnections.size) {
          //GET LIST OF KNOWN PEERS
          PeerManager.getKnownPeers.foreach { peer =>
            //CHECK IF WE ALREADY HAVE MAX CONNECTIONS
            if (Settings.getMaxConnections > callback.getActiveConnections.size) {
              //CHECK IF ALREADY CONNECTED TO PEER
              if (!callback.isConnectedTo(peer.address)) {
                //CHECK IF SOCKET IS NOT LOCALHOST
                //CONNECT
                Logger.getGlobal.info("Connecting to peer : " + peer.address)

                peer.connect(callback)
              }
            }
          }
        }

        //CHECK IF WE STILL NEED NEW CONNECTIONS
        if (Settings.getMinConnections >= callback.getActiveConnections.size) {
          //avoids Exception when adding new elements
          callback.getActiveConnections.foreach { peer =>

            //CHECK IF WE ALREADY HAVE MAX CONNECTIONS
            if (Settings.getMaxConnections > callback.getActiveConnections.size) {
              //ASK PEER FOR PEERS
              peer.getResponse(GetPeersMessage(mbId = Some(Random.nextInt(1000000) + 1))) match {
                case Success(PeersMessage(peers, _, _)) =>
                  peers.foreach { newPeer =>
                    //CHECK IF WE ALREADY HAVE MAX CONNECTIONS
                    if (Settings.getMaxConnections > callback.getActiveConnections.size) {
                      //CHECK IF THAT PEER IS NOT BLACKLISTED
                      if (!PeerManager.isBlacklisted(newPeer)) {
                        //CHECK IF CONNECTED
                        if (!callback.isConnectedTo(newPeer)) {
                          //CHECK IF SOCKET IS NOT LOCALHOST
                          if (!newPeer.address.isSiteLocalAddress && !newPeer.address.isLoopbackAddress && !newPeer.address.isAnyLocalAddress) {
                            Logger.getGlobal.info("Connecting to peer : " + newPeer.address)

                            //CONNECT
                            newPeer.connect(callback)
                          }
                        }
                      }
                    }
                  }

                case Success(m) => Logger.getGlobal.warning(s"Wrong kind of response for GetPeers")
                case Failure(e) => Logger.getGlobal.throwing("Peer", "getResponse", e)
              }
            }
          }
        }
        //SLEEP
        Thread.sleep(60 * 1000)
      }
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        Logger.getGlobal.info("Error creating new connection")
    }
  }

}
