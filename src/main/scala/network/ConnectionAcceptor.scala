package network

import java.net.ServerSocket
import java.util.logging.Logger

import settings.Settings


class ConnectionAcceptor(callback: ConnectionCallback) extends Thread {

  override def run() {
    var socket: ServerSocket = null
    while (true) {
      try {
        if (socket == null) socket = new ServerSocket(Network.PORT)

        //CHECK IF WE HAVE MAX CONNECTIONS CONNECTIONS
        if (Settings.maxConnections <= callback.getActiveConnections.size) {
          //IF SOCKET IS OPEN CLOSE IT
          if (!socket.isClosed) socket.close()
          Thread.sleep(100)
        } else {
          //REOPEN SOCKET
          if (socket.isClosed) socket = new ServerSocket(Network.PORT)

          //ACCEPT CONNECTION
          val connectionSocket = socket.accept()

          //CHECK IF SOCKET IS NOT LOCALHOST || WE ARE ALREADY CONNECTED TO THAT SOCKET || BLACKLISTED
          if (callback.isConnectedTo(connectionSocket.getInetAddress) || PeerManager.isBlacklisted(connectionSocket.getInetAddress))
            connectionSocket.close()
          else new ConnectedPeer(connectionSocket, callback)
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          Logger.getGlobal.warning("Error accepting new connection")
      }
    }
  }
}
