package network

import java.io.DataInputStream
import java.net.{InetAddress, Socket}
import java.util.Collections
import java.util.HashMap
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.TimeUnit
import java.util.logging.Logger

import settings.Settings
import network.message.Message

import scala.util.{Failure, Success, Try, Random}


case class ConnectedPeer(socket: Socket,
                         callback: ConnectionCallback) extends Peer(socket.getInetAddress) {

  override val address = socket.getInetAddress


  private val messages = Collections.synchronizedMap(new HashMap[Integer, BlockingQueue[Message]]())

  val out = socket.getOutputStream


  callback.onConnect(this)

  val pinger = new Pinger(this)

  def getPing() = pinger.getPing

  val self = this


  val listener = new Thread() {
    override def run() {
      try {
        val in = new DataInputStream(socket.getInputStream)

        while (true) {
          //READ FIRST 4 BYTES
          val messageMagic = Array.fill(Message.MAGIC_LENGTH)(0: Byte)
          in.readFully(messageMagic)

          if (messageMagic.sameElements(Message.MAGIC)) {
            //PROCESS NEW MESSAGE
            val message = Message(self, in)

            Logger.getGlobal.finest("received message " + message.messageType + " from " + address.toString)

            //CHECK IF WE ARE WAITING FOR A MESSAGE WITH THAT ID
            message.mbId match {
              case Some(id) if messages.containsKey(id) => messages.get(id).add(message)
              case _ => callback.onMessage(message)
            }
          } else {
            Logger.getGlobal.warning("received message with wrong magic")
            callback.onError(self)
            return
          }
        }
      } catch {
        case t: Throwable => callback.onDisconnect(self)
      }
    }
  }

  def sendMessage(message: Message): Boolean = {
    try {
      //CHECK IF SOCKET IS STILL ALIVE
      if (!socket.isConnected) {
        //ERROR
        callback.onError(this)

        false
      } else {

        //SEND MESSAGE
        out.synchronized {
          out.write(message.toBytes)
          out.flush()
        }
        true
      }
    } catch {
      case t: Throwable => callback.onError(this)
        false
    }
  }

  def getResponse(message: Message): Try[Message] = {
    require(message.mbId.isDefined)
    val id = message.mbId.get

    //PUT QUEUE INTO MAP SO WE KNOW WE ARE WAITING FOR A RESPONSE
    val blockingQueue = new ArrayBlockingQueue[Message](1)
    messages.put(id, blockingQueue)

    if (!sendMessage(message)){
      Failure(new Exception("FAILED TO SEND MESSAGE"))
    }else {
      Try {
        val response = blockingQueue.poll(Settings.connectionTimeout, TimeUnit.MILLISECONDS)
        messages.remove(id)

        response
      }
    }
  }

  def onPingFail() = callback.onDisconnect(this)

  def close() {
    Try {
      listener.interrupt()
      if (pinger != null) pinger.stopPing()
      if (socket != null && socket.isConnected) socket.close()
    }
  }
}


class Peer(val address: InetAddress) {
  def connect(callback: ConnectionCallback): Option[ConnectedPeer] = Try {
    val socket = new Socket(address, Network.PORT)
    socket.setSoTimeout(1000 * 60 * 60)
    ConnectedPeer(socket, callback)
  } match {
    case Success(cp) => Some(cp)
    case Failure(_) => Logger.getGlobal.info("Failed to connect to : " + address); None
  }
}
