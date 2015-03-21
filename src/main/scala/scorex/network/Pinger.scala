package scorex.network

import scorex.network.message.PingMessage
import settings.Settings

import scala.util.{Random, Success, Try}


class Pinger(peer: ConnectedPeer) extends Thread {

  private var isRunning = true
  private var ping = Long.MaxValue

  start()

  def getPing = ping

  override def run() {
    while (isRunning) {
      val start = System.currentTimeMillis()
      peer.getResponse(PingMessage(mbId = Some(Random.nextInt(1000000) + 1))) match {
        case Success(PingMessage(_, _)) => ping = System.currentTimeMillis() - start
        case _ =>
          peer.onPingFail()
          isRunning = false
      }

      Try(Thread.sleep(Settings.pingInterval))
    }
  }

  def stopPing() {
    try {
      isRunning = false
      interrupt()
      join()
    } catch {
      case t: Throwable =>
    }
  }
}
