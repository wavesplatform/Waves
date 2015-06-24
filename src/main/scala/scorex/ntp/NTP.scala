package scorex.ntp

import java.net.InetAddress
import java.util.logging.Logger

import org.apache.commons.net.ntp.NTPUDPClient


object NTP {
  private val TIME_TILL_UPDATE = 1000 * 60 * 10L
  private val NTP_SERVER = "pool.ntp.org"

  private var lastUpdate = 0L
  private var offset = 0L

  def correctedTime() = {
    //CHECK IF OFFSET NEEDS TO BE UPDATED
    if (System.currentTimeMillis() > lastUpdate + TIME_TILL_UPDATE) {
      updateOffSet()
      lastUpdate = System.currentTimeMillis()

      Logger.getGlobal.info("Adjusting time with " + offset + " milliseconds.")
    }

    //CALCULATE CORRECTED TIME
    System.currentTimeMillis() + offset
  }

  private def updateOffSet() {
    val client = new NTPUDPClient()
    client.setDefaultTimeout(10000)

    try {
      client.open()

      val info = client.getTime(InetAddress.getByName(NTP_SERVER))
      info.computeDetails()
      if (info.getOffset != null) offset = info.getOffset
    } catch {
      case t: Throwable => Logger.getGlobal.warning("Problems with NTP: " + t.getMessage)
    } finally {
      client.close()
    }
  }
}
