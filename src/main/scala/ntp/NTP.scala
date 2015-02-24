package ntp

import java.net.InetAddress
import java.util.logging.Logger

import org.apache.commons.net.ntp.NTPUDPClient

import scala.util.Try


object NTP {
  private val TIME_TILL_UPDATE = 1000 * 60 * 10L
  private val NTP_SERVER = "pool.ntp.org"

  private var lastUpdate: Long = 0
  private var offset: Long = 0

  def getTime = {
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
    //CREATE CLIENT
    val client = new NTPUDPClient()
    client.setDefaultTimeout(10000)

    Try {
      client.open()

      //GET INFO FROM NTP SERVER
      val hostAddr = InetAddress.getByName(NTP_SERVER)
      val info = client.getTime(hostAddr)
      info.computeDetails()

      if (info.getOffset != null) offset = info.getOffset
    }
    client.close()
  }
}
