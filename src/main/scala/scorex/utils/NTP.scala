package scorex.utils

import java.net.InetAddress

import org.apache.commons.net.ntp.NTPUDPClient

import scala.util.Try

object NTP extends ScorexLogging {
  private val TimeTillUpdate = 1000 * 60 * 10L
  private val NtpServer = "pool.ntp.org"

  private var lastUpdate = 0L
  private var offset = 0L

  def correctedTime(): Long = {
    //CHECK IF OFFSET NEEDS TO BE UPDATED
    if (System.currentTimeMillis() > lastUpdate + TimeTillUpdate) {
      Try {
        updateOffSet()
        lastUpdate = System.currentTimeMillis()

        log.info("Adjusting time with " + offset + " milliseconds.")
      } recover {
        case e: Throwable =>
          log.warn("Unable to get corrected time", e)
      }
    }

    //CALCULATE CORRECTED TIME
    System.currentTimeMillis() + offset
  }

  private def updateOffSet() {
    val client = new NTPUDPClient()
    client.setDefaultTimeout(10000)

    try {
      client.open()

      val info = client.getTime(InetAddress.getByName(NtpServer))
      info.computeDetails()
      if (Option(info.getOffset).isDefined) offset = info.getOffset
    } catch {
      case t: Throwable => log.warn("Problems with NTP: ", t)
    } finally {
      client.close()
    }
  }
}
