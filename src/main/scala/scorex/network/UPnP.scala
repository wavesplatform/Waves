package scorex.network

import java.net.InetAddress

import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover}
import scorex.settings.Settings
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.util.Try

class UPnP(settings:Settings) extends ScorexLogging {

  private var gateway: Option[GatewayDevice] = None

  lazy val localAddress = gateway.map(_.getLocalAddress)
  lazy val externalAddress = gateway.map(_.getExternalIPAddress).map(InetAddress.getByName)

  Try {
    log.info("Looking for UPnP gateway device...")
    val defaultHttpReadTimeout = settings.upnpGatewayTimeout.getOrElse(GatewayDevice.getHttpReadTimeout)
    GatewayDevice.setHttpReadTimeout(defaultHttpReadTimeout)
    val discover = new GatewayDiscover()
    val defaultDiscoverTimeout = settings.upnpDiscoverTimeout.getOrElse(discover.getTimeout)
    discover.setTimeout(defaultDiscoverTimeout)

    val gatewayMap = Option(discover.discover).map(_.toMap).getOrElse(Map())
    if (gatewayMap.isEmpty) {
      log.debug("There are no UPnP gateway devices")
    } else {
      gatewayMap.foreach { case (addr, _) =>
        log.debug("UPnP gateway device found on " + addr.getHostAddress)
      }
      Option(discover.getValidGateway) match {
        case None => log.debug("There is no connected UPnP gateway device")
        case Some(device) =>
          gateway = Some(device)
          log.debug("Using UPnP gateway device on " + localAddress.map(_.getHostAddress).getOrElse("err"))
          log.info("External IP address is " + externalAddress.map(_.getHostAddress).getOrElse("err"))
      }
    }
  }.recover { case t: Throwable =>
    log.error("Unable to discover UPnP gateway devices: " + t.toString)
  }

  def addPort(port: Int): Try[Unit] = Try {
    if (gateway.get.addPortMapping(port, port, localAddress.get.getHostAddress, "TCP", "Scorex")) {
      log.debug("Mapped port [" + externalAddress.get.getHostAddress + "]:" + port)
    } else {
      log.debug("Unable to map port " + port)
    }
  }.recover { case t: Throwable =>
    log.error("Unable to map port " + port + ": " + t.toString)
  }

  def deletePort(port: Int): Try[Unit] = Try {
    if (gateway.get.deletePortMapping(port, "TCP")) {
      log.debug("Mapping deleted for port " + port)
    } else {
      log.debug("Unable to delete mapping for port " + port)
    }
  }.recover { case t: Throwable =>
    log.error("Unable to delete mapping for port " + port + ": " + t.toString)
  }
}