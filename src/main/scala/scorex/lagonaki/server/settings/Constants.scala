package scorex.lagonaki.server.settings

import com.typesafe.config.ConfigFactory
import scorex.consensus._
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.utils.ScorexLogging

/**
 * System constants here.
 */

object Constants extends ScorexLogging {
  private val appConf = ConfigFactory.load().getConfig("app")

  val Product = appConf.getString("product")
  val Release = appConf.getString("release")
  val VersionString = appConf.getString("version")
  val AgentName = s"$Product - $Release v. $VersionString"
}