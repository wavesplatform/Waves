package scorex.app.settings

import com.typesafe.config.ConfigFactory
import scorex.consensus._
import scorex.app.utils.ScorexLogging

/**
 * System constants here.
 */

object Constants extends ScorexLogging {

  private val appConf = ConfigFactory.load().getConfig("app")

  val Product = appConf.getString("product")
  val Release = appConf.getString("release")
  val VersionString = appConf.getString("version")
  val AgentName = s"$Product - $Release v. $VersionString"
  val ConsensusAlgo: ConsensusModule = appConf.getString("consensusAlgo") match {
    case "ConsensusModuleNxt" => ConsensusModuleNxt
    case "ConsensusModuleQora" => ConsensusModuleQora
    case algo =>
      log.error(s"Unknown consensus algo: $algo. Use ConsensusModuleNxt instead.")
      ConsensusModuleNxt
  }

}