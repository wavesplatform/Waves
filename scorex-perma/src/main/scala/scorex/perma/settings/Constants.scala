package scorex.perma.settings

import com.typesafe.config.ConfigFactory
import scorex.crypto.Sha256
import scorex.utils.ScorexLogging

object Constants extends ScorexLogging {

  private val appConf = ConfigFactory.load().getConfig("perma")

  type DataSegment = Array[Byte]

  //few segments to be stored in a block, so segment size shouldn't be big
  val segmentSize = appConf.getInt("segmentSize") //segment size in bytes

  val n = appConf.getLong("n") //how many segments in a dataset in total

  val l = appConf.getInt("l") //how many segments to store for an each miner

  val k = appConf.getInt("k") //number of iterations during scratch-off phase

  val hash = appConf.getString("hash") match {
    case s: String if s.equalsIgnoreCase("sha256") =>
      Sha256
    case hashFunction =>
      log.error(s"Unknown hash function: $hashFunction. Use Sha256 instead.")
      Sha256
  }

}
