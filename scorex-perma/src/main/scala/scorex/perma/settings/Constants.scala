package scorex.perma.settings

import com.typesafe.config.ConfigFactory
import scorex.crypto.Sha256
import scorex.utils.ScorexLogging

object Constants extends ScorexLogging {

  private val permaConf = ConfigFactory.load("perma").getConfig("perma")

  type DataSegment = Array[Byte]

  //few segments to be stored in a block, so segment size shouldn't be big
  val segmentSize = permaConf.getInt("segmentSize") //segment size in bytes

  val n = permaConf.getLong("n") //how many segments in a dataset in total

  val l = permaConf.getInt("l") //how many segments to store for an each miner

  val k = permaConf.getInt("k") //number of iterations during scratch-off phase

  val initialTarget = BigInt(permaConf.getString("initialTarget"))

  val targetRecalculation = permaConf.getInt("targetRecalculation") //recalculate target every targetRecalculation blocks

  val averageDelay = permaConf.getInt("averageDelay") //average delay between blocks in seconds

  val hash = permaConf.getString("hash") match {
    case s: String if s.equalsIgnoreCase("sha256") =>
      Sha256
    case hashFunction =>
      log.warn(s"Unknown hash function: $hashFunction. Use Sha256 instead.")
      Sha256
  }

}
