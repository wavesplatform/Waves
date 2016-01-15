package scorex.perma.settings

import com.typesafe.config.ConfigFactory
import scorex.utils.ScorexLogging

object PermaConstants extends ScorexLogging {

  private val permaConf = ConfigFactory.load("perma").getConfig("perma")

  type DataSegmentIndex = Long
  type DataSegment = Array[Byte]

  //few segments to be stored in a block, so segment size shouldn't be big
  val segmentSize = permaConf.getInt("segmentSize") //segment size in bytes

  val n = permaConf.getLong("n") //how many segments in a dataset in total

  val l = permaConf.getInt("l") //how many segments to store for an each miner

  val k = permaConf.getInt("k") //number of iterations during scratch-off phase

  val initialTarget = BigInt(permaConf.getString("initialTarget"))

  val targetRecalculation = permaConf.getInt("targetRecalculation") //recalculate target every targetRecalculation blocks

  val averageDelay = permaConf.getInt("averageDelay") //average delay between blocks in seconds

}
