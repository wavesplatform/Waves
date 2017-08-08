package tools

import com.wavesplatform.it.api.NodeApi
import com.wavesplatform.it.api.NodeApi.Block

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Statistics extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  class DevNode(override val restAddress: String) extends NodeApi {
    override val nodeRestPort: Int = 6869
    override val matcherRestPort: Int = -1
    override val blockDelay: FiniteDuration = 60.seconds
  }

  val dev1 = new DevNode("34.251.200.245")
  val blocks = Await.result(Future.traverse(Range(1900, 2000).toList)(dev1.blockAt), 1.minute)

  val blocksPerMiner = blocks.groupBy(_.generator).map { case (gen, bs) => (gen, bs.size) }
  val transactionsPerBlockAvg = blocks.map(_.transactions.size).sum / blocks.size
}
