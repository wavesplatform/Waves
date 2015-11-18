package scorex.perma

import java.io.{File, RandomAccessFile}

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import org.slf4j.LoggerFactory
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.BlockchainBuilderSpec.SendWorkToMiners
import scorex.perma.actors.{Miner, TrustedDealer}

import scala.concurrent.duration._


object TestApp extends App {

  val MinersCount = 10

  val log = LoggerFactory.getLogger(this.getClass)

  log.info("Generating random data set")
  val treeDirName = "/tmp/scorex/testApp/"
  val treeDir = new File(treeDirName)
  val datasetFile = treeDirName + "/data.file"

  treeDir.mkdirs()
  val f = new RandomAccessFile(datasetFile, "rw")
  f.setLength(Parameters.n * Parameters.segmentSize)

  log.info("Calculate tree")
  val tree = MerkleTree.fromFile(datasetFile, treeDirName, Parameters.segmentSize)
  require(tree.nonEmptyBlocks == Parameters.n, s"${tree.nonEmptyBlocks} == ${Parameters.n}")

  log.info("start actor system")
  protected lazy val actorSystem = ActorSystem("lagonaki")
  val dealer = actorSystem.actorOf(Props(new TrustedDealer(tree)))
  val miners = (1 to MinersCount).map(x => actorSystem.actorOf(Props(classOf[Miner], dealer, tree.rootHash), s"m-$x"))

  implicit val timeout = Timeout(1 minute)

  val blockchainBuilder = actorSystem.actorOf(Props(classOf[BlockchainBuilder], miners), "BlockchainBuilder")
  blockchainBuilder ! SendWorkToMiners


}
