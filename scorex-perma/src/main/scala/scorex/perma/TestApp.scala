package scorex.perma

import java.io.{File, RandomAccessFile}
import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import org.slf4j.LoggerFactory
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.BlockchainBuilderSpec.SendWorkToMiners
import scorex.perma.actors.MinerSpec.Initialize
import scorex.perma.actors.{Miner, TrustedDealer}

import scala.concurrent.duration._


object TestApp extends App {

  val MinersCount = 10

  val log = LoggerFactory.getLogger(this.getClass)

  val treeDirName = "/tmp/scorex/testApp/"

  val tree = if (Files.exists(Paths.get(treeDirName + "/tree0.mapDB"))) {
    log.info("Get existing tree")
    new MerkleTree(treeDirName, Parameters.n, Parameters.segmentSize, Parameters.hash)
  } else {
    log.info("Generating random data set")
    val treeDir = new File(treeDirName)
    treeDir.mkdirs()
    val datasetFile = treeDirName + "/data.file"
    new RandomAccessFile(datasetFile, "rw").setLength(Parameters.n * Parameters.segmentSize)
    log.info("Calculate tree")
    val tree = MerkleTree.fromFile(datasetFile, treeDirName, Parameters.segmentSize, Parameters.hash)
    require(tree.nonEmptyBlocks == Parameters.n, s"${tree.nonEmptyBlocks} == ${Parameters.n}")
    tree
  }

  log.info("test tree")
  val index = Parameters.n - 3
  val leaf = tree.byIndex(index).get
  require(leaf.check(index, tree.rootHash)(Parameters.hash))

  log.info("Success: " + tree.rootHash.mkString)

  log.info("start actor system")
  protected lazy val actorSystem = ActorSystem("lagonaki")
  val dealer = actorSystem.actorOf(Props(new TrustedDealer(tree)))
  val miners = (1 to MinersCount).map(x => actorSystem.actorOf(Props(classOf[Miner], tree.rootHash), s"m-$x"))

  implicit val timeout = Timeout(1 minute)

  //Test, that miners download data from each other
  miners.head ! Initialize(Seq(dealer))
  Thread.sleep(200)

  val blockchainBuilder = actorSystem.actorOf(Props(classOf[BlockchainBuilder], miners, dealer), "BlockchainBuilder")
  blockchainBuilder ! SendWorkToMiners
}
