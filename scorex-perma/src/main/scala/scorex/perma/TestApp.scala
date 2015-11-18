package scorex.perma

import java.io.{File, RandomAccessFile}
import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import org.slf4j.LoggerFactory
import scorex.crypto.Sha256
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.BlockchainBuilderSpec.SendWorkToMiners
import scorex.perma.actors.{Miner, TrustedDealer}

import scala.concurrent.duration._


object TestApp extends App {

  val MinersCount = 10

  val log = LoggerFactory.getLogger(this.getClass)

  log.info("Generating random data set")
  val treeDirName = "/tmp/scorex/testApp/"

  log.info("Calculate tree")
  val tree = if (Files.exists(Paths.get(treeDirName + "/tree.mapDB"))) {
    new MerkleTree(treeDirName, Parameters.n)
  } else {
    val treeDir = new File(treeDirName)
    treeDir.mkdirs()
    val datasetFile = treeDirName + "/data.file"
    new RandomAccessFile(datasetFile, "rw").setLength(Parameters.n * Parameters.segmentSize)
    val tree = MerkleTree.fromFile(datasetFile, treeDirName, Parameters.segmentSize)
    require(tree.nonEmptyBlocks == Parameters.n, s"${tree.nonEmptyBlocks} == ${Parameters.n}")
    tree
  }

  log.info("test tree")
  val index = Parameters.n - 3
  val leaf = tree.byIndex(index).get
  require(leaf.check(index, tree.rootHash)(Sha256))

  log.info("start actor system")
  protected lazy val actorSystem = ActorSystem("lagonaki")
  val dealer = actorSystem.actorOf(Props(new TrustedDealer(tree)))
  val miners = (1 to MinersCount).map(x => actorSystem.actorOf(Props(classOf[Miner], dealer, tree.rootHash), s"m-$x"))

  implicit val timeout = Timeout(1 minute)

  val blockchainBuilder = actorSystem.actorOf(Props(classOf[BlockchainBuilder], miners), "BlockchainBuilder")
  blockchainBuilder ! SendWorkToMiners


}
