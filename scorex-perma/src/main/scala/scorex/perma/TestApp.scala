package scorex.perma

import java.io.{File, RandomAccessFile}
import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.BlockchainBuilderSpec.SendWorkToMiners
import scorex.perma.storage.AuthDataStorage
import scorex.perma.actors.MinerSpec.Initialize
import scorex.perma.actors.{Miner, TrustedDealer}
import scorex.perma.settings.{Constants, PermaSettings}
import scorex.settings.Settings
import scorex.utils.ScorexLogging

import scala.concurrent.duration._


object TestApp extends App with ScorexLogging {

  val MinersCount = 10

  implicit val settings = new Settings with PermaSettings {
    val filename = "settings.json"
  }

  val tree = if (Files.exists(Paths.get(settings.treeDir + "/tree0.mapDB"))) {
    log.info("Get existing tree")
    new MerkleTree(settings.treeDir, Constants.n, Constants.segmentSize, Constants.hash)
  } else {
    log.info("Generating random data set")
    val treeDir = new File(settings.treeDir)
    treeDir.mkdirs()
    val datasetFile = settings.treeDir + "/data.file"
    new RandomAccessFile(datasetFile, "rw").setLength(Constants.n * Constants.segmentSize)
    log.info("Calculate tree")
    val tree = MerkleTree.fromFile(datasetFile, settings.treeDir, Constants.segmentSize, Constants.hash)
    require(tree.nonEmptyBlocks == Constants.n, s"${tree.nonEmptyBlocks} == ${Constants.n}")
    tree
  }

  log.info("test tree")
  val index = Constants.n - 3
  val leaf = tree.byIndex(index).get
  require(leaf.check(index, tree.rootHash)(Constants.hash))

  log.info("Success: " + tree.rootHash.mkString)

  log.info("start actor system")
  protected lazy val actorSystem = ActorSystem("lagonaki")
  val dealer = actorSystem.actorOf(Props(new TrustedDealer(tree)))
  val storage = new AuthDataStorage(settings.authDataStorage)
  val miners = (1 to MinersCount).map(x => actorSystem.actorOf(Props(classOf[Miner], tree.rootHash, storage), s"m-$x"))

  implicit val timeout = Timeout(1 minute)

  //Test, that miners download data from each other
  miners.head ! Initialize(Seq(dealer))
  Thread.sleep(200)

  val blockchainBuilder = actorSystem.actorOf(Props(classOf[BlockchainBuilder], miners, dealer), "BlockchainBuilder")
  blockchainBuilder ! SendWorkToMiners
}
