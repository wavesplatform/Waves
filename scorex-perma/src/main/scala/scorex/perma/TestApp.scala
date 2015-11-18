package scorex.perma

import java.io.{File, RandomAccessFile}

import akka.actor.{ActorRef, ActorSystem, Props}
import org.slf4j.LoggerFactory
import scorex.crypto.ads.merkle.MerkleTree
import scorex.perma.actors.MinerSpec.Initialize
import scorex.perma.actors.{Miner, TrustedDealer}


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
  assert(tree.nonEmptyBlocks == Parameters.n, s"${tree.nonEmptyBlocks} == ${Parameters.n}")

  log.info("start actor system")
  protected lazy val actorSystem = ActorSystem("lagonaki")
  val dealer = actorSystem.actorOf(Props(new TrustedDealer(tree)))
  val miners: Seq[ActorRef] = (1 to MinersCount).map(x => actorSystem.actorOf(Props(classOf[Miner], dealer, tree.rootHash)))

  miners.foreach(minerRef => minerRef ! Initialize)

  Thread.sleep(2000)

  log.info("start BlockchainBuilder")

  val blockchainBuilder = actorSystem.actorOf(Props(classOf[BlockchainBuilder], miners))
  blockchainBuilder ! BlockchainBuilderSpec.SendWorkToMiners
}
