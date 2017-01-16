package scorex

import org.scalatest.{BeforeAndAfterAll, Sequential}
import scorex.account.AccountSpecification
import scorex.consensus.mining.{BlockGeneratorControllerSpecification, MinerSpecification}
import scorex.crypto.SigningFunctionsSpecification
import scorex.network._
import scorex.network.peer.{PeerDatabaseImplSpecification, PeerManagerSpecification}
import scorex.transaction.BlockStorageSpecification
import scorex.utils.CircularBufferSpecification

class BasicsTestSuite extends Sequential(
  new MinerSpecification,
  new BlockchainSynchronizerSpecification,
  new StoredBlockSeqSpecification,
  new ScoreObserverSpecification,
  new HistoryReplierSpecification,
  new AccountSpecification,
  new SigningFunctionsSpecification,
  new HandshakeSpecification,
  new BlacklistParallelSpecification,
  new BlacklistSpecification,
  new SendingStrategySpecification,
  new PeerManagerSpecification,
  new CoordinatorSpecification,
  new PeerDatabaseImplSpecification,
  new BlockGeneratorControllerSpecification,
  new NetworkListenerSpecification,
  new PeerSynchronizerSpecification,
  new CircularBufferSpecification,
  new BlockStorageSpecification,
  new CheckpointSpecification
) with BeforeAndAfterAll {

  import scorex.waves.TestingCommons._

  override def beforeAll(): Unit = {
    start()
  }

  override def afterAll(): Unit = {
    stop()
  }
}

