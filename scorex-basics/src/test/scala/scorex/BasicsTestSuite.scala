package scorex

import org.scalatest.Suites
import scorex.account.AccountSpecification
import scorex.consensus.mining.{BlockGeneratorControllerSpecification, MinerSpecification}
import scorex.crypto.SigningFunctionsSpecification
import scorex.network._
import scorex.network.peer.{PeerDatabaseImplSpecification, PeerManagerSpecification}
import scorex.transaction.BlockStorageSpecification
import scorex.utils.CircularBufferSpecification

class BasicsTestSuite extends Suites(
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
  new BlockStorageSpecification
)
