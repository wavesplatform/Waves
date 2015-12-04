package scorex.lagonaki

import org.scalatest.{BeforeAndAfterAll, Suites}
import scorex.lagonaki.integration.{BlockchainSyncerSpecification, BlocksRoutingSpecification, ValidChainGenerationSpecification}
import scorex.lagonaki.props.BlockStorageSpecification
import scorex.lagonaki.unit._
import scorex.transaction.state.database.blockchain.BlockTreeSpecification

class LagonakiTestSuite extends Suites(
  //unit tests
  new MessageSpecification
  , new BlockSpecification
  , new BlockStorageSpecification
  , new WalletSpecification
  , new BlockchainSyncerSpecification
  , new BlocksRoutingSpecification
  , new BlockTreeSpecification

  //integration tests - slow!
  , new ValidChainGenerationSpecification

) with BeforeAndAfterAll {

  override def beforeAll() = {}

  override def afterAll() = {}
}
