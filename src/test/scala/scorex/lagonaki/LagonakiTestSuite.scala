package scorex.lagonaki

import org.scalatest.{BeforeAndAfterAll, Suites}
import scorex.lagonaki.TestingCommons._
import scorex.lagonaki.integration._
import scorex.lagonaki.integration.api._
import scorex.lagonaki.unit._
import scorex.transaction.state.database.blockchain.BlockTreeSpecification

class LagonakiTestSuite extends Suites(
  //unit tests
  new MessageSpecification
  , new BlockSpecification
  //  , new BlockStorageSpecification
  , new WalletSpecification
  , new BlockGeneratorSpecification
  , new BlockTreeSpecification
  // API tests
  , new SeedAPISpecification
  , new PeersAPISpecification
  , new WalletAPISpecification
  , new AdressesAPISpecification
  , new TransactionsAPISpecification
  , new PaymentAPISpecification
  , new BlockAPISpecification
  //integration tests - slow!
  , new ValidChainGenerationSpecification


) with BeforeAndAfterAll {

  override protected def beforeAll() = {}

  override protected def afterAll() = {
    applications.foreach(_.stopAll())
  }
}
