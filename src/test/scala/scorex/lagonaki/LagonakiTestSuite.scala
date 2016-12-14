package scorex.lagonaki

import org.scalatest.{BeforeAndAfterAll, Sequential}
import scorex.lagonaki.TestingCommons._
import scorex.lagonaki.integration.ValidChainGenerationSpecification
import scorex.lagonaki.integration.api._
import scorex.lagonaki.unit._
import scorex.transaction.state.StateTest

class LagonakiTestSuite extends Sequential(

  // API tests
  new BlockAPISpecification
  , new UtilsAPISpecification
  , new PeersAPISpecification
  , new WalletAPISpecification
  , new AddressesAPISpecification
  , new TransactionsAPISpecification
  , new PaymentAPISpecification

  //unit tests
  , new MessageSpecification
  , new MessageHandlerSpec
  , new BlockSpecification
  , new WalletSpecification
  , new StateTest
  , new SimpleTransactionModuleSpecification
  , new OrderMatchStoredStateSpecification

  //integration tests - slow!
  , new ValidChainGenerationSpecification
  , new StoredStateSpecification

) with BeforeAndAfterAll {

  override protected def afterAll() = {
    applications.foreach(_.shutdown())
  }
}
