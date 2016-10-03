package scorex.lagonaki

import org.scalatest.{BeforeAndAfterAll, Sequential}
import scorex.lagonaki.TestingCommons._
import scorex.lagonaki.integration._
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
  , new StoredStateSpecification
  , new SimpleTransactionModuleSpecification

  //integration tests - slow!
  , new ValidChainGenerationSpecification

) with BeforeAndAfterAll {

  override protected def afterAll() = {
    applications.foreach(_.shutdown())
  }
}
