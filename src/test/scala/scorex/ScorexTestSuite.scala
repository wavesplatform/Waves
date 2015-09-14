package scorex

import org.scalatest.{BeforeAndAfterAll, Suites}
import scorex.integration.BlocksRoutingSpecification
import scorex.unit._

class ScorexTestSuite extends Suites(
  //unit tests
  new MessageSpecification
  ,new TransactionSpecification
  ,new BlockSpecification
  ,new BlockchainStorageSpecification
  ,new WalletSpecification
  ,new BlocksRoutingSpecification
  ,new CryptoTestSuite

  //integration tests - slow!
  // todo:uncomment after fixing problems with test stopping
  //,new ValidChainGenerationSpecification

)  with BeforeAndAfterAll {

  override def beforeAll() = {}

  override def afterAll() = {}
}
