package scorex

import org.scalatest.{BeforeAndAfterAll, Suites}
import scorex.integration.{ValidChainGenerationSpecification, BlocksRoutingSpecification}
import scorex.unit._

class ScorexTestSuite extends Suites(
  //unit tests
  new MessageSpecification
  ,new props.TransactionSpecification
  ,new BlockSpecification
  ,new BlockchainStorageSpecification
  ,new WalletSpecification
  ,new BlocksRoutingSpecification

  //integration tests - slow!
  ,new ValidChainGenerationSpecification

)  with BeforeAndAfterAll {

  override def beforeAll() = {}

  override def afterAll() = {}
}
