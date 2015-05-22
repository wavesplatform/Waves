package scorex

import org.scalatest.Suites
import scorex.integration.ValidChainGenerationSpecification
import scorex.unit._

class ScorexTestSuite extends Suites(
  //unit tests
  new CryptoSpecification,
  new MessageSpecification,
  new TransactionSpecification,
  new BlockchainStorageSpecification,
  new WalletSpecification,

  //integration tests - slow!
  new ValidChainGenerationSpecification
)
