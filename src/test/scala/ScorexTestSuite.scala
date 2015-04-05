package scorex.test

import org.scalatest.Suites

class ScorexTestSuite  extends Suites (
  new CryptoSpecification,
  new TransactionSpecification,
  new MessageSpecification,
  new BlockchainStorageSpecification
)
