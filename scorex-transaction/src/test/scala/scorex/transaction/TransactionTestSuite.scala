package scorex.transaction

import org.scalatest.Suites

class TransactionTestSuite extends Suites(
  new TransactionSpecification,
  new StoredStateUnitTests,
  new RowSpecification,
  new GenesisTransactionSpecification
)
