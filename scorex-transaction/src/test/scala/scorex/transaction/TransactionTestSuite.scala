package scorex.transaction

import org.scalatest.Suites
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImplSpecification
import scorex.transaction.state.database.blockchain.StoredStateUnitTests

class TransactionTestSuite extends Suites(
  new TransactionSpecification,
  new StoredStateUnitTests,
  new RowSpecification,
  new GenesisTransactionSpecification,
  new UnconfirmedPoolSynchronizerSpecification,
  new UnconfirmedTransactionsDatabaseImplSpecification
)
