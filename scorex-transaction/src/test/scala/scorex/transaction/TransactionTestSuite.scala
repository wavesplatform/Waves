package scorex.transaction

import org.scalatest.Suites
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImplSpecification

class TransactionTestSuite extends Suites(
  new OrderMatchTransactionSpecification,
  new OrderTransactionSpecification,
  new TransactionSpecification,
  new StoredStateUnitTests,
  new RowSpecification,
  new GenesisTransactionSpecification,
  new UnconfirmedPoolSynchronizerSpecification,
  new UnconfirmedTransactionsDatabaseImplSpecification
)
