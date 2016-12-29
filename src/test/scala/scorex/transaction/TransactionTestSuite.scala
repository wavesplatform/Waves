package scorex.transaction

import org.scalatest.{BeforeAndAfterAll, Sequential, Suites}
import scorex.lagonaki.TestingCommons.applications
import scorex.transaction.assets.exchange.OrderJsonSpecification
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImplSpecification
import scorex.transaction.state.database.blockchain.StoredStateUnitTests

class TransactionTestSuite extends Sequential(
  new TransactionSpecification,
  new TransferTransactionSpecification,
  new StoredStateUnitTests,
  new RowSpecification,
  new GenesisTransactionSpecification,
  new UnconfirmedPoolSynchronizerSpecification,
  new UnconfirmedTransactionsDatabaseImplSpecification,
  new OrderSpecification,
  new OrderMatchTransactionSpecification,
  new OrderJsonSpecification
) with BeforeAndAfterAll {

  override protected def afterAll() = {
    applications.foreach(_.shutdown())
  }
}

