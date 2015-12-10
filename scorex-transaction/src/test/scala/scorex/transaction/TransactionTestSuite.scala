package scorex.transaction

import org.scalatest.Suites
import scorex.transaction.TransactionSpecification

class TransactionTestSuite extends Suites(
  new TransactionSpecification
)
