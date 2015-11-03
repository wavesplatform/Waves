package scorex.transaction

import org.scalatest.Suites
import scorex.transaction.props.TransactionSpecification

class TransactionTestSuite extends Suites(
  new TransactionSpecification
)
