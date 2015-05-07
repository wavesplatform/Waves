package scorex

import org.scalatest.Suites
import scorex.unit._

class ScorexTestSuite extends Suites(
  new CryptoSpecification,
  new MessageSpecification,
  new TransactionSpecification,
  new BlockchainStorageSpecification
  // , new WalletSpecification todo: fix issues in Wallet & according tests
)
