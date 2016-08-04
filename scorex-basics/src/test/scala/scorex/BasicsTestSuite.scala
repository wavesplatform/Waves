package scorex

import org.scalatest.Suites
import scorex.account.AccountSpecification
import scorex.crypto.SigningFunctionsSpecification
import scorex.network._
import scorex.network.peer.PeerManagerSpecification

class BasicsTestSuite extends Suites(
  new AccountSpecification,
  new SigningFunctionsSpecification,
  new HandshakeSpecification,
  new BlacklistParallelSpecification,
  new BlacklistSpecification,
  new SendingStrategySpecification,
  new PeerManagerSpecification
)
