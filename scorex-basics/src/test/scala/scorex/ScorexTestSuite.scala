package scorex

import org.scalatest.Suites
import scorex.account.AccountSpecification
import scorex.crypto.SigningFunctionsSpecification
import scorex.crypto.ads.merkle.{AuthDataBlockSpecification, MerkleSpecification, MerkleTreeStorageSpecification}
import scorex.network.{BlacklistParallelSpecification, BlacklistSpecification, HandshakeSpecification}

class ScorexTestSuite extends Suites(
  new AccountSpecification,
  new AuthDataBlockSpecification,
  new SigningFunctionsSpecification,
  new MerkleSpecification,
  new MerkleTreeStorageSpecification,
  new HandshakeSpecification,
  new BlacklistParallelSpecification,
  new BlacklistSpecification
)
