package scorex

import org.scalatest.Suites
import scorex.crypto.{SigningFunctionsSpecification, Sha256Specification}
import scorex.crypto.ads.merkle.{MerkleTreeStorageSpecification, MerkleSpecification}

class ScorexTestSuite extends Suites (
  new Sha256Specification,
  new SigningFunctionsSpecification,
  new MerkleSpecification,
  new MerkleTreeStorageSpecification
)
