package scorex

import org.scalatest.Suites
import scorex.crypto.SigningFunctionsSpecification
import scorex.crypto.ads.merkle.{MerkleTreeStorageSpecification, MerkleSpecification}

class ScorexTestSuite extends Suites (
  new SigningFunctionsSpecification,
  new MerkleSpecification,
  new MerkleTreeStorageSpecification
)
