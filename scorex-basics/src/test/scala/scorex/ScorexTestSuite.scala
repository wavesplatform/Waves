package scorex

import org.scalatest.Suites

class ScorexTestSuite extends Suites (
  new props.Base58Specification,
  new unit.Base58Specification,

  new props.Sha256Specification,
  new unit.Sha256Specification,

  new props.SigningFunctionsSpecification,
  new props.MerkleSpecification
)
