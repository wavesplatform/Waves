package scorex

import org.scalatest.Suites
import scorex.basics.props.{Base58Specification, SigningFunctionsSpecification, Sha256Specification}

class ScorexTestSuite extends Suites (
  //unit tests
  new scorex.lagonaki.unit.Base58Specification,
  new Base58Specification,

  new scorex.lagonaki.unit.Sha256Specification,
  new Sha256Specification,

  new SigningFunctionsSpecification
)
