package scorex

import org.scalatest.Suites
import scorex.unit.{SigningFunctionsSpecification, Sha256Specification}

class CryptoTestSuite extends Suites (
  //unit tests
  new unit.Base58Specification,
  new props.Base58Specification,
  new Sha256Specification,
  new SigningFunctionsSpecification
)
