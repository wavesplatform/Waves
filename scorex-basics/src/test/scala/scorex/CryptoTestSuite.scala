package scorex

import org.scalatest.Suites
import scorex.unit.{SigningFunctionsSpecification, Sha256Specification, Base58Specification}

class CryptoTestSuite extends Suites (
  //unit tests
  new Base58Specification,
  new Sha256Specification,
  new SigningFunctionsSpecification
)
