package scorex

import org.scalatest.Suites
import scorex.unit.{SigningFunctionsSpecification, HashFunctionsSpecification, Base58Specification}

class CryptoTestSuite extends Suites (
  //unit tests
  new Base58Specification,
  new HashFunctionsSpecification,
  new SigningFunctionsSpecification
)
