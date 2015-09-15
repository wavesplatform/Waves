package scorex

import org.scalatest.Suites
import scorex.unit.{SigningFunctionsSpecification, CryptographicHashSpecification, Base58Specification}

class CryptoTestSuite extends Suites (
  //unit tests
  new Base58Specification,
  new CryptographicHashSpecification,
  new SigningFunctionsSpecification
)
