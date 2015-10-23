package scorex

import org.scalatest.Suites
import scorex.props.SigningFunctionsSpecification

class CryptoTestSuite extends Suites (
  //unit tests
  new unit.Base58Specification,
  new props.Base58Specification,

  new unit.Sha256Specification,
  new props.Sha256Specification,

  new SigningFunctionsSpecification
)
