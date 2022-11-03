import utest.{Tests, test}

object GetBinary extends JsTestBase {
  // getBinary
  val getBinary = s"getBinary(callerTestData, ${getRandomString()})"
  val getBinaryArgBeforeFunc = s"callerTestData.getBinary(${getRandomString()})"
  val ownDataGetBinary = s"getBinary(${getRandomString()})"
  val ownDataGetBinaryArgBeforeFunc = s"${getRandomString()}.getBinary()"

  // getBinaryValue
  val getBinaryValue = s"getBinaryValue(callerTestData, ${getRandomString()})"
  val getBinaryValueArgBeforeFunc = s"callerTestData.getBinaryValue(${getRandomString()})"
  val ownDataGetBinaryValue = s"getBinaryValue(${getRandomString()})"
  val ownDataGetBinaryValueArgBeforeFunc = s"${getRandomString()}.getBinaryValue()"

  val invalidGetBinary = s"getBinary(callerTestData)"
  val invalidGetBinaryValue = s"getBinaryValue(callerTestData)"


  val tests: Tests = Tests {
    test.apply("check ride v%i function %s compiles or failed") {
      assertCompileError("1 + 1", "Script should return boolean")
      assertCompileSuccess("true")
    }
  }
}
