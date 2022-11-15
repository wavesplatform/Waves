package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{binaryEntryForTests, booleanEntryForTests, integerEntryForTests, stringEntryForTests}
import utest.{Tests, test}

object GetBinary extends JsTestBase {
  // getBinaryKey
  val getBinaryKey = s"getBinary(callerTestData, \"key\")"
  val getBinaryKeyArgBeforeFunc = s"callerTestData.getBinary(\"key\")"
  // getBinaryIndex
  val getBinaryIndex = s"getBinary(callerTestData, $randomInt)"
  val getBinaryIndexArgBeforeFunc = s"callerTestData.getBinary($randomInt)"
  // getBinaryValueKey
  val getBinaryValueKey = s"getBinaryValue(callerTestData, \"key\")"
  val getBinaryValueKeyArgBeforeFunc = s"callerTestData.getBinaryValue(\"key\")"
  // getBinaryValueIndex
  val getBinaryValueIndex = s"getBinaryValue(callerTestData, $randomInt)"
  val getBinaryValueIndexArgBeforeFunc = s"callerTestData.getBinaryValue($randomInt)"

  // invalid getBinary
  val invalidGetBinaryKey = s"getBinary()"
  val invalidGetBinaryArgBeforeFunc = s"callerTestData.getBinary()"
  // invalid getBinaryValue
  val invalidGetBinaryValue = s"getBinaryValue()"
  val invalidGetBinaryValueArgBeforeFunc = s"callerTestData.getBinaryValue()"

  val tests: Tests = Tests {
    // getBinary
    test.apply("check: function getBinary dataTransaction compiles for V3 dataEntry") {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
        val script = precondition.codeFromMatchingAndCase(
          dataEntryForTests(randomStringArrayElement),
          getBinaryKey,
          testData.rideV3Result,
          ""
        )
        assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinary dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBinaryKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinary dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBinaryIndex
    test.apply("check: function getBinaryIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBinaryIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBinaryIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBinaryValueKey
    test.apply("check: function getBinaryValueKey dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBinaryValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryValueKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryValueKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryValueKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryValueKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBinaryValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueKey dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBinaryValueIndex
    test.apply("check: function getBinaryValueIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBinaryValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryValueIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryValueIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryValueIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryValueIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBinaryValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBinaryValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBinaryValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBinaryValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValueIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBinaryValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBinary - Can't find a function overload
    test.apply("compilation error: function getBinary V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinary version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBinary V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinary version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getBinaryIndex - Can't find a function overload
    test.apply("compilation error: function getBinaryIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinaryIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBinaryIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinaryIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getBinaryValueKey - Can't find a function overload
    test.apply("compilation error: function getBinaryValueKey V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinaryValueKey version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryValueKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBinaryValueKey V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinaryValueKey version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getBinaryValueIndex - Can't find a function overload
    test.apply("compilation error: function getBinaryValueIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinaryValueIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryValueIndex,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBinaryValueIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBinaryValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBinaryValueIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBinaryValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getBinary
    test.apply("compilation error: invalid getBinary - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetBinaryKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBinary - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetBinaryKey,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid getBinary - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetBinaryArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBinary - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetBinaryArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getBinaryValue
    test.apply("compilation error: invalid getBinaryValue - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetBinaryValue,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBinaryValue - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetBinaryValue,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid getBinaryValue - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetBinaryValueArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBinaryValue - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetBinaryValueArgBeforeFunc,
          "",
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
