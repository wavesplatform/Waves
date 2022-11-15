package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{binaryEntryForTests, booleanEntryForTests, integerEntryForTests, stringEntryForTests}
import utest.{Tests, test}

object GetBoolean extends JsTestBase {
  // getBooleanKey
  val getBooleanKey = s"getBoolean(callerTestData, \"key\")"
  val getBooleanKeyArgBeforeFunc = s"callerTestData.getBoolean(\"key\")"
  // getBooleanIndex
  val getBooleanIndex = s"getBoolean(callerTestData, $randomInt)"
  val getBooleanIndexArgBeforeFunc = s"callerTestData.getBoolean($randomInt)"
  // getBooleanValueKey
  val getBooleanValueKey = s"getBooleanValue(callerTestData, \"key\")"
  val getBooleanValueKeyArgBeforeFunc = s"callerTestData.getBooleanValue(\"key\")"
  // getBooleanValueIndex
  val getBooleanValueIndex = s"getBooleanValue(callerTestData, $randomInt)"
  val getBooleanValueIndexArgBeforeFunc = s"callerTestData.getBooleanValue($randomInt)"

  // invalid getBoolean
  val invalidGetBooleanKey = s"getBoolean()"
  val invalidGetBooleanArgBeforeFunc = s"callerTestData.getBoolean()"
  // invalid getBooleanValue
  val invalidGetBooleanValue = s"getBooleanValue()"
  val invalidGetBooleanValueArgBeforeFunc = s"callerTestData.getBooleanValue()"

  val tests: Tests = Tests {
    // getBoolean
    test.apply("check: function getBoolean dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanKey,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBoolean dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBoolean dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBoolean dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBoolean dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBoolean dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBoolean dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBoolean dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBoolean dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBoolean dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBooleanIndex
    test.apply("check: function getBooleanIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBooleanValueKey
    test.apply("check: function getBooleanValueKey dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanValueKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanValueKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanValueKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanValueKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueKey dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBooleanValueIndex
    test.apply("check: function getBooleanValueIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanValueIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanValueIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanValueIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanValueIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getBooleanValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getBooleanValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getBooleanValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getBooleanValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBooleanValueIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getBooleanValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getBoolean - Can't find a function overload
    test.apply("compilation error: function getBoolean V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBoolean version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBoolean V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBoolean version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getBooleanIndex - Can't find a function overload
    test.apply("compilation error: function getBooleanIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBooleanIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBooleanIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBooleanIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getBooleanValueKey - Can't find a function overload
    test.apply("compilation error: function getBooleanValueKey V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBooleanValueKey version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanValueKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBooleanValueKey V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBooleanValueKey version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getBooleanValueIndex - Can't find a function overload
    test.apply("compilation error: function getBooleanValueIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBooleanValueIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanValueIndex,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getBooleanValueIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getBooleanValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getBooleanValueIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getBooleanValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getBoolean
    test.apply("compilation error: invalid getBoolean - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetBooleanKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBoolean - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetBooleanKey,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid getBoolean - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetBooleanArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBoolean - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetBooleanArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getBooleanValue
    test.apply("compilation error: invalid getBooleanValue - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetBooleanValue,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBooleanValue - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetBooleanValue,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid getBooleanValue - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetBooleanValueArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getBooleanValue - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetBooleanValueArgBeforeFunc,
          "",
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
