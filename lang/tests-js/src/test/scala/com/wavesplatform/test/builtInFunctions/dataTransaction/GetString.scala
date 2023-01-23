package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{binaryEntryForTests, booleanEntryForTests, integerEntryForTests, stringEntryForTests}
import utest.{Tests, test}

object GetString extends JsTestBase {
  // getStringKey
  private val getStringKey = s"getString(callerTestData, \"key\")"
  private val getStringKeyArgBeforeFunc = s"callerTestData.getString(\"key\")"
  // getStringIndex
  private val getStringIndex = s"getString(callerTestData, $randomInt)"
  private val getStringIndexArgBeforeFunc = s"callerTestData.getString($randomInt)"
  // getStringValueKey
  private val getStringValueKey = s"getStringValue(callerTestData, \"key\")"
  private val getStringValueKeyArgBeforeFunc = s"callerTestData.getStringValue(\"key\")"
  // getStringValueIndex
  private val getStringValueIndex = s"getStringValue(callerTestData, $randomInt)"
  private val getStringValueIndexArgBeforeFunc = s"callerTestData.getStringValue($randomInt)"

  // invalid getString
  private val invalidGetStringKey = s"getString()"
  private val invalidGetStringArgBeforeFunc = s"callerTestData.getString()"
  // invalid getStringValue
  private val invalidGetStringValue = s"getStringValue()"
  private val invalidGetStringValueArgBeforeFunc = s"callerTestData.getStringValue()"

  val tests: Tests = Tests {
    // getString
    test("check: function getString dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringKey,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getString dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getString dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getString dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getString dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getString dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getString dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getString dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getString dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getString dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getStringIndex
    test("check: function getStringIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getStringIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getStringIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getStringValueKey
    test("check: function getStringValueKey dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getStringValueKey dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringValueKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueKey dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringValueKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueKey dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringValueKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueKey dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringValueKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueKey dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getStringValueKey dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueKey dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueKey dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueKey dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getStringValueIndex
    test("check: function getStringValueIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getStringValueIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringValueIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringValueIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringValueIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringValueIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getStringValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test("check: function getStringValueIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getStringValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getStringValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getStringValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getStringValueIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getStringValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getString - Can't find a function overload
    test("compilation error: function getString V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getString version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: function getString V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getString version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getStringIndex - Can't find a function overload
    test("compilation error: function getStringIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getStringIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: function getStringIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getStringIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getStringValueKey - Can't find a function overload
    test("compilation error: function getStringValueKey V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getStringValueKey version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringValueKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: function getStringValueKey V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getStringValueKey version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getStringValueIndex - Can't find a function overload
    test("compilation error: function getStringValueIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getStringValueIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringValueIndex,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: function getStringValueIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getStringValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: function getStringValueIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getStringValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getString
    test("compilation error: invalid getString - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetStringKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: invalid getString - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetStringKey,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid getString - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetStringArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: invalid getString - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetStringArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getStringValue
    test("compilation error: invalid getStringValue - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetStringValue,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: invalid getStringValue - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetStringValue,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid getStringValue - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetStringValueArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: invalid getStringValue - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetStringValueArgBeforeFunc,
          "",
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
