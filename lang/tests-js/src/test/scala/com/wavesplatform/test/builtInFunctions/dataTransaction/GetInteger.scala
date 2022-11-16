package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{binaryEntryForTests, booleanEntryForTests, integerEntryForTests, stringEntryForTests}
import utest.{Tests, test}

object GetInteger extends JsTestBase {
  // getIntegerKey
  private val getIntegerKey = s"getInteger(callerTestData, \"key\")"
  private val getIntegerKeyArgBeforeFunc = s"callerTestData.getInteger(\"key\")"
  // getIntegerIndex
  private val getIntegerIndex = s"getInteger(callerTestData, $randomInt)"
  private val getIntegerIndexArgBeforeFunc = s"callerTestData.getInteger($randomInt)"
  // getIntegerValueKey
  private val getIntegerValueKey = s"getIntegerValue(callerTestData, \"key\")"
  private val getIntegerValueKeyArgBeforeFunc = s"callerTestData.getIntegerValue(\"key\")"
  // getIntegerValueIndex
  private val getIntegerValueIndex = s"getIntegerValue(callerTestData, $randomInt)"
  private val getIntegerValueIndexArgBeforeFunc = s"callerTestData.getIntegerValue($randomInt)"

  // invalid getInteger
  private val invalidGetIntegerKey = s"getInteger()"
  private val invalidGetIntegerArgBeforeFunc = s"callerTestData.getInteger()"
  // invalid getIntegerValue
  private val invalidGetIntegerValue = s"getIntegerValue()"
  private val invalidGetIntegerValueArgBeforeFunc = s"callerTestData.getIntegerValue()"

  val tests: Tests = Tests {
    // getInteger
    test.apply("check: function getInteger dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerKey,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getInteger dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getInteger dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getInteger dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getInteger dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getInteger dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getInteger dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getInteger dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getInteger dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getInteger dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getIntegerIndex
    test.apply("check: function getIntegerIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getIntegerValueKey
    test.apply("check: function getIntegerValueKey dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerValueKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerValueKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerValueKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerValueKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueKey dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getIntegerValueIndex
    test.apply("check: function getIntegerValueIndex dataTransaction compiles for V3 dataEntry") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for binaryEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerValueIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for integerEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerValueIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for stringEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerValueIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for booleanEntry - version V4 and more") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerValueIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for V3 dataEntry (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        dataEntryForTests(randomStringArrayElement),
        getIntegerValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for binaryEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          binaryEntryForTests,
          getIntegerValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for integerEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          integerEntryForTests,
          getIntegerValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for stringEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          getIntegerValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getIntegerValueIndex dataTransaction compiles for booleanEntry - version V4 and more (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          getIntegerValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // getInteger - Can't find a function overload
    test.apply("compilation error: function getInteger V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getInteger version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getInteger V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getInteger version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getIntegerIndex - Can't find a function overload
    test.apply("compilation error: function getIntegerIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getIntegerIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getIntegerIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getIntegerIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getIntegerValueKey - Can't find a function overload
    test.apply("compilation error: function getIntegerValueKey V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerValueKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getIntegerValueKey version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerValueKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getIntegerValueKey V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerValueKeyArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getIntegerValueKey version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerValueKeyArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // getIntegerValueIndex - Can't find a function overload
    test.apply("compilation error: function getIntegerValueIndex V3 - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerValueIndex,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getIntegerValueIndex version V4 and more - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerValueIndex,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function getIntegerValueIndex V3 - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomInt.toString,
        getIntegerValueIndexArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: function getIntegerValueIndex version V4 and more - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomBoolean.toString,
          getIntegerValueIndexArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getInteger
    test.apply("compilation error: invalid getInteger - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetIntegerKey,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getInteger - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetIntegerKey,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid getInteger - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetIntegerArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getInteger - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetIntegerArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    // invalid getIntegerValue
    test.apply("compilation error: invalid getIntegerValue - Can't find a function overload") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        integerEntryForTests,
        invalidGetIntegerValue,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getIntegerValue - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          booleanEntryForTests,
          invalidGetIntegerValue,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid getIntegerValue - Can't find a function overload (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeFromMatchingAndCase(
        binaryEntryForTests,
        invalidGetIntegerValueArgBeforeFunc,
        testData.rideV3Result,
        ""
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test.apply("compilation error: invalid getIntegerValue - Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          stringEntryForTests,
          invalidGetIntegerValueArgBeforeFunc,
          "",
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, V3, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
