package testHelpers

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5, V6}

object TestDataConstantsAndMethods {

  val oldVersions: Iterable[StdLibVersion]                      = Seq(V3, V4)
  val versionsSupportingTheNewFeatures: Iterable[StdLibVersion] = Seq(V5, V6)

  val actualVersionsWithoutV3: Iterable[StdLibVersion] = Seq(V4, V5, V6)
  val actualVersions: Iterable[StdLibVersion]          = Seq(V3, V4, V5, V6)

  val CANT_FIND_A_FUNCTION_OVERLOAD = "Can't find a function overload"
  val CANT_FIND_FUNCTION            = "Can't find a function"

  def nonMatchingTypes(expectType: String): String = {
    s"Non-matching types: expected: $expectType"
  }

  def invalidFunctionError(functionName: String, numberOfArguments: Integer): String = {
    s"Function '$functionName' requires $numberOfArguments arguments"
  }

  val LATEST_ESTIMATOR       = 3
  val STDLIB_INVALID_VERSION = 44
  val stringList             = "[\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\"]"
  val intList                = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"

  val rideV3Result: String =
    """
      |WriteSet([
      |    DataEntry("dataType", val)
      |  ])
      |""".stripMargin

  val GreaterV3ResultBinaryEntry: String =
    """
      |[
      |    BinaryEntry("bin", val)
      |]
      |""".stripMargin

  val GreaterV3ResultBooleanEntry: String =
    """
      |[
      |    BooleanEntry("boolean", val)
      |]
      |""".stripMargin

  val GreaterV3ResultIntegerEntry: String =
    """
      |[
      |    IntegerEntry("integer", val)
      |]
      |""".stripMargin

  val GreaterV3ResultStringEntry: String =
    """
      |[
      |    StringEntry("String", val)
      |]
      |""".stripMargin
}
