package testHelpers

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5, V6}
import testHelpers.RandomDataGenerator.entryMap

object TestDataConstantsAndMethods {
  val oldVersions: Iterable[StdLibVersion]                      = Seq(V3, V4)
  val versionsSupportingTheNewFeatures: Iterable[StdLibVersion] = Seq(V5, V6)
  val versionsWithoutV6: Iterable[StdLibVersion]                = Seq(V3, V4, V5)
  val actualVersionsWithoutV3: Iterable[StdLibVersion]          = Seq(V4, V5, V6)
  val actualVersions: Iterable[StdLibVersion]                   = Seq(V3, V4, V5, V6)
  val CANT_FIND_A_FUNCTION_OVERLOAD                             = "Can't find a function overload"
  val CANT_FIND_FUNCTION                                        = "Can't find a function"
  val CANT_MATCH_INFERRED_TYPE                                  = "Can't match inferred types of T over"
  val MATCHING_NOT_EXHAUSTIVE                                   = "Matching not exhaustive:"
  val UNDEFINED_TYPE                                            = "Undefined type: `BigInt`"
  val stringList                                                = "[\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\"]"
  val intList                                                   = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
  val thisVariable                                              = "this"

  def nonMatchingTypes(expectType: String): String = {
    s"Non-matching types: expected: $expectType"
  }

  def invalidFunctionError(functionName: String, numberOfArguments: Integer): String = {
    s"Function '$functionName' requires $numberOfArguments arguments"
  }

  val binaryEntryForTests: String  = entryMap("BinaryEntry")
  val integerEntryForTests: String = entryMap("IntegerEntry")
  val stringEntryForTests: String  = entryMap("StringEntry")
  val booleanEntryForTests: String = entryMap("BooleanEntry")

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
