package testData

class TestDataConstants {
  val CANT_FIND_A_FUNCTION_OVERLOAD = "Can't find a function overload"
  val LATEST_ESTIMATOR = 3
  val STDLIB_INVALID_VERSION = 44
  val stringList = "[\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\"]"
  val intList = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"

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
