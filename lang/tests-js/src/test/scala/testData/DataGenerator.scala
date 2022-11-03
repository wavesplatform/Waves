package testData

import scala.math.floor

class DataGenerator {
  val addressDataArray: Array[String] = Array(
    "Address(base58'')",
    "Address(base58'3MDaMwqLtwBGcJrTA5tstJfY95GqnNnDDAS')",
    "Address(base58'3PDaScqLtwBGcJrTA5tstJfY95GqnNnLxGA')",
    "Address(base58'3P3aScAJsxBGcJrTA5tstJfY95GqnNnHLGA')",
  )

  val aliasDataArray: Array[String] = Array(
    "Alias(\"merry_1312@pro\")",
    "Alias(\"four\")",
    "Alias(\"1111this_alias_30@long-symbols\")",
    "Alias(\"\")",
  )

  val byteVectorArray: Array[String] = Array(
    "base16'52696465'",
    "base58'8t38fWQhrYJsqxXtPpiRCEk1g5RJdq9bG5Rkr2N7mDFC'",
    "base64'UmlkZQ=='"
  )

  val stringArray: Array[String] = Array(
    "\"3P3aScAJsxBGcJrTA5tstJfY95GqnNnHLGAlsadjbf87\"",
    "\"8t38fWQhrYJsqxXtPpiRCEk1g5RJdq9bG5Rkr2N7mDFC\"",
    "\"3MDaMwqLtwBGcJrTA5tstJfY95GqnNnDDASmsakmd091\"",
    "\"3P3aScAJsxBGGqnNnHLGAlsadjbf87\"",
    "\"8t38fWQhrYJsqxXtPpiRasdiuh32he98ddr2N7\"",
    "\"091\""
  )

  val issuesArray: Array[String] = Array(
    "Issue(\"superToken\", \"token for super humans\", 1000, 2, true)",
    "Issue(\"trueToken\", \"real token in real life\", 5_000_000_000, 5, false)",
    "Issue(\"oneMoreToken\", \"just one more token in this crypto world\", 1_500_000_000, 5, true)",
    "Issue(\"assetForAll\", \"asset for all peoples\", 1_500_000_000, 5, true)",
  )

  val unionArray: Array[String] = Array("DOWN", "CEILING", "FLOOR", "HALFUP", "HALFEVEN")

  val digestAlgorithmTypeArray: Array[String] = Array(
    "NOALG", "MD5", "SHA1", "SHA224", "SHA256", "SHA384", "SHA512", "SHA3224", "SHA3256", "SHA3384", "SHA3512",
  )

  def randomAddressDataArray: String = addressDataArray[math.floor(123)]
}
