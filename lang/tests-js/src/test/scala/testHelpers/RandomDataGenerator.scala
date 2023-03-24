package testHelpers

import scala.collection.immutable.HashMap

object RandomDataGenerator {
  val rnd = new scala.util.Random

  val addressDataArray: Array[String] = Array(
    "Address(base58'')",
    "Address(base58'3MDaMwqLtwBGcJrTA5tstJfY95GqnNnDDAS')",
    "Address(base58'3PDaScqLtwBGcJrTA5tstJfY95GqnNnLxGA')",
    "Address(base58'3P3aScAJsxBGcJrTA5tstJfY95GqnNnHLGA')"
  )

  val aliasDataArray: Array[String] = Array(
    "Alias(\"merry_1312@pro\")",
    "Alias(\"four\")",
    "Alias(\"1111this_alias_30@long-symbols\")",
    "Alias(\"\")"
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
    "Issue(\"assetForAll\", \"asset for all peoples\", 1_500_000_000, 5, true)"
  )

  val unionArray: Array[String] = Array("DOWN", "CEILING", "FLOOR", "HALFUP", "HALFEVEN")

  val digestAlgorithmTypeArray: Array[String] = Array(
    "NOALG",
    "MD5",
    "SHA1",
    "SHA224",
    "SHA256",
    "SHA384",
    "SHA512",
    "SHA3224",
    "SHA3256",
    "SHA3384",
    "SHA3512"
  )

  def entryMap(key: String): String = {
    val entryArray: HashMap[String, String] = HashMap(
      "BinaryEntry"  -> s"[BinaryEntry(\"key\", $randomByteVectorArrayElement)]",
      "IntegerEntry" -> s"[IntegerEntry(\"key\", $randomInt)]",
      "StringEntry"  -> s"[StringEntry(\"key\", $randomStringArrayElement)]",
      "BooleanEntry" -> s"[BooleanEntry(\"key\", $randomBoolean)]"
    )
    entryArray(key)
  }

  def dataEntryForTests(value: String)              = s"[DataEntry(\"key\", $value)]"
  def randomAddressDataArrayElement: String         = addressDataArray(rnd.nextInt(addressDataArray.length))
  def randomAliasDataArrayElement: String           = aliasDataArray(rnd.nextInt(aliasDataArray.length))
  def randomByteVectorArrayElement: String          = byteVectorArray(rnd.nextInt(byteVectorArray.length))
  def randomStringArrayElement: String              = stringArray(rnd.nextInt(stringArray.length))
  def randomIssuesArrayElement: String              = issuesArray(rnd.nextInt(issuesArray.length))
  def randomUnionArrayElement: String               = unionArray(rnd.nextInt(unionArray.length))
  def randomDigestAlgorithmTypeArrayElement: String = digestAlgorithmTypeArray(rnd.nextInt(digestAlgorithmTypeArray.length))
  def randomInt: Integer                            = rnd.nextInt(90000000)
  def randomBoolean: Boolean                        = randomInt % 2 == 0
}
