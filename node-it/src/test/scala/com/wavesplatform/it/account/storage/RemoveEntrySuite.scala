package com.wavesplatform.it.account.storage

import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

case class WriteEntry(ct: String, t: String, v: Any, k: String = "somekey")

class RemoveEntrySuite extends BaseSuite {

  private val stringTestData  = WriteEntry("String", "String", "somevalue")
  private val integerTestData = WriteEntry("Integer", "Int", 1)
  private val booleanTestData = WriteEntry("Boolean", "Boolean", true)
  private val binaryTestData  = WriteEntry("Binary", "ByteVector", "bytes")

  def writeEntry(we: WriteEntry): String = s"@Callable(i) func write${we.ct}(k: String, v: ${we.t}) = [${we.ct}Entry(k, v)]"

  private val script = """
                 |{-# STDLIB_VERSION 4 #-}
                 |{-# SCRIPT_TYPE ACCOUNT #-}
                 |{-# CONTENT_TYPE DAPP #-}
                 |
                 |let a100 = [
                 |  "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                 |  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                 |  "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60",
                 |  "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                 |  "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"
                 |]
                 |
                 |let a101 = a100 ++ ["100"]
                 |
                 |func writeEntry(acc: List[StringEntry], e: Int) = StringEntry(e.toString(), "value") :: acc
                 |
                 |func deleteEntry(acc: List[DeleteEntry], e: String) = DeleteEntry(e) :: acc
                 |
                 |@Callable(i) func delete100Entries() = { FOLD<100>(a100, [], deleteEntry) }
                 |
                 |@Callable(i) func delete101Entries() = { FOLD<101>(a101, [], deleteEntry) }
                 |
                 |@Callable(i) func write(k: String, v: String) = [StringEntry(k, v)]
                 |
                 |@Callable(i) func write4() = [
                 |   StringEntry("96", "1"),
                 |   IntegerEntry("97", 1),
                 |   BooleanEntry("98", true),
                 |   BinaryEntry("99", "1".toBytes()),
                 |   BinaryEntry("100", "1".toBytes())
                 |]
                 |
                 |@Callable(i) func delete(k: String) = [DeleteEntry(k)]
                 |
       """.stripMargin

  "Remove entry from account storage" - {
    for (data <- Seq(stringTestData, integerTestData, booleanTestData, binaryTestData)) s"${data.ct}Entry could be removed from account storage" in {
      val keyPair = createDapp(script, writeEntry(data))
      val v       = if (data.ct.equals("Binary")) ByteStr(data.v.toString.getBytes()) else data.v

      invokeScript(keyPair, s"write${data.ct}", data.k, data.v.toString)

      nodes.waitForHeightArise() //TODO: delete this line after NODE-2099 will be done

      val address = keyPair.toAddress.toString
      miner.getData(address) should have size 1
      miner.getDataByKey(address, data.k).key shouldBe data.k
      miner.getDataByKey(address, data.k).value shouldBe v
      miner.getDataByKey(address, data.k).getClass.getCanonicalName shouldBe s"com.wavesplatform.state.${data.ct}DataEntry"

      invokeScript(keyPair, "delete", data.k)

      miner.getData(address) should have size 0
    }

    "Removing nonexistent entry should not produce an error" in {
      invokeScript(createDapp(script), "delete", "nonexistent-key")
    }

    "Could remove 100 entries" in {
      val keyPair = createDapp(script)

      invokeScript(keyPair, s"write4")

      val data = (0 to 92).map { i =>
        StringDataEntry(s"$i", "q")
      } ++ List(IntegerDataEntry("93", 1), BooleanDataEntry("94", true), BinaryDataEntry("95", ByteStr("1212".getBytes())))

      miner.putData(keyPair, data.toList, 1.waves, true)

      miner.getData(keyPair.toAddress.toString) should have size 101

      miner.waitForTransaction(invokeScript(keyPair, s"delete100Entries"))

      miner.getData(keyPair.toAddress.toString) should have size 1
    }

    "Removing more than 100 entries should produce an error" in {
      val keyPair = createDapp(script)
      invokeScript(keyPair, s"write4")
      val data = (100 to 196).map { i =>
        StringDataEntry(s"$i", "q")
      }

      miner.putData(keyPair, data.toList, 1.waves, true)
      miner.getData(keyPair.toAddress.toString) should have size 101

      assertApiError(
        invokeScript(keyPair, s"delete101Entries"),
        AssertiveApiError(ScriptExecutionError.Id, "Stored data count limit is exceeded", matchMessage = true)
      )

      miner.getData(keyPair.toAddress.toString) should have size 101
    }

    "Trying of writing key longer than 400 bytes and removing it should produce an error" in {
      val address    = createDapp(script)
      val tooLongKey = new scala.util.Random().nextPrintableChar().toString * 401

      assertBadRequestAndMessage(invokeScript(address, s"write", tooLongKey, "value"), "Data entry key size = 401 bytes must be less than 400")
    }
  }

  def createDapp(scriptParts: String*): KeyPair = {
    val script  = scriptParts.mkString(" ")
    val address = miner.createKeyPair()
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV3
      )
      .explicitGet()
      ._1

    miner.transfer(sender.keyPair, address.toAddress.toString, 10.waves, minFee, waitForTx = true)

    nodes.waitForTransaction(
      miner
        .setScript(address, Some(compiledScript.bytes().base64), setScriptFee, 1.toByte, true)
        .id
    )

    address
  }

  def invokeScript(address: KeyPair, function: String, key: String = "", value: String = "", wait: Boolean = true): String = {
    val args = function match {
      case "write"            => List(CONST_STRING(key).explicitGet(), CONST_STRING(value).explicitGet())
      case "writeString"      => List(CONST_STRING(key).explicitGet(), CONST_STRING(value).explicitGet())
      case "writeInteger"     => List(CONST_STRING(key).explicitGet(), CONST_LONG(value.toLong))
      case "writeBoolean"     => List(CONST_STRING(key).explicitGet(), CONST_BOOLEAN(value.toBoolean))
      case "writeBinary"      => List(CONST_STRING(key).explicitGet(), CONST_BYTESTR(ByteStr(value.getBytes())).explicitGet())
      case "delete"           => List(CONST_STRING(key).explicitGet())
      case "write4"           => List.empty
      case "delete100Entries" => List.empty
      case "delete101Entries" => List.empty
      case _                  => List.empty
    }

    val tx = miner
      .invokeScript(
        address,
        address.publicKey.toAddress.toString,
        fee = smartMinFee + smartFee,
        waitForTx = wait,
        func = Some(function),
        args = args
      )
      ._1
      .id

    if (wait) nodes.waitForTransaction(tx)
    tx
  }
}
