package com.wavesplatform.it.account.storage

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class RemoveEntrySuite extends BaseSuite {

  def writeEntry(t: String, v: String) =
    s"""
       |@Callable(i) func write$t(k: String, v: ${if (t.contains("Int")) "Int" else t}) = [${t}Entry(k, v${if (t.equals("Binary")) ".toBytes" else ""})]
       """.stripMargin

  val writeEntriesFunc = s"@Callable(i) func write%sEntries() = FOLD<%s>(a, [], writeEntry)"

  def deleteEntriesFunc(c: Int) = s"@Callable(i) func delete${c}Entries() = FOLD<$c>(a, [], deleteEntry)"

  val script = """
                 |{-# STDLIB_VERSION 4 #-}
                 |{-# SCRIPT_TYPE ACCOUNT #-}
                 |{-# CONTENT_TYPE DAPP #-}
                 |
                 |let a = [
                 |  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                 |  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                 |  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
                 |  61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
                 |  81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                 |]
                 |
                 |func writeEntry(acc: List[StringEntry], e: Int) = StringEntry(e.toString(), "value") :: acc
                 |
                 |func deleteEntry(acc: List[DeleteEntry], e: Int) = DeleteEntry(e.toString()) :: acc
                 |
                 |@Callable(i) func write(k: String, v: String) = [StringEntry(k, v)]
                 |
                 |@Callable(i) func delete(k: String) = [DeleteEntry(k)]
                 |
       """.stripMargin

  "Remove entry from account storage" - {
    val stringTestData  = "String"  -> "someValue"
    val integerTestData = "Integer" -> 1
    val booleanTestData = "Boolean" -> true
    val binaryTestData  = "Binary"  -> ""

    for (data <- Seq(stringTestData, integerTestData, booleanTestData, binaryTestData)) s"${data._1} entry could be removed from account storage" in {
      val t = data._1
      val k = "someKey"
      val v = data._2
      val address =
        createDapp(script, writeEntry(t, v.toString))

      invokeScript(address, s"write$t", k, v.toString)

      miner.getData(address) should have size 1
      miner.getDataByKey(address, k).key shouldBe k
      miner.getDataByKey(address, k).value shouldBe v
      miner.getDataByKey(address, k).getClass.getCanonicalName shouldBe s"com.wavesplatform.state.${t}DataEntry"

      invokeScript(address, "delete", k)

      miner.getData(address) should have size 0
    }

    "Removing nonexistent entry should not produce an error" in {
      val address = createDapp(script)

      invokeScript(address, "delete", "nonexistent-key")
    }

    "Could remove 100 entries" in {
      val address = createDapp(script, deleteEntriesFunc(100))

      miner.waitForTransaction((0 to 99).map(i => invokeScript(address, s"write", s"key-$i", "value", wait = false)).last)
      miner.getData(address) should have size 100

      miner.waitForTransaction(invokeScript(address, s"delete100Entries", wait = false))

      miner.getData(address) should have size 0
    }

    "Removing more than 100 entries should produce an error" in {
      val address = createDapp(script, deleteEntriesFunc(101))

      miner.waitForTransaction((0 to 100).map(i => invokeScript(address, s"write", s"key-$i", "value", wait = false)).last)
      miner.getData(address) should have size 101

      assertBadRequestAndMessage(
        miner.waitForTransaction(invokeScript(address, s"delete101Entries", wait = false)),
        "Error while executing account-script: List size exceed 101"
      )

      miner.getData(address) should have size 0
    }

    "Trying of writing key longer than 400 bytes and removing it should produce an error" in {
      val address    = createDapp(script, deleteEntriesFunc(101))
      val tooLongKey = new scala.util.Random().nextString(401)

      assertBadRequestAndMessage(
        miner.waitForTransaction(invokeScript(address, s"write", tooLongKey, "value", wait = false)),
        "State check failed. Reason: Key size must be less than 100"
      )
    }
  }

  def createDapp(scriptParts: String*): String = {
    val script  = scriptParts.mkString(" ")
    val address = miner.createAddress()
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    miner.transfer(sender.address, address, 10.waves, minFee, waitForTx = true)

    nodes.waitForHeightAriseAndTxPresent(
      miner
        .signedBroadcast(
          SetScriptTransaction
            .selfSigned(1.toByte, KeyPair(Base58.decode(miner.seed(address))), Some(compiledScript), setScriptFee, System.currentTimeMillis())
            .explicitGet()
            .json
            .value
        )
        .id
    )

    address
  }

  def invokeScript(address: String, function: String, key: String = "", value: String = "", wait: Boolean = true): String = {
    val args = function match {
      case "write"            => List(CONST_STRING(key).explicitGet(), CONST_STRING(value).explicitGet())
      case "writeString"      => List(CONST_STRING(key).explicitGet(), CONST_STRING(value).explicitGet())
      case "writeInteger"     => List(CONST_STRING(key).explicitGet(), CONST_LONG(value.toLong))
      case "writeBoolean"     => List(CONST_STRING(key).explicitGet(), CONST_BOOLEAN(value.toBoolean))
      case "writeBinary"      => List(CONST_STRING(key).explicitGet(), CONST_BYTESTR(value.getBytes()))
      case "delete"           => List(CONST_STRING(key).explicitGet())
      case "delete100Entries" => List.empty
      case "delete101Entries" => List.empty
      case _                  => List.empty
    }

    val tx = miner
      .invokeScript(
        address,
        address,
        fee = smartMinFee + smartFee,
        waitForTx = wait,
        func = Some(function),
        args = args
      )
      ._1
      .id

    if (wait) nodes.waitForHeightAriseAndTxPresent(tx)
    tx
  }
}
