package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.http.NodeConnectionSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import org.scalatest.EitherValues._

class RideReplBlockchainFunctionsSuite extends BaseTransactionSuite {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  private val alice = pkByAddress(firstAddress)
  private val bob = pkByAddress(secondAddress)

  val chainId: Char = miner.settings.blockchainSettings.addressSchemeCharacter
  val settings = NodeConnectionSettings(miner.nodeApiEndpoint.toString, chainId.toByte, alice.stringRepr)
  val repl = Repl(Some(settings))

  var dataTxId = ""
  var aliasTxId = ""
  var assetId = ""
  var transferTxId = ""

  test("prepare") {
    dataTxId = sender.putData(alice.stringRepr, List(
      BinaryDataEntry("bin", ByteStr("binary".getBytes)),
      BooleanDataEntry("bool1", true),
      BooleanDataEntry("bool2", false),
      IntegerDataEntry("int", 100500),
      StringDataEntry("str", "Hello")
    ), minFee).id

    aliasTxId = sender.createAlias(bob.stringRepr, "nickname", minFee).id
    assetId = sender.issue(
      alice.stringRepr, "Asset", "descr", 1000, 2, waitForTx = true).id
    transferTxId = sender.transfer(
      alice.stringRepr, s"alias:$chainId:nickname", 100, minFee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferTxId)
  }
//TODO Alias
  test("this") {
    repl.execute("this.toString()").right.value should endWith(alice.stringRepr)
  }
  test("height") {
    repl.execute("height > 0").right.value should endWith("true")
  }
  test("lastBlock") {
    repl.execute("lastBlock.height == height").right.value should endWith("true")
  }
  test("tx variable doesn't exist") {
    repl.execute("tx").left.value should include("A definition of 'tx' is not found")
  }

  test("assetBalance()") {
    repl.execute(s"this.assetBalance(base58'$assetId')").right.value should endWith (" 900")
  }
  test("wavesBalance()") {
    repl.execute("this.wavesBalance()").right.value should endWith(" 9899800000")
  }

  test("getBinary()") {
    repl.execute("""this.getBinary("bin").value()""")
      .right.value should endWith(s" ${Base58.encode("binary".getBytes)}")
  }
  test("getBinaryValue()") {
    repl.execute("""this.getBinaryValue("bin")""")
      .right.value should endWith(s" ${Base58.encode("binary".getBytes)}")
  }
  test("getBoolean()") {
    repl.execute("""this.getBoolean("bool1").value()""").right.value should endWith(" true")
    repl.execute("""this.getBoolean("bool2").value()""").right.value should endWith(" false")
  }
  test("getBooleanValue()") {
    repl.execute("""this.getBooleanValue("bool1")""").right.value should endWith(" true")
    repl.execute("""this.getBooleanValue("bool2")""").right.value should endWith(" false")
  }
  test("getInteger()") {
    repl.execute("""this.getInteger("int").value()""").right.value should endWith(" 100500")
  }
  test("getIntegerValue()") {
    repl.execute("""this.getIntegerValue("int")""").right.value should endWith(" 100500")
  }
  test("getString()") {
    repl.execute("""this.getString("str").value()""").right.value should endWith(" Hello")
  }
  test("getStringValue()") {
    repl.execute("""this.getStringValue("str")""").right.value should endWith(" Hello")
  }

  test("assetInfo()") {
    repl.execute(s"assetInfo(base58'$assetId').value().issuer.toString()")
      .right.value should endWith(alice.stringRepr)
  }
  test("blockInfoByHeight()") {
    val h = miner.height - 1
    repl.execute(s"blockInfoByHeight($h).value().height")
      .right.value should endWith(s" $h")
  }

  test("transactionHeightById()") {
    repl.execute(s"transactionHeightById(base58'$dataTxId').value() > 0").right.value should endWith("true")
  }

  test("transferTransactionById()") {
    repl.execute(s"let transferTx = transferTransactionById(base58'$transferTxId').value()")
    repl.execute(s"transferTx.sender.toString()").right.value should endWith(alice.stringRepr)
  }

  test("addressFromPublicKey()") {
    repl.execute(s"addressFromPublicKey(base58'${alice.publicKey.stringRepr}').value().toString()")
      .right.value should endWith(alice.stringRepr)
  }
  test("addressFromRecipient() with alias") {
    repl.execute(s"addressFromRecipient(transferTx.recipient).toString()")
      .right.value should endWith(bob.stringRepr)
  }
  test("addressFromString()") {
    repl.execute(s"""addressFromString("${alice.stringRepr}").value().toString()""")
      .right.value should endWith(alice.stringRepr)
  }
  test("addressFromStringValue()") {
    repl.execute(s"""addressFromStringValue("${alice.stringRepr}").toString()""")
      .right.value should endWith(alice.stringRepr)
  }

}
