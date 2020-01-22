package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import org.scalatest.EitherValues._

import scala.concurrent.Await
import scala.concurrent.duration._

class RideReplBlockchainFunctionsSuite extends BaseTransactionSuite {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  private val alice = pkByAddress(firstAddress)
  private val bob   = pkByAddress(secondAddress)

  val chainId: Char = miner.settings.blockchainSettings.addressSchemeCharacter
  val settings      = NodeConnectionSettings(miner.nodeApiEndpoint.toString, chainId.toByte, alice.stringRepr)
  val repl          = Repl(Some(settings))

  var dataTxId     = ""
  var aliasTxId    = ""
  var assetId      = ""
  var transferTxId = ""

  private def execute(expr: String): Either[String, String] =
    Await.result(repl.execute(expr), 2 seconds)

  test("prepare") {
    dataTxId = sender
      .putData(
        alice.stringRepr,
        List(
          BinaryDataEntry("bin", ByteStr("binary".getBytes)),
          BooleanDataEntry("bool1", true),
          BooleanDataEntry("bool2", false),
          IntegerDataEntry("int", 100500),
          StringDataEntry("str", "Hello")
        ),
        minFee
      )
      .id

    aliasTxId = sender.createAlias(bob.stringRepr, "nickname", minFee).id
    assetId = sender.issue(alice.stringRepr, "Asset", "descr", 1000, 2, waitForTx = true).id
    transferTxId = sender.transfer(alice.stringRepr, s"alias:$chainId:nickname", 100, minFee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferTxId)
  }

  test("this") {
    execute("this.toString()").right.value should endWith(s""""${alice.stringRepr}"""")
  }
  test("height") {
    execute("height > 0").right.value should endWith("true")
  }
  test("lastBlock") {
    execute("lastBlock.height == height").right.value should endWith("true")
  }
  test("tx variable doesn't exist") {
    execute("tx").left.value should include("A definition of 'tx' is not found")
  }

  test("assetBalance()") {
    //TODO always returns waves balance instead of asset
    execute(s"this.assetBalance(base58'$assetId')").right.value should endWith(" 900")
  }
  test("wavesBalance()") {
    execute("this.wavesBalance()").right.value should endWith(" 9899800000")
  }

  test("getBinary()") {
    execute("""this.getBinary("bin").value()""").right.value should endWith(s" base58\'${Base58.encode("binary".getBytes)}\'")
  }
  test("getBinaryValue()") {
    execute("""this.getBinaryValue("bin")""").right.value should endWith(s" base58\'${Base58.encode("binary".getBytes)}\'")
  }
  test("getBoolean()") {
    execute("""this.getBoolean("bool1").value()""").right.value should endWith(" true")
    execute("""this.getBoolean("bool2").value()""").right.value should endWith(" false")
  }
  test("getBooleanValue()") {
    execute("""this.getBooleanValue("bool1")""").right.value should endWith(" true")
    execute("""this.getBooleanValue("bool2")""").right.value should endWith(" false")
  }
  test("getInteger()") {
    execute("""this.getInteger("int").value()""").right.value should endWith(" 100500")
  }
  test("getIntegerValue()") {
    execute("""this.getIntegerValue("int")""").right.value should endWith(" 100500")
  }
  test("getString()") {
    execute("""this.getString("str").value()""").right.value should endWith(" \"Hello\"")
  }
  test("getStringValue()") {
    execute("""this.getStringValue("str")""").right.value should endWith(" \"Hello\"")
  }

  test("assetInfo()") {
    execute(s"assetInfo(base58'$assetId').value().issuer.toString()").right.value should endWith(s""""${alice.stringRepr}"""")
  }
  test("blockInfoByHeight()") {
    val h  = miner.height - 1
    val bi = miner.blockAt(h)
    execute(s"let bi = blockInfoByHeight($h).value()")
    //TODO possibly using wrong chain Id for generator address
    execute(s"bi").right.value should endWith(s"""BlockInfo(
        |	baseTarget = ${bi.nxtConsensus.baseTarget}
        |	generator = Address(
        |		bytes = base58'${bi.generator}'
        |	)
        |	timestamp = ${bi.timestamp}
        |	height = ${bi.height}
        |	generationSignature = base58'${bi.nxtConsensus.generationSignature}'
        |	generatorPublicKey = base58'${miner.publicKey.stringRepr}'
        |""".stripMargin)
  }

  test("transactionHeightById()") {
    execute(s"transactionHeightById(base58'$dataTxId').value() > 0").right.value should endWith("true")
  }

  test("transferTransactionById()") {
    execute(s"let transferTx = transferTransactionById(base58'$transferTxId').value()")
    //TODO Left because recipient alias can't be parsed
    execute(s"transferTx").right.value should endWith("""TransferTransaction(
        |	feeAssetId = ""
        |	amount = ""
        |	assetId = ""
        |	recipient = ""
        |	attachment = ""
        |	id = ""
        |	fee = ""
        |	timestamp = ""
        |	version = ""
        |	sender = ""
        |	senderPublicKey = ""
        |	bodyBytes = ""
        |	proofs = ""
        |""".stripMargin)
  }

  test("addressFromPublicKey()") {
    //TODO possibly using wrong chain Id
    execute(s"addressFromPublicKey(base58'${alice.publicKey.stringRepr}').value().toString()").right.value should endWith(alice.stringRepr)
  }
  test("addressFromRecipient() with alias") {
    //TODO Left because recipient alias can't be parsed
    execute(s"addressFromRecipient(transferTx.recipient).toString()").right.value should endWith(bob.stringRepr)
  }
  test("addressFromString()") {
    execute(s"""addressFromString("${alice.stringRepr}").value().toString()""").right.value should endWith(s""""${alice.stringRepr}"""")
  }
  test("addressFromStringValue()") {
    execute(s"""addressFromStringValue("${alice.stringRepr}").toString()""").right.value should endWith(s""""${alice.stringRepr}"""")
  }

}
