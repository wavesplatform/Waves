package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransferTransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.transfer.TransferTransaction
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

  private val chainId: Char = miner.settings.blockchainSettings.addressSchemeCharacter

  private val settings = NodeConnectionSettings(miner.nodeApiEndpoint.toString, chainId.toByte, alice.toAddress.stringRepr)
  private val repl     = Repl(Some(settings))

  private var dataTxId     = ""
  private var aliasTxId    = ""
  private var assetId      = ""
  private var transferTxId = ""

  private val alias = "nickname"
  private val transferAmount = 100

  private def execute(expr: String): Either[String, String] =
    Await.result(repl.execute(expr), 2 seconds)

  test("prepare") {
    dataTxId = sender
      .putData(
        alice.toAddress.stringRepr,
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

    aliasTxId = sender.createAlias(bob.toAddress.stringRepr, alias, minFee).id
    assetId = sender.issue(alice.toAddress.stringRepr, "Asset", "descr", 1000, 2, waitForTx = true).id
    transferTxId = sender.transfer(alice.toAddress.stringRepr, s"alias:$chainId:$alias", transferAmount, minFee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferTxId)
  }

  test("this") {
    execute("this.toString()").right.value should endWith(s""""${alice.toAddress.stringRepr}"""")
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
    execute(s"this.assetBalance(base58'$assetId')").right.value should endWith(" 900")
  }

  test("wavesBalance()") {
    execute("this.wavesBalance()").right.value should endWith(
      """
        |BalanceDetails(
        |	available = 9899800000
        |	regular = 9899800000
        |	generating = 0
        |	effective = 9899800000
        |)
      """.trim.stripMargin
    )
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
    execute(s"assetInfo(base58'$assetId').value().issuer.toString()").right.value should endWith(s""""${alice.toAddress.stringRepr}"""")
  }

  test("blockInfoByHeight()") {
    val h  = miner.height - 1
    val bi = miner.blockAt(h)
    execute(s"let bi = blockInfoByHeight($h).value()")
    execute(s"bi").right.value should endWith(
      s"""
         |BlockInfo(
         |	baseTarget = ${bi.baseTarget.get}
         |	generator = Address(
         |		bytes = base58'${bi.generator}'
         |	)
         |	timestamp = ${bi.timestamp}
         |	vrf = base58'${bi.vrf.get}'
         |	height = ${bi.height}
         |	generationSignature = base58'${bi.generationSignature.get}'
         |	generatorPublicKey = base58'${bi.generatorPublicKey}'
         |)
      """.trim.stripMargin
    )
  }

  test("transactionHeightById()") {
    execute(s"transactionHeightById(base58'$dataTxId').value() > 0").right.value should endWith("true")
  }

  test("transferTransactionById()") {
    val responseTx = sender.transactionInfo[TransferTransactionInfo](transferTxId)
    val bodyBytes = TransferTransaction.selfSigned(
        version = TxVersion.V2,
        sender = alice,
        recipient = Alias.createWithChainId(alias, chainId.toByte).explicitGet(),
        asset = IssuedAsset(ByteStr.decodeBase58(assetId).get),
        amount = transferAmount,
        feeAsset = Waves,
        fee = responseTx.fee,
        attachment = None,
        timestamp = responseTx.timestamp
      )
      .explicitGet()
      .bodyBytes
      .value()

    execute(s"let transferTx = transferTransactionById(base58'$transferTxId').value()")
    execute(s"transferTx").right.value should endWith(
      s"""
         |TransferTransaction(
         |	recipient = Alias(
         |		alias = "$alias"
         |	)
         |	timestamp = ${responseTx.timestamp}
         |	bodyBytes = base58'${Base58.encode(bodyBytes)}'
         |	assetId = base58'$assetId'
         |	feeAssetId = Unit
         |	amount = 100
         |	version = 2
         |	id = base58'$transferTxId'
         |	senderPublicKey = base58'${alice.publicKey}'
         |	attachment = base58''
         |	sender = Address(
         |		bytes = base58'${responseTx.sender.get}'
         |	)
         |	proofs = [base58'${responseTx.proofs.get.head}', base58'', base58'', base58'', base58'', base58'', base58'', base58'']
         |	fee = ${responseTx.fee}
         |)
       """.trim.stripMargin
    )
  }

  test("addressFromPublicKey()") {
    execute(s"addressFromPublicKey(base58'${alice.publicKey}').value().toString()").right.value should endWith(
      s""""${alice.toAddress.stringRepr}""""
    )
  }

  test("addressFromRecipient() with alias") {
    execute(s"""addressFromRecipient(transferTx.recipient).toString()""").explicitGet() should endWith(s""""${bob.toAddress.stringRepr}"""")
  }

  test("addressFromString()") {
    execute(s"""addressFromString("${alice.toAddress.stringRepr}").value().toString()""").right.value should endWith(
      s""""${alice.toAddress.stringRepr}""""
    )
  }

  test("addressFromStringValue()") {
    execute(s"""addressFromStringValue("${alice.toAddress.stringRepr}").toString()""").right.value should endWith(
      s""""${alice.toAddress.stringRepr}""""
    )
  }
}
