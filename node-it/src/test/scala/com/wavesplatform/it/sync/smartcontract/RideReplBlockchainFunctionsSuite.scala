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
import org.scalatest.{Assertion, Ignore}
import org.scalatest.EitherValues._

import scala.concurrent.Await
import scala.concurrent.duration._

@Ignore
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
  private var assetId      = ""
  private var transferTxId = ""

  private val alias = "nickname"
  private val transferAmount = 100

  private def execute(expr: String): Either[String, String] =
    Await.result(repl.execute(expr), 2 seconds)

  private def assert(expr: String, result: String): Assertion =
    execute(expr).explicitGet() should endWith(result)

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

    sender.createAlias(bob.toAddress.stringRepr, alias, minFee).id
    assetId = sender.issue(alice.toAddress.stringRepr, "Asset", "descr", 1000, 2, waitForTx = true).id
    transferTxId = sender.transfer(alice.toAddress.stringRepr, s"alias:$chainId:$alias", transferAmount, minFee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferTxId)
  }

  test("this") {
    assert("this.toString()", s""""${alice.toAddress.stringRepr}"""")
  }

  test("height") {
    assert("height > 0", "true")
  }

  test("lastBlock") {
    assert("lastBlock.height == height", "true")
  }

  test("tx variable doesn't exist") {
    execute("tx").left.value should include("A definition of 'tx' is not found")
  }

  test("assetBalance()") {
    assert(s"this.assetBalance(base58'$assetId')", "= 900")
  }

  test("wavesBalance()") {
    assert(
      "this.wavesBalance()",
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
    assert(
      """this.getBinary("bin").value()""",
      s" base58\'${Base58.encode("binary".getBytes)}\'"
    )
  }

  test("getBinaryValue()") {
    assert(
      """this.getBinaryValue("bin")""",
      s" base58\'${Base58.encode("binary".getBytes)}\'"
    )
  }

  test("getBoolean()") {
    assert("""this.getBoolean("bool1").value()""", " true")
    assert("""this.getBoolean("bool2").value()"""," false")
  }

  test("getBooleanValue()") {
    assert("""this.getBooleanValue("bool1")""", " true")
    assert("""this.getBooleanValue("bool2")""", " false")
  }

  test("getInteger()") {
    assert("""this.getInteger("int").value()""", " 100500")
  }

  test("getIntegerValue()") {
    assert("""this.getIntegerValue("int")""", " 100500")
  }

  test("getString()") {
    assert("""this.getString("str").value()""", " \"Hello\"")
  }

  test("getStringValue()") {
    assert("""this.getStringValue("str")""", " \"Hello\"")
  }

  test("assetInfo()") {
    assert(
      s"assetInfo(base58'$assetId').value().issuer.toString()",
      s""""${alice.toAddress.stringRepr}""""
    )
  }

  test("blockInfoByHeight()") {
    val h  = miner.height - 1
    val bi = miner.blockAt(h)
    execute(s"let bi = blockInfoByHeight($h).value()")
    assert(
      s"bi",
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
    assert(s"transactionHeightById(base58'$dataTxId').value() > 0", "true")
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
        attachment = ByteStr.empty,
        timestamp = responseTx.timestamp
      )
      .explicitGet()
      .bodyBytes
      .value()

    execute(s"let transferTx = transferTransactionById(base58'$transferTxId').value()")
    assert(
      "transferTx",
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
    assert(
      s"addressFromPublicKey(base58'${alice.publicKey}').value().toString()",
      s""""${alice.toAddress.stringRepr}""""
    )
  }

  test("addressFromRecipient() with alias") {
    assert(
      s"""addressFromRecipient(transferTx.recipient).toString()""",
      s""""${bob.toAddress.stringRepr}""""
    )
  }

  test("addressFromString()") {
    assert(
      s"""addressFromString("${alice.toAddress.stringRepr}").value().toString()""",
      s""""${alice.toAddress.stringRepr}""""
    )
  }

  test("addressFromStringValue()") {
    assert(
      s"""addressFromStringValue("${alice.toAddress.stringRepr}").toString()""",
      s""""${alice.toAddress.stringRepr}""""
    )
  }
}
