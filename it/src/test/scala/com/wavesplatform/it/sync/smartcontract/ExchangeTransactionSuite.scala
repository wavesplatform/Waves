package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.NTP
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber
import com.wavesplatform.it.util._

class ExchangeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  var exchAsset: String = null

  val sc1 = s"""true"""
  val sc2 = s"""
               |match tx {
               |  case s : SetScriptTransaction => true
               |  case _ => false
               |}""".stripMargin

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    exchAsset = sender
      .issue(acc0.address, "ExchangeCoin", "ExchangeCoin for tests with exchange transaction", someAssetAmount, 0, reissuable = false, issueFee, 2)
      .id
    nodes.waitForHeightAriseAndTxPresent(exchAsset)
  }

  test("set simple contracts and put exchange transaction in blockchain") {
    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc1, sc1)
         )) {
      setContract(contr1, acc0)
      setContract(contr2, acc1)
      setContract(mcontr, acc2)

      val tx = exchangeTx()

      val txId = sender.signedBroadcast(tx).id
      nodes.waitForHeightAriseAndTxPresent(txId)
      //TODO : add assert balances
    }
  }

  test("negative: set simple contracts and put exchange transaction in blockchain") {
    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc2, sc1),
           (sc1, sc1, sc2)
         )) {
      setContract(contr1, acc0)
      setContract(contr2, acc1)
      setContract(mcontr, acc2)

      val tx = exchangeTx()
      assertBadRequestAndMessage(sender.signedBroadcast(tx), "Transaction not allowed by account-script")
      //TODO : add assert balances
    }
  }

  def setContract(contractText: String, acc: PrivateKeyAccount) = {
    val scriptText = contractText.stripMargin
    val script     = ScriptCompiler(scriptText).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, acc, Some(script), 0.014.waves, System.currentTimeMillis())
      .right
      .get
    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)
  }

  def exchangeTx() = {
    val buyer               = acc1
    val seller              = acc0
    val matcher             = acc2
    val time                = NTP.correctedTime()
    val expirationTimestamp = time + Order.MaxLiveTime
    val buyPrice            = 1 * Order.PriceConstant
    val sellPrice           = (0.50 * Order.PriceConstant).toLong
    val mf                  = 700000L
    val buyAmount           = 2
    val sellAmount          = 3
    val assetPair           = AssetPair.createAssetPair("WAVES", exchAsset).get
    val buy                 = Order.buy(buyer, matcher, assetPair, buyAmount, buyPrice, time, expirationTimestamp, mf, 2)
    val sell                = Order.sell(seller, matcher, assetPair, sellAmount, sellPrice, time, expirationTimestamp, mf, 2)

    val amount = math.min(buy.amount, sell.amount)
    val tx = ExchangeTransactionV2
      .create(
        matcher = matcher,
        buyOrder = buy,
        sellOrder = sell,
        amount = amount,
        price = sellPrice,
        buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
        sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
        fee = mf,
        timestamp = NTP.correctedTime()
      )
      .explicitGet()
      .json()

    tx
  }

}
