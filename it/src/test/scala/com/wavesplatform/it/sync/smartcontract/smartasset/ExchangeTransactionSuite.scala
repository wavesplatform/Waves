package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.{cryptoContext, pureContext, wavesContext}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.state._
import com.wavesplatform.transaction.DataTransaction
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.NTP
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsObject
import scorex.crypto.encode.Base64

class ExchangeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  private var dtx: DataTransaction = _

  private val sc1 = Some(s"""true""")

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", value = true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val entry4 = StringDataEntry("str", "test")

    dtx = DataTransaction.selfSigned(1, acc0, List(entry1, entry2, entry3, entry4), 0.001.waves, NTP.correctedTime()).explicitGet()
    val dtxId = sender.signedBroadcast(dtx.json()).id
    nodes.waitForHeightAriseAndTxPresent(dtxId)
  }

  test("require using a certain matcher with smart accounts") {
    /*
    combination of smart accounts and smart assets
     */
    val script = Some(ScriptCompiler(s"""
                                       |match tx {
                                       |case s : SetAssetScriptTransaction => true
                                       |case e: ExchangeTransaction => e.sender == addressFromPublicKey(base58'${ByteStr(acc2.publicKey).base58}')
                                       |case _ => false}""".stripMargin).explicitGet()._1.bytes.value.base64)

    val smartAsset = sender
      .issue(firstAddress, "SmartAsset", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, script)
      .id
    nodes.waitForHeightAriseAndTxPresent(smartAsset)

    val smartPair = AssetPair(ByteStr.decodeBase58(smartAsset).toOption, None)

    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc1, sc1),
           (None, sc1, None)
         )) {
      setContract(contr1, acc0)
      setContract(contr2, acc1)
      setContract(mcontr, acc2)

      val txId = sender.signedBroadcast(exchangeTx(smartPair)).id
      nodes.waitForHeightAriseAndTxPresent(txId)
    }

    val scriptUpdated = Some(ScriptCompiler(s"""
                                          |match tx {
                                          |case s : SetAssetScriptTransaction => true
                                          |case e: ExchangeTransaction => e.sender == addressFromPublicKey(base58'${ByteStr(acc1.publicKey).base58}')
                                          |case _ => false}""".stripMargin).explicitGet()._1.bytes.value.base64)

    val scriptUpdateTx = sender.setAssetScript(smartAsset, firstAddress, setAssetScriptFee, scriptUpdated).id
    nodes.waitForHeightAriseAndTxPresent(scriptUpdateTx)

    assertBadRequestAndMessage(sender.signedBroadcast(exchangeTx(smartPair)).id, errNotAllowedByToken)
    
    setContract(None, acc0)
    setContract(None, acc1)
    setContract(None, acc2)
  }

  test("AssetPair from smart assets") {
    val assetA = sender
      .issue(firstAddress, "assetA", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, Some(scriptBase64))
      .id
    nodes.waitForHeightAriseAndTxPresent(assetA)
    val assetB = sender
      .issue(secondAddress, "assetB", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, Some(scriptBase64))
      .id
    nodes.waitForHeightAriseAndTxPresent(assetB)

    val toATx = sender.transfer(secondAddress, firstAddress, 1000, minFee + smartExtraFee, Some(assetB)).id
    nodes.waitForHeightAriseAndTxPresent(toATx)

    val toBTx = sender.transfer(firstAddress, secondAddress, 1000, minFee + smartExtraFee, Some(assetA)).id
    nodes.waitForHeightAriseAndTxPresent(toBTx)

    val script = Some(ScriptCompiler(s"""
                                        |let assetA = base58'$assetA'
                                        |let assetB = base58'$assetB'
                                        |match tx {
                                        |case s : SetAssetScriptTransaction => true
                                        |case e: ExchangeTransaction => (e.sellOrder.assetPair.priceAsset == assetA || e.sellOrder.assetPair.amountAsset == assetA) && (e.sellOrder.assetPair.priceAsset == assetB || e.sellOrder.assetPair.amountAsset == assetB)
                                        |case _ => false}""".stripMargin).explicitGet()._1.bytes.value.base64)

    val txAId = sender.setAssetScript(assetA, firstAddress, setAssetScriptFee, script).id
    nodes.waitForHeightAriseAndTxPresent(txAId)

    val txBId = sender.setAssetScript(assetB, secondAddress, setAssetScriptFee, script).id
    nodes.waitForHeightAriseAndTxPresent(txBId)

    val smartAssetPair = AssetPair(
      amountAsset = Some(ByteStr.decodeBase58(assetA).get),
      priceAsset = Some(ByteStr.decodeBase58(assetB).get)
    )

    val txId = sender.signedBroadcast(exchangeTx(smartAssetPair)).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    withClue("check fee for smart accounts and smart AssetPair - extx.fee == 0.023.waves") {
      setContract(sc1, acc0)
      setContract(sc1, acc1)
      setContract(sc1, acc2)

      assertBadRequestAndMessage(
        sender.signedBroadcast(exchangeTx(smartAssetPair)).id,
        "com.wavesplatform.transaction.assets.exchange.ExchangeTransactionV2 does not exceed minimal value of 2300000"
      )

      val txId = sender.signedBroadcast(exchangeTx(smartAssetPair, someSmart = false)).id
      nodes.waitForHeightAriseAndTxPresent(txId)
    }

    withClue("try to use incorrect assetPair") {
      val incorrectSmartAssetPair = AssetPair(
        amountAsset = Some(ByteStr.decodeBase58(assetA).get),
        priceAsset = None
      )
      assertBadRequestAndMessage(sender.signedBroadcast(exchangeTx(incorrectSmartAssetPair)).id, errNotAllowedByToken)
    }
  }

  test("use all functions from RIDE for asset script") {
    val script1 = Some(ScriptCompiler(cryptoContext(dtx).get).explicitGet()._1.bytes.value.base64)
    val script2 = Some(ScriptCompiler(pureContext(dtx).get).explicitGet()._1.bytes.value.base64)
    val script3 = Some(ScriptCompiler(wavesContext(dtx).get).explicitGet()._1.bytes.value.base64)

    for (script <- List(script1, script2, script3)) {
      val asset = sender
        .issue(firstAddress, "assetA", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, script)
        .id
      nodes.waitForHeightAriseAndTxPresent(asset)

      val smartPair = AssetPair(ByteStr.decodeBase58(asset).toOption, None)

      val txId = sender.signedBroadcast(exchangeTx(smartPair)).id
      nodes.waitForHeightAriseAndTxPresent(txId)
    }
  }

  def exchangeTx(pair: AssetPair, someSmart: Boolean = true): JsObject = {
    val matcher     = acc2
    val sellPrice   = (0.50 * Order.PriceConstant).toLong
    val (buy, sell) = orders(pair, 2, someSmart)

    val amount = math.min(buy.amount, sell.amount)

    val matcherFee     = if (someSmart) 1900000L else 2300000L
    val sellMatcherFee = LimitOrder.getPartialFee(sell.matcherFee, sell.amount, amount)
    val buyMatcherFee  = LimitOrder.getPartialFee(buy.matcherFee, buy.amount, amount)

    val tx = ExchangeTransactionV2
      .create(
        matcher = matcher,
        buyOrder = buy,
        sellOrder = sell,
        amount = amount,
        price = sellPrice,
        buyMatcherFee = buyMatcherFee,
        sellMatcherFee = sellMatcherFee,
        fee = matcherFee,
        timestamp = NTP.correctedTime()
      )
      .explicitGet()
      .json()

    tx
  }

  def orders(pair: AssetPair, version: Byte = 2, isSmart: Boolean = true): (Order, Order) = {
    val buyer               = acc1
    val seller              = acc0
    val matcher             = acc2
    val time                = NTP.correctedTime()
    val expirationTimestamp = time + Order.MaxLiveTime
    val buyPrice            = 1 * Order.PriceConstant
    val sellPrice           = (0.50 * Order.PriceConstant).toLong
    val mf                  = if (isSmart) 1500000L else 700000L
    val buyAmount           = 2
    val sellAmount          = 3

    val buy  = Order.buy(buyer, matcher, pair, buyAmount, buyPrice, time, expirationTimestamp, mf, version)
    val sell = Order.sell(seller, matcher, pair, sellAmount, sellPrice, time, expirationTimestamp, mf, version)

    (buy, sell)
  }

}
