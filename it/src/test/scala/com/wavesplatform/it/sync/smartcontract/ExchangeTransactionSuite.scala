package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state._
import com.wavesplatform.transaction.DataTransaction
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.NTP
import org.scalatest.CancelAfterFailure
import play.api.libs.json._
import scorex.crypto.encode.Base64

class ExchangeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  var exchAsset: String    = ""
  var dtx: DataTransaction = null

  val sc1 = Some(s"""true""")
  val sc2 = Some(s"""
               |match tx {
               |  case s : SetScriptTransaction => true
               |  case _ => false
               |}""".stripMargin)
  val sc3 = Some(s"""
               |match tx {
               |  case s : SetScriptTransaction => true
               |  case _ => throw("Some generic error")
               |}""".stripMargin)

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    exchAsset = sender
      .issue(acc0.address, "ExchangeCoin", "ExchangeCoin for tests with exchange transaction", someAssetAmount, 0, reissuable = false, issueFee, 2)
      .id
    nodes.waitForHeightAriseAndTxPresent(exchAsset)

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val entry4 = StringDataEntry("str", "test")

    dtx = DataTransaction.selfSigned(1, acc0, List(entry1, entry2, entry3, entry4), 0.001.waves, NTP.correctedTime()).explicitGet()
    val dtxId = sender.signedBroadcast(dtx.json()).id
    nodes.waitForHeightAriseAndTxPresent(dtxId)
  }

  private def setContracts(contracts: (Option[String], PrivateKeyAccount)*): Unit = {
    contracts
      .map {
        case (src, acc) => setContract(src, acc)
      }
      .foreach(id => sender.waitForTransaction(id))
    nodes.waitForHeightArise()
  }

  test("set contracts and put exchange transaction in blockchain") {
    val sc4 = cryptoContext(dtx)
    val sc5 = pureContext(dtx)
    val sc6 = wavesContext(dtx)

    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc1, sc1),
           (None, sc1, None),
           (None, None, sc1),
           (None, None, sc4),
           (None, None, sc5),
           (None, None, sc6),
         )) {

      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2),
      )

      val tx = exchangeTx()

      val txId = sender.signedBroadcast(tx).id
      nodes.waitForHeightAriseAndTxPresent(txId)
      //TODO : add assert balances
    }

    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }

  test("negative: set simple contracts and put exchange transaction in blockchain") {
    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc2, sc1),
           (sc1, sc1, sc2),
           (None, None, sc2),
           (None, sc2, None)
         )) {
      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2),
      )

      val tx = exchangeTx()
      assertBadRequestAndMessage(sender.signedBroadcast(tx), "Transaction is not allowed by account-script")
      //TODO : add assert balances
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }

  test("negative: check custom exception") {
    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc1, sc3)
         )) {
      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2),
      )

      val tx = exchangeTx()
      assertBadRequestAndMessage(sender.signedBroadcast(tx), "Error while executing account-script: Some generic error")
      //TODO : add assert balances
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }

  test("positive: versioning verification") {
    for ((contr1, contr2, mcontr) <- Seq(
           (None, None, None),
           (sc1, None, None),
           (None, None, sc1)
         )) {
      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2),
      )

      val mf        = 700000L
      val matcher   = acc2
      val sellPrice = (0.50 * Order.PriceConstant).toLong
      val buy       = orders(1)._1
      val sell      = orders(2)._2

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

      val txId = sender.signedBroadcast(tx).id
      nodes.waitForHeightAriseAndTxPresent(txId)

      //TODO : add assert balances
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }

  test("negative: check orders v2 with exchange tx v1") {
    val tx        = exchangeTx()
    val sig       = (Json.parse(tx.toString()) \ "proofs").as[Seq[JsString]].head
    val changedTx = tx + ("version" -> JsNumber(1)) + ("signature" -> sig)
    assertBadRequestAndMessage(sender.signedBroadcast(changedTx).id, "can only contain orders of version 1", 400)
  }

  test("negative: exchange tx v2 and order v1 from scripted acc") {
    setContracts((sc1, acc0))

    val mf        = 700000L
    val matcher   = acc2
    val sellPrice = (0.50 * Order.PriceConstant).toLong
    val buy       = orders(2)._1
    val sell      = orders(1)._2

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

    assertBadRequestAndMessage(sender.signedBroadcast(tx).id, "Reason: Can't process order with signature from scripted account")
  }

  private def exchangeTx(isSmart: Boolean = true) = {
    val matcher   = acc2
    val sellPrice = (0.50 * Order.PriceConstant).toLong
    val buy       = orders(2, isSmart)._1
    val sell      = orders(2, isSmart)._2

    val amount = math.min(buy.amount, sell.amount)

    val matcherFee     = if (isSmart) 1500000 else 700000L
    val sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong
    val buyMatcherFee  = matcherFee - sellMatcherFee

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

  private def orders(version: Byte, isSmart: Boolean = true) = {
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
    val assetPair           = AssetPair.createAssetPair(exchAsset, "WAVES").get
    val buy                 = Order.buy(buyer, matcher, assetPair, buyAmount, buyPrice, time, expirationTimestamp, mf, version)
    val sell                = Order.sell(seller, matcher, assetPair, sellAmount, sellPrice, time, expirationTimestamp, mf, version)

    (buy, sell)
  }

}
