package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.{DataTransaction, TxVersion}
import org.scalatest.CancelAfterFailure

class ExchangeWithContractsSuite extends BaseTransactionSuite with CancelAfterFailure with NTPTime {
  private def acc0 = firstKeyPair
  private def acc1 = secondKeyPair
  private def acc2 = thirdKeyPair

  var exchAsset: String    = ""
  var dtx: DataTransaction = _
  var pair: AssetPair      = _

  val sc1: Option[String] = Some(s"true")
  val sc2: Option[String] = Some(s"""
                                    |match tx {
                                    |  case _: SetScriptTransaction => true
                                    |  case _ => false
                                    |}""".stripMargin)
  val sc3: Option[String] = Some(s"""
                                    |match tx {
                                    |  case _: SetScriptTransaction => true
                                    |  case _ => throw("Some generic error")
                                    |}""".stripMargin)

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    exchAsset = sender
      .issue(
        acc0,
        "ExchangeCoin",
        "ExchangeCoin for tests with exchange transaction",
        someAssetAmount,
        8,
        reissuable = false,
        issueFee,
        TxVersion.V2,
        waitForTx = true
      )
      .id

    pair = AssetPair.createAssetPair(exchAsset, "WAVES").get

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", value = true)
    val entry3 = BinaryDataEntry("blob", ByteStr.decodeBase64("YWxpY2U=").get)
    val entry4 = StringDataEntry("str", "test")

    dtx = DataTransaction.selfSigned(1.toByte, acc0, List(entry1, entry2, entry3, entry4), minFee, ntpTime.correctedTime()).explicitGet()
    sender.signedBroadcast(dtx.json(), waitForTx = true)
  }

  test("set contracts and put exchange transaction in blockchain") {

    val sc4 = Some(cryptoContextScript(accountScript = true))
    val sc5 = Some(pureContextScript(dtx, accountScript = true))
    val sc6 = Some(wavesContextScript(dtx, accountScript = true))

    for (
      (contr1, contr2, mcontr) <- Seq(
        (sc1, sc1, sc1),
        (None, sc1, None),
        (None, None, sc1),
        (None, None, sc4),
        (None, None, sc5),
        (None, None, sc6),
        (sc5, None, sc5)
      )
    ) {

      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2)
      )
      for (
        (o1ver, o2ver) <- Seq(
          (2: Byte, 2: Byte),
          (2: Byte, 3: Byte)
        )
      ) {

        sender.signedBroadcast(exchangeTx(pair, smartMatcherFee, orderFee, ntpTime, o1ver, o2ver, acc1, acc0, acc2), waitForTx = true)

        // TODO : add assert balances
      }
    }

    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2)
    )
  }

  test("negative - set simple contracts and put exchange transaction in blockchain") {
    for (
      (contr1, contr2, mcontr) <- Seq(
        (sc1, sc2, sc1),
        (sc1, sc1, sc2),
        (None, None, sc2),
        (None, sc2, None)
      )
    ) {
      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2)
      )
      for (
        (o1ver, o2ver) <- Seq(
          (2: Byte, 2: Byte),
          (3: Byte, 3: Byte)
        )
      ) {
        assertBadRequestAndMessage(
          sender.signedBroadcast(exchangeTx(pair, smartMatcherFee, orderFee, ntpTime, o1ver, o2ver, acc1, acc0, acc2)),
          "Transaction is not allowed by account-script"
        )
        // TODO : add assert balances
      }
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2)
    )
  }

  test("negative - check custom exception") {
    for (
      (contr1, contr2, mcontr) <- Seq(
        (sc1, sc1, sc3)
      )
    ) {
      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2)
      )
      for (
        (o1ver, o2ver) <- Seq(
          (2: Byte, 2: Byte),
          (3: Byte, 3: Byte)
        )
      ) {
        val tx = exchangeTx(pair, smartMatcherFee, orderFee, ntpTime, o1ver, o2ver, acc1, acc0, acc2)
        assertBadRequestAndMessage(sender.signedBroadcast(tx), "Error while executing account-script: Some generic error")
        // TODO : add assert balances
      }
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2)
    )
  }

  test("positive - versioning verification") {
    for (
      (contr1, contr2, mcontr) <- Seq(
        (None, None, None),
        (sc1, None, None),
        (None, None, sc1)
      )
    ) {
      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2)
      )

      val matcher   = acc2
      val sellPrice = (0.50 * Order.PriceConstant).toLong
      for (
        (o1ver, o2ver) <- Seq(
          (1: Byte, 2: Byte),
          (1: Byte, 3: Byte)
        )
      ) {

        val (buy, sell) = orders(pair, o1ver, o2ver, orderFee, ntpTime, acc1, acc0, acc2)

        val amount = math.min(buy.amount.value, sell.amount.value)
        val tx = ExchangeTransaction
          .signed(
            3.toByte,
            matcher = matcher.privateKey,
            order1 = sell,
            order2 = buy,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(orderFee) * amount / buy.amount.value).toLong,
            sellMatcherFee = (BigInt(orderFee) * amount / sell.amount.value).toLong,
            fee = smartMatcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .explicitGet()
          .json()

        val txId = sender.signedBroadcast(tx).id
        nodes.waitForTransaction(txId)

        // TODO : add assert balances
      }
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2)
    )
  }

  test("negative - exchange tx v2 and order v1 from scripted acc") {
    setContracts((sc1, acc0))

    val matcher   = acc2
    val sellPrice = (0.50 * Order.PriceConstant).toLong

    for (
      (o1ver, o2ver) <- Seq(
        (2: Byte, 1: Byte),
        (3: Byte, 1: Byte)
      )
    ) {
      val (buy, sell) = orders(pair, o1ver, o2ver, orderFee, ntpTime, acc1, acc0, acc2)

      val amount = math.min(buy.amount.value, sell.amount.value)
      val tx = ExchangeTransaction
        .signed(
          2.toByte,
          matcher = matcher.privateKey,
          order1 = buy,
          order2 = sell,
          amount = amount,
          price = sellPrice,
          buyMatcherFee = (BigInt(orderFee) * amount / buy.amount.value).toLong,
          sellMatcherFee = (BigInt(orderFee) * amount / sell.amount.value).toLong,
          fee = smartMatcherFee,
          timestamp = ntpTime.correctedTime()
        )
        .explicitGet()
        .json()

      assertBadRequestAndMessage(sender.signedBroadcast(tx), "Reason: Can't process order with signature from scripted account")
    }
  }
}
