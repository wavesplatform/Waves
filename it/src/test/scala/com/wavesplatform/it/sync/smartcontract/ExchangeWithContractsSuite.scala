package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
import com.wavesplatform.transaction.{Asset, DataTransaction}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure
import scorex.crypto.encode.Base64

class ExchangeWithContractsSuite extends BaseTransactionSuite with CancelAfterFailure with NTPTime {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  var exchAsset: String    = ""
  var dtx: DataTransaction = _
  var pair: AssetPair      = _

  val sc1 = Some(s"true")
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
      .issue(acc0.address,
             "ExchangeCoin",
             "ExchangeCoin for tests with exchange transaction",
             someAssetAmount,
             0,
             reissuable = false,
             issueFee,
             2,
             waitForTx = true)
      .id

    pair = AssetPair.createAssetPair(exchAsset, "WAVES").get

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", value = true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val entry4 = StringDataEntry("str", "test")

    dtx = DataTransaction.selfSigned(acc0, List(entry1, entry2, entry3, entry4), minFee, ntpTime.correctedTime()).explicitGet()
    sender.signedBroadcast(dtx.json(), waitForTx = true)
  }
  /*
  test("set contracts and put exchange transaction in blockchain") {
    val sc4 = Some(cryptoContextScript(true))
    val sc5 = Some(pureContextScript(dtx, true))
    val sc6 = Some(wavesContextScript(dtx, true))

    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc1, sc1),
           (None, sc1, None),
           (None, None, sc1),
           (None, None, sc4),
           (None, None, sc5),
           (None, None, sc6),
           (sc5, None, sc5),
         )) {

      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2),
      )
      for ((o1ver, o2ver) <- Seq(
             (2: Byte, 2: Byte),
             (2: Byte, 3: Byte),
           )) {

        sender.signedBroadcast(exchangeTx(pair, smartMatcherFee, orderFee, ntpTime, o1ver, o2ver, acc1, acc0, acc2), waitForTx = true)

        //TODO : add assert balances
      }
    }

    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }

  test("negative - set simple contracts and put exchange transaction in blockchain") {
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
      for ((o1ver, o2ver) <- Seq(
             (2: Byte, 2: Byte),
             (3: Byte, 3: Byte),
           )) {
        assertBadRequestAndMessage(sender.signedBroadcast(exchangeTx(pair, smartMatcherFee, orderFee, ntpTime, o1ver, o2ver, acc1, acc0, acc2)),
                                   "Transaction is not allowed by account-script")
        //TODO : add assert balances
      }
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }

  test("negative - check custom exception") {
    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc1, sc3)
         )) {
      setContracts(
        (contr1, acc0),
        (contr2, acc1),
        (mcontr, acc2),
      )
      for ((o1ver, o2ver) <- Seq(
             (2: Byte, 2: Byte),
             (3: Byte, 3: Byte),
           )) {
        val tx = exchangeTx(pair, smartMatcherFee, orderFee, ntpTime, o1ver, o2ver, acc1, acc0, acc2)
        assertBadRequestAndMessage(sender.signedBroadcast(tx), "Error while executing account-script: Some generic error")
        //TODO : add assert balances
      }
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }

  test("positive - versioning verification") {
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

      val matcher   = acc2
      val sellPrice = (0.50 * Order.PriceConstant).toLong
      for ((o1ver, o2ver) <- Seq(
             (1: Byte, 2: Byte),
             (1: Byte, 3: Byte),
           )) {

        val (buy, sell) = orders(pair, o1ver, o2ver, orderFee, ntpTime, acc1, acc0, acc2)

        val amount = math.min(buy.amount, sell.amount)
        val tx = ExchangeTransactionV2
          .create(
            matcher = matcher,
            buyOrder = buy,
            sellOrder = sell,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(orderFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(orderFee) * amount / sell.amount).toLong,
            fee = smartMatcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .explicitGet()
          .json()

        val txId = sender.signedBroadcast(tx).id
        nodes.waitForHeightAriseAndTxPresent(txId)

        //TODO : add assert balances
      }
    }
    setContracts(
      (None, acc0),
      (None, acc1),
      (None, acc2),
    )
  }
   */
  test("positive - exchange tx with asset/smartasset matcherFee") {
    val buyer   = acc0
    val seller  = acc1
    val matcher = acc2
    sender.transfer(buyer.address, seller.address, someAssetAmount / 2, minFee + smartFee, Some(exchAsset), waitForTx = true)

    for ((contr1, contr2, mcontr) <- Seq(
           (sc1, sc1, sc1)
         )) {
      setContracts(
        (contr1, buyer),
        (contr2, seller),
        (mcontr, matcher),
      )

      val assetDescription = "my asset description"

      val IssueTx: IssueTransactionV2 = IssueTransactionV2
        .selfSigned(
          AddressScheme.current.chainId,
          sender = buyer,
          name = "myasset".getBytes(),
          description = assetDescription.getBytes(),
          quantity = someAssetAmount,
          decimals = 8,
          reissuable = true,
          fee = issueFee + smartFee,
          script = None,
          timestamp = System.currentTimeMillis()
        )
        .right
        .get

      val scriptV2 = ScriptCompiler.compile("""
                                              |func isTrue() = true
                                              |isTrue()
                                            """.stripMargin).explicitGet()._1

      val IssueTxV2: IssueTransactionV2 = IssueTransactionV2
        .selfSigned(
          AddressScheme.current.chainId,
          sender = buyer,
          name = "scriptedasset".getBytes,
          description = assetDescription.getBytes,
          quantity = someAssetAmount,
          decimals = 8,
          reissuable = true,
          script = Some(scriptV2),
          fee = issueFee + smartFee,
          timestamp = System.currentTimeMillis()
        )
        .right
        .get

      val assetId      = IssueTx.id()
      val smartAssetId = IssueTxV2.id()

      sender.postJson("/transactions/broadcast", IssueTx.json())
      sender.postJson("/transactions/broadcast", IssueTxV2.json())

      nodes.waitForHeightAriseAndTxPresent(assetId.base58)
      nodes.waitForHeightAriseAndTxPresent(smartAssetId.base58)

      val sellPrice = (0.50 * Order.PriceConstant).toLong
      for ((o1ver, o2ver, matcherFeeOrder1, matcherFeeOrder2) <- Seq(
             (2: Byte, 3: Byte, Waves, IssuedAsset(assetId)),
             (2: Byte, 3: Byte, Waves, Waves),
             (3: Byte, 2: Byte, IssuedAsset(assetId), Waves),
             (3: Byte, 3: Byte, Waves, IssuedAsset(smartAssetId)),
             (3: Byte, 2: Byte, IssuedAsset(smartAssetId), Waves)
           )) {

        var assetBalanceBefore: Long = 0l

        if (matcherFeeOrder1 == Waves && matcherFeeOrder2 == IssuedAsset(assetId)) {
          assetBalanceBefore = sender.assetBalance(seller.address, assetId.base58).balance
          sender.transfer(buyer.address, seller.address, 300000, minFee + smartFee, Some(assetId.base58), waitForTx = true)
        } else if (matcherFeeOrder1 == Waves && matcherFeeOrder2 == IssuedAsset(smartAssetId)) {
          assetBalanceBefore = sender.assetBalance(seller.address, smartAssetId.base58).balance
          sender.transfer(buyer.address, seller.address, 300000, minFee + 2 * smartFee, Some(smartAssetId.base58), waitForTx = true)
        }

        val (buy, sell) = orders(pair, o1ver, o2ver, matcherFee, ntpTime, matcherFeeOrder1, matcherFeeOrder2, buyer, seller, matcher)

        val amount         = math.min(buy.amount, sell.amount)
        val buyMatcherFee  = (BigInt(orderFee) * amount / buy.amount).toLong
        val sellMatcherFee = (BigInt(orderFee) * amount / sell.amount).toLong
        val tx = ExchangeTransactionV2
          .create(
            matcher = matcher,
            buyOrder = buy,
            sellOrder = sell,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = buyMatcherFee,
            sellMatcherFee = sellMatcherFee,
            fee = smartMatcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .explicitGet()
          .json()

        val txId = sender.signedBroadcast(tx).id
        nodes.waitForHeightAriseAndTxPresent(txId)

        if (matcherFeeOrder1 == Waves && matcherFeeOrder2 == IssuedAsset(assetId)) {
          sender.assetBalance(seller.address, assetId.base58).balance shouldBe buyMatcherFee - sellMatcherFee
        } else if (matcherFeeOrder1 == Waves && matcherFeeOrder2 == IssuedAsset(smartAssetId)) {
          sender.assetBalance(seller.address, smartAssetId.base58).balance shouldBe buyMatcherFee - sellMatcherFee
        }
      }
    }
    setContracts(
      (None, buyer),
      (None, seller),
      (None, matcher),
    )
  }

  test("negative - exchange tx v2 and order v1 from scripted acc") {
    setContracts((sc1, acc0))

    val matcher   = acc2
    val sellPrice = (0.50 * Order.PriceConstant).toLong

    for ((o1ver, o2ver) <- Seq(
           (2: Byte, 1: Byte),
           (3: Byte, 1: Byte),
         )) {
      val (buy, sell) = orders(pair, o1ver, o2ver, orderFee, ntpTime, acc1, acc0, acc2)

      val amount = math.min(buy.amount, sell.amount)
      val tx = ExchangeTransactionV2
        .create(
          matcher = matcher,
          buyOrder = buy,
          sellOrder = sell,
          amount = amount,
          price = sellPrice,
          buyMatcherFee = (BigInt(orderFee) * amount / buy.amount).toLong,
          sellMatcherFee = (BigInt(orderFee) * amount / sell.amount).toLong,
          fee = smartMatcherFee,
          timestamp = ntpTime.correctedTime()
        )
        .explicitGet()
        .json()

      assertBadRequestAndMessage(sender.signedBroadcast(tx), "Reason: Can't process order with signature from scripted account")
    }
  }
}
