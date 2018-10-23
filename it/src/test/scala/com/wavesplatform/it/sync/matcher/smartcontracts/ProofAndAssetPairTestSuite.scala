package com.wavesplatform.it.sync.matcher.smartcontracts

import com.typesafe.config.Config
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV2}
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Proofs

import scala.concurrent.duration._

class ProofAndAssetPairTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()

  aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2)
  nodes.waitForHeightArise()

  val predefAssetPair    = wavesUsdPair
  val unallowedAssetPair = wctWavesPair

  val aliceAsset =
    aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
  nodes.waitForHeightAriseAndTxPresent(aliceAsset)
  val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

  "Proofs and AssetPairs verification with SmartContracts" - {
    val sc1 = s"""true"""
    val sc2 =
      s"""
         |match tx {
         | case s : SetScriptTransaction => true
         | case _ => false
         |}""".stripMargin
    val sc3 = s"""
                 |match tx {
                 | case t : Order =>
                 |   let id = t.id == base58''
                 |   let assetPair1Amount = isDefined(t.assetPair.amountAsset)
                 |   let assetPair1Price = if (isDefined(t.assetPair.priceAsset)) then extract(t.assetPair.priceAsset) == base58'${UsdId.toString}' else false
                 |   let assetPair2Amount = if (isDefined(t.assetPair.amountAsset)) then extract(t.assetPair.amountAsset) == base58'${aliceAsset}' else false
                 |   let assetPair2Price = isDefined(t.assetPair.priceAsset)
                 |   (!assetPair1Amount && assetPair1Price) || (assetPair2Amount && !assetPair2Price)
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sc4 = s"""
              |match tx {
              | case t : Order =>
              |    let id = t.id == base58''
              |    #let sender = t.sender == (base58'${ByteStr(aliceAcc.publicKey).base58}')
              |    let senderPublicKey = t.senderPublicKey == base58'${ByteStr(aliceAcc.publicKey).base58}'
              |    let matcherPublicKey = t.matcherPublicKey == base58'${ByteStr(matcherNode.publicKey.publicKey).base58}'
              |    let timestamp = t.timestamp > 0
              |    let price = t.price > 0
              |    let amount = t.amount > 0
              |    let expiration = t.expiration > 0
              |    let matcherFee = t.matcherFee > 0
              |    let bodyBytes = t.bodyBytes == base64''
              |    !id && senderPublicKey && matcherPublicKey && timestamp && price && amount && expiration && matcherFee &&
              |    expiration && matcherFee && !bodyBytes
              |  case s : SetScriptTransaction => true
              |  case _ => throw()
              | }
      """.stripMargin

    val sc5 = s"""
                 |match tx {
                 |  case t : Order => 
                 |        let pk1 = base58'${ByteStr(aliceAcc.publicKey)}'
                 |   sigVerify(t.bodyBytes,t.proofs[0],pk1)
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    val sc6 = s"""
                 |match tx {
                 |  case t : Order => 
                 |        let pk1 = base58'${ByteStr(aliceAcc.publicKey)}'
                 |        let pk2 = base58'${ByteStr(bobAcc.publicKey)}'
                 |        let pk3 = base58'${ByteStr(matcherNode.publicKey.publicKey)}'
                 |        
                 |        let alice = if (sigVerify(t.bodyBytes,t.proofs[0],pk1)) then 1 else 0
                 |        let bob = if (sigVerify(t.bodyBytes,t.proofs[1],pk2)) then 1 else 0
                 |        let matcher = if (sigVerify(t.bodyBytes,t.proofs[2],pk3)) then 1 else 0
                 |        alice + bob + matcher >=2
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    val sc7 = s"""
                 |match tx {
                 | case t : Order =>
                 |   let id = t.id == base58''
                 |   let assetPairAmount = if (isDefined(t.assetPair.amountAsset)) then extract(t.assetPair.amountAsset) == base58'${WctId.toString}' else false
                 |   let assetPairPrice = isDefined(t.assetPair.priceAsset)
                 |   (assetPairAmount && !assetPairPrice)
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sc8 = s"""
                 |match tx {
                 |  case t : Order => 
                 |        let pk1 = base58'${ByteStr(aliceNode.publicKey.publicKey)}'
                 |   sigVerify(t.bodyBytes,t.proofs[0],pk1)
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    val sc9 = s"""
                 |match tx {
                 |  case t : Order => height > 0
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    "positive scenarios of order placement" - {
      "set contracts with AssetPairs/all tx fields/true/one proof and then place order" in {
        for (i <- Seq(sc1, sc3, sc4, sc5)) {
          log.debug(s"contract: $i")
          setContract(i, aliceAcc)

          val aliceOrd1 = matcherNode
            .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = matcherNode
            .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          nodes.waitForHeightArise()

          matcherNode.cancelOrder(aliceAcc, predefAssetPair, Some(aliceOrd1)).status should be("OrderCanceled")
          matcherNode.cancelOrder(aliceAcc, aliceWavesPair, Some(aliceOrd2)).status should be("OrderCanceled")
        }

        setContract(null, aliceAcc)
      }

      "set contracts with many proofs and then place order" ignore {
        for (i <- Seq(sc5, sc6)) {
          setContract(i, aliceAcc)

          val currTime = System.currentTimeMillis()

          val unsigned =
            OrderV2(
              aliceAcc,
              matcherNode.publicKey,
              predefAssetPair,
              OrderType.BUY,
              500,
              2.waves * Order.PriceConstant,
              currTime,
              (30.days - 1.seconds).toMillis + currTime,
              300000,
              Proofs.empty
            )

          val sigAlice = ByteStr(crypto.sign(aliceAcc, unsigned.bodyBytes()))
          val sigBob   = ByteStr(crypto.sign(bobAcc, unsigned.bodyBytes()))

          val signed = unsigned.copy(proofs = Proofs(Seq(sigAlice, sigBob)))

          val ord1 = matcherNode
            .placeOrder(signed)
            .message
            .id
          matcherNode.waitOrderStatus(predefAssetPair, ord1, "Accepted", 1.minute)

          nodes.waitForHeightArise()

          matcherNode.cancelOrder(aliceAcc, predefAssetPair, Some(ord1)).status should be("OrderCanceled")
        }

        setContract(null, aliceAcc)
      }

      "place order and then set contract on AssetPairs/true/all fields/one proof" in {
        for (i <- Seq(sc1, sc3, sc4, sc5)) {
          log.debug(s"contract: $i")
          val aliceOrd1 = matcherNode
            .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = matcherNode
            .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          setContract(i, aliceAcc)

          val bobOrd1 = matcherNode
            .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id
          val bobOrd2 = matcherNode
            .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id

          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val exchangeTx1 = matcherNode.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
          nodes.waitForHeightAriseAndTxPresent(exchangeTx1.id)

          val exchangeTx2 = matcherNode.transactionsByOrder(bobOrd2).headOption.getOrElse(fail("Expected an exchange transaction"))
          nodes.waitForHeightAriseAndTxPresent(exchangeTx2.id)

          matcherNode.reservedBalance(bobAcc) shouldBe empty
        }

        setContract(null, aliceAcc)
      }

      "place order and then set contract with many proofs" ignore {
        for (i <- Seq(sc5, sc6)) {
          for (assetP <- Seq(predefAssetPair, aliceWavesPair)) {
            val currTime = System.currentTimeMillis()

            val unsigned =
              OrderV2(
                aliceAcc,
                matcherNode.publicKey,
                assetP,
                OrderType.BUY,
                500,
                2.waves * Order.PriceConstant,
                currTime,
                (30.days - 1.seconds).toMillis + currTime,
                300000,
                Proofs.empty
              )

            val sigAlice = ByteStr(crypto.sign(aliceAcc, unsigned.bodyBytes()))
            val sigMat   = ByteStr(crypto.sign(matcherNode.privateKey, unsigned.bodyBytes()))

            val signed = unsigned.copy(proofs = Proofs(Seq(sigAlice, ByteStr.empty, sigMat)))

            val ord = matcherNode
              .placeOrder(signed)
              .message
              .id
            matcherNode.waitOrderStatus(assetP, ord, "Accepted", 1.minute)
          }

          setContract(i, aliceAcc)

          val bobOrd1 = matcherNode
            .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id
          val bobOrd2 = matcherNode
            .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
            .message
            .id

          matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val exchangeTx1 = matcherNode.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
          nodes.waitForHeightAriseAndTxPresent(exchangeTx1.id)

          val exchangeTx2 = matcherNode.transactionsByOrder(bobOrd2).headOption.getOrElse(fail("Expected an exchange transaction"))
          nodes.waitForHeightAriseAndTxPresent(exchangeTx2.id)

          matcherNode.reservedBalance(bobAcc) shouldBe empty
        }
        setContract(null, aliceAcc)
      }
    }

    "negative scenarios of order placement" - {
      "set contact and then place order" in {
        for (i <- Seq(sc2, sc7, sc8, sc9)) {
          log.debug(s"contract: $i")
          setContract(i, aliceAcc)

          assertBadRequest(
            matcherNode
              .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
          )

          assertBadRequest(
            matcherNode
              .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1, 10.minutes)
          )
        }
        setContract(null, aliceAcc)
      }

      "place order and then set contract" in {
        for (i <- Seq(sc2, sc7, sc8 /*, sc9 */ )) {
          log.debug(s"contract: $i")
//          val bobBalance     = bobNode.accountBalances(bobAcc.address)._1
//          val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
          val aliceBalance = aliceNode.accountBalances(aliceAcc.address)._1

          val aliceOrd1 = matcherNode
            .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 100, 2.waves * Order.PriceConstant, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = matcherNode
            .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          setContract(i, aliceAcc)

          val bobOrd1 = matcherNode
            .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 100, 2.waves * Order.PriceConstant, version = 2, 10.minutes)
            .message
            .id

          val bobOrd2 = matcherNode
            .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, version = 2, 10.minutes)
            .message
            .id

          matcherNode.transactionsByOrder(aliceOrd1)
          matcherNode.transactionsByOrder(aliceOrd2)

          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Cancelled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Cancelled", 1.minute)
          matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Accepted", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, bobOrd2, "Accepted", 1.minute)

          // TODO:  check Bob's balance
          matcherNode.ordersByAddress(aliceAcc, activeOnly = true).length shouldBe 0

          // Alice checks that she received some Waves
          val updatedAliceBalance = aliceNode.accountBalances(aliceAcc.address)._1
          updatedAliceBalance shouldBe (aliceBalance - 0.014.waves)

          matcherNode.cancelOrder(bobAcc, predefAssetPair, Some(bobOrd1)).status should be("OrderCanceled")
          matcherNode.cancelOrder(bobAcc, aliceWavesPair, Some(bobOrd2)).status should be("OrderCanceled")

          setContract(null, aliceAcc)
        }

      }
    }
  }
}
