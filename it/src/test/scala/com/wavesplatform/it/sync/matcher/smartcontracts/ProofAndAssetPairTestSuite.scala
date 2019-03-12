package com.wavesplatform.it.sync.matcher.smartcontracts

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV2}
import play.api.libs.json.Json

import scala.concurrent.duration._

class ProofAndAssetPairTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  private val aliceAsset =
    aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2).id

  {
    val issueTx = matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
    nodes.waitForTransaction(issueTx.id)

    val transferTx = aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2)
    nodes.waitForTransaction(transferTx.id)
  }

  private val predefAssetPair = wavesUsdPair
  private val aliceWavesPair  = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Waves)

  "Proofs and AssetPairs verification with SmartContracts" - {
    val sc3 = s"""
                 |match tx {
                 | case t : Order =>
                 |   let id = t.id == base58''
                 |   let assetPair1Amount = isDefined(t.assetPair.amountAsset)
                 |   let assetPair1Price = if (isDefined(t.assetPair.priceAsset)) then extract(t.assetPair.priceAsset) == base58'${UsdId.toString}' else false
                 |   let assetPair2Amount = if (isDefined(t.assetPair.amountAsset)) then extract(t.assetPair.amountAsset) == base58'$aliceAsset' else false
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
                 |  case t : Order => height < 0
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    "positive scenarios of order placement" - {
      "set contracts with AssetPairs/all tx fields/true/one proof and then place order" - {
        for ((sc, i) <- Seq(sc1, sc3, sc4, sc5).zip(Seq(1, 3, 4, 5))) s"$i" in {
          log.debug(s"contract: $sc")
          setContract(Some(sc), aliceAcc)

          val aliceOrd1 = matcherNode
            .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = matcherNode
            .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          matcherNode.cancelOrder(aliceAcc, predefAssetPair, aliceOrd1)
          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Cancelled")

          matcherNode.cancelOrder(aliceAcc, aliceWavesPair, aliceOrd2).status should be("OrderCanceled")
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Cancelled")
        }
      }

      "set contracts with many proofs and then place order" - {
        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) s"$i" in {
          setContract(Some(sc), aliceAcc)

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
              smartMatcherFee,
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

          matcherNode.cancelOrder(aliceAcc, predefAssetPair, ord1)
          matcherNode.waitOrderStatus(predefAssetPair, ord1, "Cancelled")
        }

        "reset" in setContract(None, aliceAcc)
      }

      "place order and then set contract on AssetPairs/true/all fields/one proof" - {
        for ((sc, i) <- Seq(sc1, sc3, sc4, sc5).zip(Seq(1, 3, 4, 5))) s"$i" in {
          log.debug(s"contract: $sc")
          val aliceOrd1 = matcherNode
            .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = matcherNode
            .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          setContract(Some(sc), aliceAcc)

          val bobOrd1 = matcherNode
            .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          val bobOrd2 = matcherNode
            .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Filled", 1.minute)
          matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val exchangeTx1 = matcherNode.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
          matcherNode.waitForTransaction(exchangeTx1.id)
          assert(exchangeTx1.fee == 300000)

          val exchangeTx2 = matcherNode.transactionsByOrder(bobOrd2).headOption.getOrElse(fail("Expected an exchange transaction"))
          matcherNode.waitForTransaction(exchangeTx2.id)
          assert(exchangeTx2.fee == 300000)

          matcherNode.reservedBalance(bobAcc) shouldBe empty
        }
      }

      "place order and then set contract with many proofs" in {
        setContract(Some("true"), aliceAcc)

        val transferTx = aliceNode.transfer(aliceAcc.address, bobAcc.address, 1000, 0.005.waves, Some(aliceAsset), None, 2)
        nodes.waitForHeightAriseAndTxPresent(transferTx.id)

        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) {
          markup(s"$i")

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
                smartMatcherFee,
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

          setContract(Some(sc), aliceAcc)

          val bobOrd1 = matcherNode
            .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          val bobOrd2 = matcherNode
            .placeOrder(bobAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val exchangeTx1 = matcherNode.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
          matcherNode.waitForTransaction(exchangeTx1.id)
          assert(exchangeTx1.fee == 300000)

          val exchangeTx2 = matcherNode.transactionsByOrder(bobOrd2).headOption.getOrElse(fail("Expected an exchange transaction"))
          matcherNode.waitForTransaction(exchangeTx2.id)
          assert(exchangeTx2.fee == 300000)

          matcherNode.reservedBalance(bobAcc) shouldBe empty
        }

        setContract(None, aliceAcc)
      }
    }

    "negative scenarios of order placement" - {
      "set contact and then place order" - {
        for ((sc, i) <- Seq(sc2, sc7, sc8).zip(Seq(2, 7, 8))) s"$i" in {
          log.debug(s"contract: $sc")
          setContract(Some(sc), aliceAcc)

          assertBadRequestAndResponse(
            matcherNode
              .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes),
            "The account's script of .* rejected the order"
          )

          assertBadRequestAndResponse(
            matcherNode
              .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes),
            "The account's script of .* rejected the order"
          )
        }

        "9" in {
          setContract(Some(sc9), aliceAcc)
          assertBadRequestAndResponse(
            matcherNode
              .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes),
            "The account's script of .* returned the error"
          )
        }

        "reset" in setContract(None, aliceAcc)
      }

      "place order and then set contract" - {
        for ((contract, i) <- Seq(sc2, sc7, sc8, sc9).zip(Seq(2, 7, 8, 9))) s"$i" in {
          log.debug(s"contract $contract")

          val aliceOrd1 = matcherNode
            .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = matcherNode
            .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          setContract(Some(contract), aliceAcc)

          val bobOrd1 = matcherNode
            .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          val bobOrd2 = matcherNode
            .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Filled", 1.minute)
          matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          matcherNode.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val aliceOrd1Txs = matcherNode.transactionsByOrder(aliceOrd1)
          aliceOrd1Txs.size shouldBe 1
          matcherNode.expectSignedBroadcastRejected(Json.toJson(aliceOrd1Txs.head))

          val aliceOrd2Txs = matcherNode.transactionsByOrder(aliceOrd2)
          aliceOrd2Txs.size shouldBe 1
          matcherNode.expectSignedBroadcastRejected(Json.toJson(aliceOrd2Txs.head))

          matcherNode.ordersByAddress(aliceAcc, activeOnly = true).length shouldBe 0

          matcherNode.reservedBalance(bobAcc) shouldBe empty

          setContract(None, aliceAcc)
        }
      }
    }
  }
}
