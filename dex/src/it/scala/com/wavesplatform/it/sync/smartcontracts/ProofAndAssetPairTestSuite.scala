package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV2}
import play.api.libs.json.Json

import scala.concurrent.duration._

class ProofAndAssetPairTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  private val aliceAsset =
    node.broadcastIssue(alice, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, None).id

  {
    val issueTx = node.signedIssue(createSignedIssueRequest(IssueUsdTx))
    nodes.waitForTransaction(issueTx.id)
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
              |    #let sender = t.sender == (base58'${ByteStr(alice.publicKey)}')
              |    let senderPublicKey = t.senderPublicKey == base58'${ByteStr(alice.publicKey)}'
              |    let matcherPublicKey = t.matcherPublicKey == base58'${ByteStr(matcher.publicKey)}'
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
                 |        let pk1 = base58'${ByteStr(alice.publicKey)}'
                 |   sigVerify(t.bodyBytes,t.proofs[0],pk1)
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    val sc6 = s"""
                 |match tx {
                 |  case t : Order => 
                 |        let pk1 = base58'${ByteStr(alice.publicKey)}'
                 |        let pk2 = base58'${ByteStr(bob.publicKey)}'
                 |        let pk3 = base58'${ByteStr(matcher.publicKey)}'
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
                 |        let pk1 = base58'${ByteStr(matcher.publicKey)}'
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
          setContract(Some(sc), alice)

          val aliceOrd1 = node
            .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = node
            .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          node.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          node.cancelOrder(alice, predefAssetPair, aliceOrd1)
          node.waitOrderStatus(predefAssetPair, aliceOrd1, "Cancelled")

          node.cancelOrder(alice, aliceWavesPair, aliceOrd2).status should be("OrderCanceled")
          node.waitOrderStatus(aliceWavesPair, aliceOrd2, "Cancelled")
        }
      }

      "set contracts with many proofs and then place order" - {
        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) s"$i" in {
          setContract(Some(sc), alice)

          val currTime = System.currentTimeMillis()

          val unsigned =
            OrderV2(
              alice,
              node.publicKey,
              predefAssetPair,
              OrderType.BUY,
              500,
              2.waves * Order.PriceConstant,
              currTime,
              (30.days - 1.seconds).toMillis + currTime,
              smartMatcherFee,
              Proofs.empty
            )

          val sigAlice = ByteStr(crypto.sign(alice, unsigned.bodyBytes()))
          val sigBob   = ByteStr(crypto.sign(bob, unsigned.bodyBytes()))

          val signed = unsigned.copy(proofs = Proofs(Seq(sigAlice, sigBob)))

          val ord1 = node
            .placeOrder(signed)
            .message
            .id
          node.waitOrderStatus(predefAssetPair, ord1, "Accepted", 1.minute)

          node.cancelOrder(alice, predefAssetPair, ord1)
          node.waitOrderStatus(predefAssetPair, ord1, "Cancelled")
        }

        "reset" in setContract(None, alice)
      }

      "place order and then set contract on AssetPairs/true/all fields/one proof" - {
        for ((sc, i) <- Seq(sc1, sc3, sc4, sc5).zip(Seq(1, 3, 4, 5))) s"$i" in {
          log.debug(s"contract: $sc")
          val aliceOrd1 = node
            .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = node
            .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          node.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          setContract(Some(sc), alice)

          val bobOrd1 = node
            .placeOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          val bobOrd2 = node
            .placeOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, fee = smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          node.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
          node.waitOrderStatus(aliceWavesPair, aliceOrd2, "Filled", 1.minute)
          node.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          node.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val exchangeTx1 = node.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
          node.waitForTransaction(exchangeTx1.id)
          assert(exchangeTx1.fee == 300000)

          val exchangeTx2 = node.transactionsByOrder(bobOrd2).headOption.getOrElse(fail("Expected an exchange transaction"))
          node.waitForTransaction(exchangeTx2.id)
          assert(exchangeTx2.fee == 300000)

          node.reservedBalance(bob) shouldBe empty
        }
      }

      "place order and then set contract with many proofs" in {
        setContract(Some("true"), alice)

        val transferTx = node.broadcastTransfer(alice, bob.address, 1000, 0.005.waves, Some(aliceAsset), None)
        nodes.waitForHeightAriseAndTxPresent(transferTx.id)

        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) {
          markup(s"$i")

          for (assetP <- Seq(predefAssetPair, aliceWavesPair)) {
            val currTime = System.currentTimeMillis()

            val unsigned =
              OrderV2(
                alice,
                matcher,
                assetP,
                OrderType.BUY,
                500,
                2.waves * Order.PriceConstant,
                currTime,
                (30.days - 1.seconds).toMillis + currTime,
                smartMatcherFee,
                Proofs.empty
              )

            val sigAlice = ByteStr(crypto.sign(alice, unsigned.bodyBytes()))
            val sigMat   = ByteStr(crypto.sign(matcher.privateKey, unsigned.bodyBytes()))

            val signed = unsigned.copy(proofs = Proofs(Seq(sigAlice, ByteStr.empty, sigMat)))

            val ord = node
              .placeOrder(signed)
              .message
              .id
            node.waitOrderStatus(assetP, ord, "Accepted", 1.minute)
          }

          setContract(Some(sc), alice)

          val bobOrd1 = node
            .placeOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          val bobOrd2 = node
            .placeOrder(bob, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          node.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          node.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val exchangeTx1 = node.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
          node.waitForTransaction(exchangeTx1.id)
          assert(exchangeTx1.fee == 300000)

          val exchangeTx2 = node.transactionsByOrder(bobOrd2).headOption.getOrElse(fail("Expected an exchange transaction"))
          node.waitForTransaction(exchangeTx2.id)
          assert(exchangeTx2.fee == 300000)

          node.reservedBalance(bob) shouldBe empty
        }

        setContract(None, alice)
      }
    }

    "negative scenarios of order placement" - {
      "set contact and then place order" - {
        for ((sc, i) <- Seq(sc2, sc7, sc8).zip(Seq(2, 7, 8))) s"$i" in {
          log.debug(s"contract: $sc")
          setContract(Some(sc), alice)

          assertBadRequestAndResponse(
            node
              .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes),
            "The account's script of .* rejected the order"
          )

          assertBadRequestAndResponse(
            node
              .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes),
            "The account's script of .* rejected the order"
          )
        }

        "9" in {
          setContract(Some(sc9), alice)
          assertBadRequestAndResponse(
            node
              .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes),
            "The account's script of .* returned the error"
          )
        }

        "reset" in setContract(None, alice)
      }

      "place order and then set contract" - {
        for ((contract, i) <- Seq(sc2, sc7, sc8, sc9).zip(Seq(2, 7, 8, 9))) s"$i" in {
          log.debug(s"contract $contract")

          val aliceOrd1 = node
            .placeOrder(alice, predefAssetPair, OrderType.BUY, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

          val aliceOrd2 = node
            .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id
          node.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

          setContract(Some(contract), alice)

          val bobOrd1 = node
            .placeOrder(bob, predefAssetPair, OrderType.SELL, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          val bobOrd2 = node
            .placeOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id

          node.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
          node.waitOrderStatus(aliceWavesPair, aliceOrd2, "Filled", 1.minute)
          node.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
          node.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

          val aliceOrd1Txs = node.transactionsByOrder(aliceOrd1)
          aliceOrd1Txs.size shouldBe 1
          node.expectSignedBroadcastRejected(Json.toJson(aliceOrd1Txs.head))

          val aliceOrd2Txs = node.transactionsByOrder(aliceOrd2)
          aliceOrd2Txs.size shouldBe 1
          node.expectSignedBroadcastRejected(Json.toJson(aliceOrd2Txs.head))

          node.ordersByAddress(alice, activeOnly = true).length shouldBe 0

          node.reservedBalance(bob) shouldBe empty

          setContract(None, alice)
        }
      }
    }
  }
}
