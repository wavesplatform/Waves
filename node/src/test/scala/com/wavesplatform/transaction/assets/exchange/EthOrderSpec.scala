package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.common.utils.*
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.transaction.{TxExchangeAmount, TxHelpers, TxMatcherFee, TxOrderPrice, TxVersion}
import com.wavesplatform.utils.{DiffMatchers, EthEncoding, EthHelpers, JsonMatchers}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll
import play.api.libs.json.{JsObject, Json}

class EthOrderSpec
    extends FlatSpec
    with BeforeAndAfterAll
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with DiffMatchers
    with JsonMatchers {
  import EthOrderSpec.{ethBuyOrder, ethSellOrder}

  "ETH signed order" should "recover signer public key correctly" in {
    val testOrder = Order(
      Order.V4,
      EthSignature(
        "0xfe56e1cbd6945f1e17ce9f9eb21172dd7810bcc74651dd7d3eaeca5d9ae0409113e5236075841af8195cb4dba3947ae9b99dbd560fd0c43afe89cc0b648690321c"
      ),
      PublicKey(EthStubBytes32),
      AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), IssuedAsset(ByteStr(EthStubBytes32))),
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(1),
      TxOrderPrice.unsafeFrom(1),
      123,
      321,
      TxMatcherFee.unsafeFrom(1),
      IssuedAsset(ByteStr(EthStubBytes32))
    )

    val result = EthOrders.recoverEthSignerKey(testOrder, testOrder.eip712Signature.get.arr)
    result shouldBe TestEthOrdersPublicKey
    result.toAddress shouldBe TestEthOrdersPublicKey.toAddress
  }

  it should "recover public key at json parse stage" in {
    import com.wavesplatform.transaction.assets.exchange.OrderJson.orderReads

    val json  = Json.toJson(ethBuyOrder).as[JsObject] - "senderPublicKey"
    val order = Json.fromJson[Order](json).get
    order.senderPublicKey shouldBe ethBuyOrder.senderPublicKey

    intercept[IllegalArgumentException](Json.fromJson[Order](json - "eip712Signature")).getMessage should include(
      "Either senderPublicKey or eip712Signature should be provided"
    )
  }

  it should "be of version 4" in {
    val testOrder = Order(
      Order.V1,
      EthSignature(
        "0xb557dae4c614146dd35ba6fd80e4702a75d33ffcb8af09e80e0c1a7386b8ffcb5b76bd8037f6484de809a80a5b39a224301c76e8bad9b1a9e7ada53ba6fa7e361c"
      ),
      PublicKey(EthStubBytes32),
      AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(1),
      TxOrderPrice.unsafeFrom(1),
      123,
      321,
      TxMatcherFee.unsafeFrom(1),
      Waves
    )

    testOrder.isValid(123).labels shouldBe Set("eip712Signature available only in V4")
  }

  it should "work in exchange transaction" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(ethBuyOrder.senderAddress, *)
      sh.creditBalance(ethSellOrder.senderAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val differ      = blockchain.stub.transactionDiffer(TestTime(100))
    val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, price = 100, version = TxVersion.V3, timestamp = 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "work in exchange transaction with an old order" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(ethSellOrder.senderAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val buyOrder = Order
      .selfSigned(
        Order.V3,
        TxHelpers.defaultSigner,
        TxHelpers.matcher.publicKey,
        AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
        OrderType.BUY,
        1,
        100L,
        1,
        123,
        100000,
        Waves
      )
      .explicitGet()

    val differ      = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers.exchange(buyOrder, ethSellOrder, price = 100, version = TxVersion.V3, timestamp = 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "recover valid ids of exchange tx" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthOrdersPublicKey.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val buyOrder = Order
      .selfSigned(
        Order.V3,
        TxHelpers.defaultSigner,
        TxHelpers.matcher.publicKey,
        AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
        OrderType.BUY,
        1,
        100L,
        1,
        123,
        100000,
        Waves
      )
      .explicitGet()
      .withProofs(TxHelpers.signature("2Bi5YFCeAUvQqWFJYUTzaDUfAdoHmQ4RC6nviBwvQgUYJLKrsa4T5eESGr5Er261kdeyNgHVJUGai8mALtLLWDoQ"))

    val sellOrder = ethSellOrder.copy(orderAuthentication =
      EthSignature(
        "0x6c4385dd5f6f1200b4d0630c9076104f34c801c16a211e505facfd743ba242db4429b966ffa8d2a9aff9037dafda78cfc8f7c5ef1c94493f5954bc7ebdb649281b"
      )
    )

    StubHelpers(blockchain).creditBalance(sellOrder.senderAddress, *)

    val transaction = TxHelpers
      .exchange(
        buyOrder,
        sellOrder,
        price = 100,
        buyMatcherFee = buyOrder.matcherFee.value,
        sellMatcherFee = sellOrder.matcherFee.value,
        version = TxVersion.V3,
        timestamp = 100
      )
      .copy(proofs = TxHelpers.signature("4WrABDgkk9JraBLNQK4LTq7LWqVLgLzAEv8fr1rjr4ovca7224EBzLrEgcHdtHscGpQbLsk39ttQfqHMVLr9tXcB"))

    transaction.json() should matchJson(
      """{
        |  "type": 7,
        |  "id": "GtWWteMgnVYeAq4BSbqw9aFM3K17zHrYsij14VtJiVdL",
        |  "fee": 1000000,
        |  "feeAssetId": null,
        |  "timestamp": 100,
        |  "version": 3,
        |  "chainId": 84,
        |  "sender": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
        |  "senderPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
        |  "proofs": [
        |    "4WrABDgkk9JraBLNQK4LTq7LWqVLgLzAEv8fr1rjr4ovca7224EBzLrEgcHdtHscGpQbLsk39ttQfqHMVLr9tXcB"
        |  ],
        |  "order1": {
        |    "version": 3,
        |    "id": "75YqwVQbiQmLMQBE61W1aLcsaAUnWbzM5Udh9Z4mXUBf",
        |    "sender": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
        |    "senderPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
        |    "matcherPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
        |    "assetPair": {
        |      "amountAsset": "5fQPsn8hoaVddFG26cWQ5QFdqxWtUPNaZ9zH2E6LYzFn",
        |      "priceAsset": null
        |    },
        |    "orderType": "buy",
        |    "amount": 1,
        |    "price": 100,
        |    "timestamp": 1,
        |    "expiration": 123,
        |    "matcherFee": 100000,
        |    "signature": "2Bi5YFCeAUvQqWFJYUTzaDUfAdoHmQ4RC6nviBwvQgUYJLKrsa4T5eESGr5Er261kdeyNgHVJUGai8mALtLLWDoQ",
        |    "proofs": [
        |      "2Bi5YFCeAUvQqWFJYUTzaDUfAdoHmQ4RC6nviBwvQgUYJLKrsa4T5eESGr5Er261kdeyNgHVJUGai8mALtLLWDoQ"
        |    ],
        |    "matcherFeeAssetId": null
        |  },
        |  "order2": {
        |    "version": 4,
        |    "id": "6tXL591oH3mnwgFcbxqQnqHBF1oQ1Cc6hdLuBU6FB6UG",
        |    "sender": "3Mvrr424JENHdP4wrSFyNWBVEuQTHBDxMVi",
        |    "senderPublicKey": "4nZcsfxa3mtAg8D2iR8J139CTVm7Y2aTEd3B8J6p45tX6v8sjCT9JGAWnHGa8ZxenQyaSAVu3FPsry1RnXucpcqE",
        |    "matcherPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
        |    "assetPair": {
        |      "amountAsset": "5fQPsn8hoaVddFG26cWQ5QFdqxWtUPNaZ9zH2E6LYzFn",
        |      "priceAsset": null
        |    },
        |    "orderType": "sell",
        |    "amount": 1,
        |    "price": 100,
        |    "timestamp": 1,
        |    "expiration": 123,
        |    "matcherFee": 100000,
        |    "signature": "",
        |    "proofs": [],
        |    "matcherFeeAssetId": null,
        |    "eip712Signature": "0x6c4385dd5f6f1200b4d0630c9076104f34c801c16a211e505facfd743ba242db4429b966ffa8d2a9aff9037dafda78cfc8f7c5ef1c94493f5954bc7ebdb649281b",
        |    "priceMode": null
        |  },
        |  "amount": 1,
        |  "price": 100,
        |  "buyMatcherFee": 100000,
        |  "sellMatcherFee": 100000
        |}""".stripMargin
    )
  }

  it should "not work in exchange transaction with changed signature" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthOrdersPublicKey.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val differ = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers
      .exchange(ethBuyOrder, ethSellOrder, version = TxVersion.V3, timestamp = 100)
      .copy(
        order2 = ethSellOrder.copy(orderAuthentication =
          EthSignature(
            "0x1717804a1d60149988821546732442eabc69f46b2764e231eaeef48351d9f36577278c3f29fe3d61500932190dba8c045b19acda117a4690bfd3d2c28bb67bf91c"
          )
        )
      )

    differ(transaction).resultE should matchPattern {
      case Left(err) if err.toString.contains("negative waves balance") =>
    }
  }

  it should "work in exchange transaction with asset script" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(ethSellOrder.senderAddress, *)

      // TODO: something more smart ?
      val script = TxHelpers.script("""
                                      |match tx {
                                      |  case e: ExchangeTransaction => true
                                      |  case _ => false
                                      |}""".stripMargin)

      sh.issueAsset(ByteStr(EthStubBytes32), Some(script))
    }

    val buyOrder = Order
      .selfSigned(
        Order.V3,
        TxHelpers.defaultSigner,
        TxHelpers.matcher.publicKey,
        AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
        OrderType.BUY,
        1,
        100L,
        1,
        123,
        100000,
        Waves
      )
      .explicitGet()

    val differ      = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers.exchange(buyOrder, ethSellOrder, price = 100, version = TxVersion.V3, timestamp = 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "work in exchange transaction with matcher script" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(ethBuyOrder.senderAddress, *)
      sh.creditBalance(ethSellOrder.senderAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """
          |{-# STDLIB_VERSION 5 #-}
          |{-# CONTENT_TYPE EXPRESSION #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |
          |
          |match tx {
          |  case e: ExchangeTransaction => if (e.buyOrder.proofs[0] == base58'' && e.sellOrder.proofs[0] == base58'') then true else throw("Only ethereum")
          |  case _: Order => true
          |  case _ => false
          |}""".stripMargin
      )
      sh.setScript(TxHelpers.matcher.toAddress, script)
    }

    val differ      = blockchain.stub.transactionDiffer(TestTime(100))
    val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, price = 100, version = TxVersion.V3, timestamp = 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }
}

object EthOrderSpec extends EthHelpers {

  /** Use this method to create a hardcoded signature for a test order
    * @param order
    *   Order parameters
    */
  def signOrder(order: Order): Unit = {
    val signature = EthOrders.signOrder(order, TxHelpers.defaultEthSigner)
    println(EthEncoding.toHexString(signature))
  }

  val ethBuyOrder: Order = Order(
    Order.V4,
    EthSignature(
      "0x0a897d382e4e4a066e1d98e5c3c1051864a557c488571ff71e036c0f5a2c7204274cb293cd4aa7ad40f8c2f650e1a2770ecca6aa14a1da883388fa3b5b9fa8b71c"
    ),
    TxHelpers.matcher.publicKey,
    AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
    OrderType.BUY,
    TxExchangeAmount.unsafeFrom(1),
    TxOrderPrice.unsafeFrom(100L),
    1,
    123,
    TxMatcherFee.unsafeFrom(100000),
    Waves
  )

  val ethSellOrder: Order = Order(
    Order.V4,
    EthSignature(
      "0x6c4385dd5f6f1200b4d0630c9076104f34c801c16a211e505facfd743ba242db4429b966ffa8d2a9aff9037dafda78cfc8f7c5ef1c94493f5954bc7ebdb649281b"
    ),
    TxHelpers.matcher.publicKey,
    AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
    OrderType.SELL,
    TxExchangeAmount.unsafeFrom(1),
    TxOrderPrice.unsafeFrom(100L),
    1,
    123,
    TxMatcherFee.unsafeFrom(100000),
    Waves
  )
}
