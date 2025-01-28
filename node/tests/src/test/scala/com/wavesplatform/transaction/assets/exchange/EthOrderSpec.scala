package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.{TxExchangeAmount, TxHelpers, TxMatcherFee, TxOrderPrice, TxVersion}
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.utils.{EthEncoding, EthHelpers, JsonMatchers}
import org.scalatest.{Assertion, ParallelTestExecution}
import play.api.libs.json.{JsArray, JsObject, Json}
import org.web3j.crypto.Bip32ECKeyPair

class EthOrderSpec extends FlatSpec with EthHelpers with WithDomain with ParallelTestExecution with JsonMatchers {
  import EthOrderSpec.{ethBuyOrderSigned, ethSellOrderSigned}

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

  it should s"recover signer public key with leading zeros correctly" in {

    val testOrder = Order(
      Order.V4,
      EthSignature(
        "0xc3b8c59ee779ef7b308e44d3c24b0f05687eaebc49f7f94fe0cc4f6fb13bae351adfce1419d6d35c41d5bd7fdefd87871f1ed3b9df8771d1eb76e981adf48e741b"
      ),
      PublicKey.fromBase58String("9cpfKN9suPNvfeUNphzxXMjcnn974eme8ZhWUjaktzU5").explicitGet(),
      AssetPair(Waves, IssuedAsset(ByteStr(Base58.decode("34N9YcEETLWn93qYQ64EsP1x89tSruJU44RrEMSXXEPJ")))),
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(211125290L),
      TxOrderPrice.unsafeFrom(2357071L),
      1668605799020L,
      1671111399020L,
      TxMatcherFee.unsafeFrom(23627L),
      IssuedAsset(ByteStr(Base58.decode("34N9YcEETLWn93qYQ64EsP1x89tSruJU44RrEMSXXEPJ"))),
      OrderPriceMode.AssetDecimals
    )

    val resultFixed = EthOrders.recoverEthSignerKey(testOrder, testOrder.eip712Signature.get.arr)
    EthEncoding.toHexString(
      resultFixed.arr
    ) shouldBe "0x00d7cf9ff594b07273228e7dd591707d38a1dba0a39492fd64445ba9cbb3bf66c862b9752f02bf8d1a0f00ccb11ae550a7616bd965c10f0101202d75580786ee"
  }

  it should "recover signer public key when v < 27 in signature data" in {
    val testOrder = Order(
      Order.V4,
      EthSignature(
        "0x12f72d3bba93bda930ee5c280e1d39b7e7dcc439d789c92eff40ea860480213a0e79323093c8aee04c2a269de01c7d587a18b02d02746dec75ec1457accb72a301"
      ),
      PublicKey.fromBase58String("8QUAqtTckM5B8gvcuP7mMswat9SjKUuafJMusEoSn1Gy").explicitGet(),
      AssetPair(Waves, IssuedAsset(ByteStr(Base58.decode("25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT")))),
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(100000000L),
      TxOrderPrice.unsafeFrom(14781968L),
      1668520875679L,
      1671026475679L,
      TxMatcherFee.unsafeFrom(24884L),
      IssuedAsset(ByteStr(Base58.decode("25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT"))),
      OrderPriceMode.AssetDecimals
    )

    val result = EthOrders.recoverEthSignerKey(testOrder, testOrder.eip712Signature.get.arr)
    result.toAddress.toString shouldBe "3N8HNri7zQXVw8Bn9BZKGRpsznNUFXM24zL"
  }

  it should "recover public key at json parse stage" in {
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
    val assetIssuer      = TxHelpers.defaultSigner
    val buyerEthAccount  = TxHelpers.signer(1).toEthKeyPair
    val sellerEthAccount = TxHelpers.signer(2).toEthKeyPair

    val balances = Seq(
      AddrWithBalance(buyerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(sellerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(TxHelpers.matcher.toAddress, 1000.waves)
    )

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Issue an asset
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to seller
      d.appendBlock(TxHelpers.transfer(assetIssuer, sellerEthAccount.toWavesAddress, 1000L, testAsset))

      val ethBuyOrder  = ethBuyOrderSigned(testAsset, buyerEthAccount, TxHelpers.timestamp)
      val ethSellOrder = ethSellOrderSigned(testAsset, sellerEthAccount, TxHelpers.timestamp)

      val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxHelpers.matcher, price = 100, version = TxVersion.V3)

      d.appendBlock(transaction)
      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
    }
  }

  it should "work in exchange transaction with an old order" in {
    val assetIssuer      = TxHelpers.defaultSigner
    val buyerAccount     = TxHelpers.signer(1)
    val sellerEthAccount = TxHelpers.signer(2).toEthKeyPair

    val balances = Seq(
      AddrWithBalance(buyerAccount.toAddress, 1000.waves),
      AddrWithBalance(sellerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(TxHelpers.matcher.toAddress, 1000.waves)
    )

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Issue an asset
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue, 8)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to seller
      d.appendBlock(TxHelpers.transfer(assetIssuer, sellerEthAccount.toWavesAddress, 1000L, testAsset))

      val buyOrder = Order
        .selfSigned(
          Order.V3,
          buyerAccount,
          TxHelpers.matcher.publicKey,
          AssetPair(testAsset, Waves),
          OrderType.BUY,
          1,
          100L,
          TxHelpers.timestamp,
          TxHelpers.timestamp + 10000,
          100000,
          Waves
        )
        .explicitGet()

      val ethSellOrder = ethSellOrderSigned(testAsset, sellerEthAccount, TxHelpers.timestamp)

      val transaction = TxHelpers.exchange(buyOrder, ethSellOrder, TxHelpers.matcher, price = 100, version = TxVersion.V3)

      d.appendBlock(transaction)
      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
    }
  }

  it should "recover valid ids of exchange tx" in {
    val assetIssuer      = TxHelpers.defaultSigner
    val buyerAccount     = TxHelpers.signer(1)
    val sellerEthAccount = TxHelpers.signer(2).toEthKeyPair

    val balances = Seq(
      AddrWithBalance(buyerAccount.toAddress, 1000.waves),
      AddrWithBalance(sellerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(TxHelpers.matcher.toAddress, 1000.waves)
    )

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Issue an asset
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue, 8)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to seller
      d.appendBlock(TxHelpers.transfer(assetIssuer, sellerEthAccount.toWavesAddress, 1000L, testAsset))

      val timestamp = TxHelpers.timestamp

      val buyOrder = Order
        .selfSigned(
          Order.V3,
          buyerAccount,
          TxHelpers.matcher.publicKey,
          AssetPair(testAsset, Waves),
          OrderType.BUY,
          1,
          100L,
          timestamp,
          timestamp + 10000,
          100000,
          Waves
        )
        .explicitGet()

      val ethSellOrder = ethSellOrderSigned(testAsset, sellerEthAccount, timestamp)

      val transaction = TxHelpers.exchange(buyOrder, ethSellOrder, TxHelpers.matcher, price = 100, version = TxVersion.V3)

      d.appendBlock(transaction)
      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)

      transaction.json() should matchJson(
        s"""
           |{
           |  "type": 7,
           |  "id": "${transaction.id().toString}",
           |  "fee": 1000000,
           |  "feeAssetId": null,
           |  "timestamp": ${transaction.timestamp},
           |  "version": 3,
           |  "chainId": 84,
           |  "sender": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
           |  "senderPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
           |  "proofs": [ "${transaction.proofs.base58.value().head}" ],
           |  "order1": {
           |    "version": 3,
           |    "id": "${buyOrder.id().toString}",
           |    "sender": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
           |    "senderPublicKey": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr",
           |    "matcherPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
           |    "assetPair": {
           |      "amountAsset": "${testAsset.toString}",
           |      "priceAsset": null
           |    },
           |    "orderType": "buy",
           |    "amount": 1,
           |    "price": 100,
           |    "timestamp": ${timestamp},
           |    "expiration": ${timestamp + 10000},
           |    "matcherFee": 100000,
           |    "signature": "${buyOrder.signature.toString}",
           |    "proofs": [ "${buyOrder.proofs.base58.value().head}" ],
           |    "matcherFeeAssetId": null
           |  },
           |  "order2": {
           |    "version": 4,
           |    "id": "${ethSellOrder.id().toString}",
           |    "sender": "3N6Kr345mXL1NJGm7g4fd83BwLCb5wcfqiG",
           |    "senderPublicKey": "3bw8NgoV6fE6JnX1mBhggFZH12SyEw4rCfLG9ZVyLNRahwhC2qPW4xJwBawBB1n9gfDkg2bwr3wTtZ4vTjfiXgEv",
           |    "matcherPublicKey": "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
           |    "assetPair": {
           |      "amountAsset": "${testAsset.toString}",
           |      "priceAsset": null
           |    },
           |    "orderType": "sell",
           |    "amount": 1,
           |    "price": 100,
           |    "timestamp": ${timestamp},
           |    "expiration": ${timestamp + 10000},
           |    "matcherFee": 100000,
           |    "signature": "",
           |    "proofs": [ ],
           |    "matcherFeeAssetId": null,
           |    "eip712Signature": "${EthEncoding.toHexString(ethSellOrder.eip712Signature.get.arr)}",
           |    "priceMode": null
           |  },
           |  "amount": 1,
           |  "price": 100,
           |  "buyMatcherFee": 1,
           |  "sellMatcherFee": 1
           |}""".stripMargin
      )
    }
  }

  it should "not work in exchange transaction with changed signature" in {
    val assetIssuer      = TxHelpers.defaultSigner
    val buyerEthAccount  = TxHelpers.signer(1).toEthKeyPair
    val sellerEthAccount = TxHelpers.signer(2).toEthKeyPair

    val balances = Seq(
      AddrWithBalance(buyerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(sellerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(TxHelpers.matcher.toAddress, 1000.waves)
    )

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Issue an asset
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to seller
      d.appendBlock(TxHelpers.transfer(assetIssuer, sellerEthAccount.toWavesAddress, 1000L, testAsset))

      val ethBuyOrder  = ethBuyOrderSigned(testAsset, buyerEthAccount, TxHelpers.timestamp)
      val ethSellOrder = ethSellOrderSigned(testAsset, sellerEthAccount, TxHelpers.timestamp)

      val transaction = TxHelpers
        .exchange(ethBuyOrder, ethSellOrder, price = 100, version = TxVersion.V3)
        .copy(
          order2 = ethSellOrder.copy(orderAuthentication =
            EthSignature(
              "0x1717804a1d60149988821546732442eabc69f46b2764e231eaeef48351d9f36577278c3f29fe3d61500932190dba8c045b19acda117a4690bfd3d2c28bb67bf91c"
            )
          )
        )

      d.appendBlockE(transaction) should matchPattern {
        case Left(err) if err.toString.contains("negative waves balance") =>
      }
    }
  }

  it should "work in exchange transaction with asset script" in {
    val assetIssuer      = TxHelpers.defaultSigner
    val buyerEthAccount  = TxHelpers.signer(1).toEthKeyPair
    val sellerEthAccount = TxHelpers.signer(2).toEthKeyPair

    val balances = Seq(
      AddrWithBalance(buyerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(sellerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(TxHelpers.matcher.toAddress, 1000.waves)
    )

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Issue an asset
      // TODO: something more smart ?
      val script = TxHelpers.script("""
                                      |match tx {
                                      |  case _: ExchangeTransaction => true
                                      |  case _: TransferTransaction => true
                                      |  case _ => false
                                      |}""".stripMargin)
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue, script = Some(script))
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to seller
      d.appendBlock(TxHelpers.transfer(assetIssuer, sellerEthAccount.toWavesAddress, 1000L, testAsset))

      val ethBuyOrder  = ethBuyOrderSigned(testAsset, buyerEthAccount, TxHelpers.timestamp)
      val ethSellOrder = ethSellOrderSigned(testAsset, sellerEthAccount, TxHelpers.timestamp)

      val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxHelpers.matcher, price = 100, version = TxVersion.V3)

      d.appendBlock(transaction)
      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
    }
  }

  it should "work in exchange transaction with matcher script" in {
    val assetIssuer      = TxHelpers.defaultSigner
    val buyerEthAccount  = TxHelpers.signer(1).toEthKeyPair
    val sellerEthAccount = TxHelpers.signer(2).toEthKeyPair

    val balances = Seq(
      AddrWithBalance(buyerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(sellerEthAccount.toWavesAddress, 1000.waves),
      AddrWithBalance(TxHelpers.matcher.toAddress, 1000.waves)
    )

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Issue an asset
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to seller
      d.appendBlock(TxHelpers.transfer(assetIssuer, sellerEthAccount.toWavesAddress, 1000L, testAsset))

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

      d.appendBlock(TxHelpers.setScript(TxHelpers.matcher, script))

      val ethBuyOrder  = ethBuyOrderSigned(testAsset, buyerEthAccount, TxHelpers.timestamp)
      val ethSellOrder = ethSellOrderSigned(testAsset, sellerEthAccount, TxHelpers.timestamp)

      val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxHelpers.matcher, price = 100, version = TxVersion.V3)

      d.appendBlock(transaction)
      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
    }
  }

  it should "be serialized correctly to EIP-712 json with and without attachment (NODE-996)" in {
    val signer = TxHelpers.defaultEthSigner

    def checkOrderJson(json: JsObject, version: Int, attachment: Option[ByteStr]): Assertion = {
      (json \ "domain" \ "version").as[String] shouldBe version.toString
      (json \ "types" \ "Order")
        .as[JsArray]
        .value
        .exists(_.as[JsObject].fields.map { case (name, value) =>
          name -> value.as[String]
        } == Seq("name" -> "attachment", "type" -> "string")) shouldBe attachment.isDefined
      (json \ "message" \ "attachment").asOpt[String] shouldBe attachment.map(_.toString)
    }

    def testOrder(attachment: Option[ByteStr]): Order = {
      val order = Order
        .buy(
          Order.V4,
          TxHelpers.defaultSigner,
          TxHelpers.secondSigner.publicKey,
          AssetPair(Waves, IssuedAsset(ByteStr.fill(32)(1))),
          100,
          100,
          100,
          100,
          100,
          attachment = attachment
        )
        .explicitGet()

      order.copy(orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(EthOrders.signOrder(order, signer))))
    }

    val attachment             = Some(ByteStr.fill(32)(1))
    val orderWithoutAttachment = testOrder(None)
    val orderWithAttachment    = testOrder(attachment)

    val jsonWithoutAttachment = EthOrders.toEip712Json(orderWithoutAttachment)
    checkOrderJson(jsonWithoutAttachment, 1, None)

    val jsonWithAttachment = EthOrders.toEip712Json(orderWithAttachment)
    checkOrderJson(jsonWithAttachment, 2, attachment)
  }

  it should "recover signer public key for order with and without attachment correctly (NODE-997)" in {
    val signer = TxHelpers.defaultEthSigner

    def testOrder(attachment: Option[ByteStr]): Order = {
      val order = Order
        .buy(
          Order.V4,
          TxHelpers.defaultSigner,
          TxHelpers.secondSigner.publicKey,
          AssetPair(Waves, IssuedAsset(ByteStr.fill(32)(1))),
          100,
          100,
          100,
          100,
          100,
          attachment = attachment
        )
        .explicitGet()

      order.copy(orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(EthOrders.signOrder(order, signer))))
    }

    val orderWithoutAttachment = testOrder(None)
    val orderWithAttachment    = testOrder(Some(ByteStr.fill(32)(1)))

    val expectedPk = PublicKey(EthEncoding.toBytes(signer.getPublicKey.toString(16)))

    EthOrders.recoverEthSignerKey(orderWithoutAttachment, orderWithoutAttachment.eip712Signature.get.arr) shouldBe expectedPk

    EthOrders.recoverEthSignerKey(orderWithAttachment, orderWithAttachment.eip712Signature.get.arr) shouldBe expectedPk
  }
}

object EthOrderSpec extends EthHelpers {
  private val emptySignature = OrderAuthentication.Eip712Signature(ByteStr(new Array[Byte](64)))

  def ethBuyOrderSigned(testAsset: IssuedAsset, buyerEthAccount: Bip32ECKeyPair, timestamp: Long): Order = {
    val ethBuyOrderTemplate: Order = Order(
      Order.V4,
      emptySignature,
      TxHelpers.matcher.publicKey,
      AssetPair(testAsset, Waves),
      OrderType.BUY,
      TxExchangeAmount.unsafeFrom(1),
      TxOrderPrice.unsafeFrom(100L),
      timestamp,
      timestamp + 10000,
      TxMatcherFee.unsafeFrom(100000),
      Waves
    )

    ethBuyOrderTemplate.copy(
      orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(EthOrders.signOrder(ethBuyOrderTemplate, buyerEthAccount)))
    )
  }

  def ethSellOrderSigned(testAsset: IssuedAsset, sellerEthAccount: Bip32ECKeyPair, timestamp: Long): Order = {
    val ethSellOrderTemplate: Order = Order(
      Order.V4,
      emptySignature,
      TxHelpers.matcher.publicKey,
      AssetPair(testAsset, Waves),
      OrderType.SELL,
      TxExchangeAmount.unsafeFrom(1),
      TxOrderPrice.unsafeFrom(100L),
      timestamp,
      timestamp + 10000,
      TxMatcherFee.unsafeFrom(100000),
      Waves
    )

    ethSellOrderTemplate.copy(
      orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(EthOrders.signOrder(ethSellOrderTemplate, sellerEthAccount)))
    )
  }
}
