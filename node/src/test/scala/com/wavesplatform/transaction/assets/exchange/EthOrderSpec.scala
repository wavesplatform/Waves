package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.common.utils._
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.transaction.{Proofs, TxHelpers, TxVersion}
import com.wavesplatform.utils.{DiffMatchers, EthEncoding, EthHelpers, EthSetChainId}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll

class EthOrderSpec
    extends FlatSpec
    with BeforeAndAfterAll
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with EthSetChainId
    with DiffMatchers {
  val ethBuyOrder = Order(
    Order.V4,
    TestEthPublicKey,
    TxHelpers.matcher.publicKey,
    AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
    OrderType.BUY,
    1,
    100L,
    1,
    123,
    100000,
    Waves,
    eip712Signature = EthSignature(
      "0xe5ff562bfb0296e95b631365599c87f1c5002597bf56a131f289765275d2580f5344c62999404c37cd858ea037328ac91eca16ad1ce69c345ebb52fde70b66251c"
    )
  )

  val ethSellOrder = Order(
    Order.V4,
    TestEthPublicKey,
    TxHelpers.matcher.publicKey,
    AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
    OrderType.SELL,
    1,
    100L,
    1,
    123,
    100000,
    Waves,
    eip712Signature = EthSignature(
      "0xc8ba2bdafd27742546b3be34883efc51d6cdffbb235798d7b51876c6854791f019b0522d7a39b6f2087cba46ae86919b71a2d9d7920dfc8e00246d8f02a258f21b"
    )
  )

  "ETH signed order" should "recover signer public key correctly" in {
    val testOrder = Order(
      Order.V1,
      PublicKey(EthStubBytes32),
      PublicKey(EthStubBytes32),
      AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), IssuedAsset(ByteStr(EthStubBytes32))),
      OrderType.BUY,
      1,
      1,
      123,
      321,
      1,
      IssuedAsset(ByteStr(EthStubBytes32))
    )

    val signature =
      EthEncoding.toBytes(
        "0x54119bc5b24d9363b7a1a31a71a2e6194dfeedc5e9644893b0a04bb57004e5b14342c1ce29ee00877da49180fd6d7fb332ff400231f809da7ed0dcb07c504e2d1c"
      )

    val result = EthOrders.recoverEthSignerKey(testOrder, signature)
    result shouldBe TestEthPublicKey
    result.toAddress shouldBe TestEthPublicKey.toAddress
  }

  it should "be of version 4" in {
    val testOrder = Order(
      Order.V1,
      PublicKey(EthStubBytes32),
      PublicKey(EthStubBytes32),
      AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
      OrderType.BUY,
      1,
      1,
      123,
      321,
      1,
      Waves,
      eip712Signature = EthSignature(
        "0xb557dae4c614146dd35ba6fd80e4702a75d33ffcb8af09e80e0c1a7386b8ffcb5b76bd8037f6484de809a80a5b39a224301c76e8bad9b1a9e7ada53ba6fa7e361c"
      )
    )

    testOrder.isValid(123).labels shouldBe Set("eip712Signature available only in V4")
  }

  it should "be not contain proofs" in {
    val testOrder = Order(
      Order.V4,
      PublicKey(EthStubBytes32),
      PublicKey(EthStubBytes32),
      AssetPair(IssuedAsset(ByteStr(EthStubBytes32)), Waves),
      OrderType.BUY,
      1,
      1,
      123,
      321,
      1,
      Waves,
      Proofs(ByteStr.empty),
      eip712Signature = EthSignature(
        "0xb557dae4c614146dd35ba6fd80e4702a75d33ffcb8af09e80e0c1a7386b8ffcb5b76bd8037f6484de809a80a5b39a224301c76e8bad9b1a9e7ada53ba6fa7e361c"
      )
    )

    testOrder.isValid(123).labels shouldBe Set("eip712Signature excludes proofs")
  }

  it should "work in exchange transaction" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val differ      = blockchain.stub.transactionDiffer(TestTime(100))
    val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "work in exchange transaction with old order" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val buyOrder = Order.selfSigned(
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

    val differ      = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers.exchange(buyOrder, ethSellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "not work in exchange transaction with changed signature" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val differ = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers
      .exchange(ethBuyOrder, ethSellOrder, TxVersion.V3, 100)
      .copy(
        order2 = ethSellOrder.copy(
          eip712Signature = EthSignature(
            "0x1717804a1d60149988821546732442eabc69f46b2764e231eaeef48351d9f36577278c3f29fe3d61500932190dba8c045b19acda117a4690bfd3d2c28bb67bf91c"
          )
        )
      )

    differ(transaction).resultE should matchPattern {
      case Left(err) if err.toString.contains("Proof doesn't validate as signature") =>
    }
  }

  it should "work in exchange transaction with asset script" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toAddress, *)

      // TODO: something more smart ?
      val script = TxHelpers.script("""
                                      |match tx {
                                      |  case e: ExchangeTransaction => true
                                      |  case _ => false
                                      |}""".stripMargin)

      sh.issueAsset(ByteStr(EthStubBytes32), Some(script))
    }

    val buyOrder = Order.selfSigned(
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

    val differ      = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers.exchange(buyOrder, ethSellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "work in exchange transaction with matcher script" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toAddress, *)
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
    val transaction = TxHelpers.exchange(ethBuyOrder, ethSellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }
}
