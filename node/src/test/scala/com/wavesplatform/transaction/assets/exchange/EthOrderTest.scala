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

class EthOrderTest
    extends FlatSpec
    with BeforeAndAfterAll
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with EthSetChainId
    with DiffMatchers {
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
        "0xb557dae4c614146dd35ba6fd80e4702a75d33ffcb8af09e80e0c1a7386b8ffcb5b76bd8037f6484de809a80a5b39a224301c76e8bad9b1a9e7ada53ba6fa7e361c"
      )

    val result = EthOrders.recoverEthSignerKey(testOrder, signature)
    result shouldBe TestEthPublicKey
    result.toEthAddress shouldBe TestEthPublicKey.toEthAddress
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
      ethSignature = EthSignature(
        "0xb557dae4c614146dd35ba6fd80e4702a75d33ffcb8af09e80e0c1a7386b8ffcb5b76bd8037f6484de809a80a5b39a224301c76e8bad9b1a9e7ada53ba6fa7e361c"
      )
    )

    testOrder.isValid(123).labels shouldBe Set("ethSignature available only in V4")
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
      ethSignature = EthSignature(
        "0xb557dae4c614146dd35ba6fd80e4702a75d33ffcb8af09e80e0c1a7386b8ffcb5b76bd8037f6484de809a80a5b39a224301c76e8bad9b1a9e7ada53ba6fa7e361c"
      )
    )

    testOrder.isValid(123).labels shouldBe Set("ethSignature excludes proofs")
  }

  it should "work in exchange transaction" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toEthAddress.asWaves, *)
      sh.issueAsset(ByteStr(EthStubBytes32))
    }

    val buyOrder = Order(
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
      ethSignature = EthSignature(
        "0x1717804a1d60149988821546732442eabc69f46b2764e231eaeef48351d9f36577278c3f29fe3d61500932190dba8c045b19acda117a4690bfd3d2c28bb67bf91c"
      )
    )

    val sellOrder = Order(
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
      ethSignature = EthSignature(
        "0xd1a95bf94c6a3be6b7bf929d2c68263b1c88a520c67445ff1fba1d73e2b852ca2a09ebc50a0760e8683d72e4060109030591a3678d51b259da034c24579648aa1b"
      )
    )

    val differ      = blockchain.stub.transactionDiffer(TestTime(100))
    val transaction = TxHelpers.exchange(buyOrder, sellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "work in exchange transaction with old order" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toEthAddress.asWaves, *)
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

    val sellOrder = Order(
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
      ethSignature = Some(
        ByteStr(
          EthEncoding.toBytes(
            "0xd1a95bf94c6a3be6b7bf929d2c68263b1c88a520c67445ff1fba1d73e2b852ca2a09ebc50a0760e8683d72e4060109030591a3678d51b259da034c24579648aa1b"
          )
        )
      )
    )

    val differ      = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers.exchange(buyOrder, sellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "not work in exchange transaction with changed signature" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toEthAddress.asWaves, *)
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

    val sellOrder = Order(
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
      ethSignature = EthSignature(
        "0xd1a95bf94c6a3be6b7bf929d2c68263b1c88a520c67445ff1fba1d73e2b852ca2a09ebc50a0760e8683d72e4060109030591a3678d51b259da034c24579648aa1b"
      )
    )

    val differ = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers
      .exchange(buyOrder, sellOrder, TxVersion.V3, 100)
      .copy(
        order2 = sellOrder.copy(
          ethSignature = EthSignature(
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
      sh.creditBalance(TestEthPublicKey.toEthAddress.asWaves, *)

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

    val sellOrder = Order(
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
      ethSignature = Some(
        ByteStr(
          EthEncoding.toBytes(
            "0xd1a95bf94c6a3be6b7bf929d2c68263b1c88a520c67445ff1fba1d73e2b852ca2a09ebc50a0760e8683d72e4060109030591a3678d51b259da034c24579648aa1b"
          )
        )
      )
    )

    val differ      = TransactionDiffer(Some(1L), 100L)(blockchain, _)
    val transaction = TxHelpers.exchange(buyOrder, sellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }

  it should "work in exchange transaction with matcher script" in {
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.creditBalance(TxHelpers.matcher.toAddress, *)
      sh.creditBalance(TestEthPublicKey.toEthAddress.asWaves, *)
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

    val buyOrder = Order(
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
      ethSignature = EthSignature(
        "0x1717804a1d60149988821546732442eabc69f46b2764e231eaeef48351d9f36577278c3f29fe3d61500932190dba8c045b19acda117a4690bfd3d2c28bb67bf91c"
      )
    )

    val sellOrder = Order(
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
      ethSignature = EthSignature(
        "0xd1a95bf94c6a3be6b7bf929d2c68263b1c88a520c67445ff1fba1d73e2b852ca2a09ebc50a0760e8683d72e4060109030591a3678d51b259da034c24579648aa1b"
      )
    )

    val differ      = blockchain.stub.transactionDiffer(TestTime(100))
    val transaction = TxHelpers.exchange(buyOrder, sellOrder, TxVersion.V3, 100)
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
  }
}
