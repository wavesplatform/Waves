package com.wavesplatform.transaction.smart

import cats.syntax.monoid._
import com.wavesplatform.{BlockchainStubHelpers, TestValues}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs.produceE
import com.wavesplatform.state.Portfolio
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.{ERC20Address, TxHelpers}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters._
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.utils.EthTxGenerator.Arg
import com.wavesplatform.utils.{DiffMatchers, EthHelpers, EthSetChainId, JsonMatchers}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll
import play.api.libs.json.Json

class EthTransactionSpec
    extends FlatSpec
    with BeforeAndAfterAll
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with EthSetChainId
    with DiffMatchers
    with JsonMatchers {

  val TestAsset: IssuedAsset = TestValues.asset

  "Ethereum transfer" should "work with long.max" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val senderAddress    = TxHelpers.defaultSigner.toEthWavesAddress
    val recipientAddress = TxHelpers.secondSigner.toAddress

    val blockchain = createBlockchainStub { b =>
      b.stub.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      b.stub.creditBalance(senderAddress, Waves, Long.MaxValue)
      b.stub.creditBalance(senderAddress, TestAsset, Long.MaxValue)
      (b.resolveERC20Address _).when(ERC20Address(TestAsset.id.take(20))).returning(Some(TestAsset))
    }
    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis())).andThen(_.resultE.explicitGet())

    val TransferValue = Long.MaxValue - 200000
    val transfer      = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, TransferValue, Waves)
    val assetTransfer = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, TransferValue, TestAsset)

    (differ(transfer) |+| differ(assetTransfer)).portfolios shouldBe Map(
      senderAddress    -> Portfolio(-Long.MaxValue, assets = Map(TestAsset -> -TransferValue)),
      recipientAddress -> Portfolio(TransferValue, assets = Map(TestAsset  -> TransferValue))
    )
  }

  it should "use chainId in signer key recovery" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val senderAddress    = TxHelpers.defaultSigner.toEthWavesAddress
    val recipientAddress = TxHelpers.secondSigner.toAddress('W'.toByte) // Other network

    val blockchain = createBlockchainStub { b =>
      b.stub.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      b.stub.creditBalance(senderAddress, Waves)
      b.stub.creditBalance(senderAddress, TestAsset)
      (b.resolveERC20Address _).when(ERC20Address(TestAsset.id.take(20))).returning(Some(TestAsset))
    }
    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis())).andThen(_.resultE.explicitGet())

    val transfer      = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, 1, Waves)
    val assetTransfer = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, 1, TestAsset)

    intercept[RuntimeException](differ(transfer)).toString should include("negative waves balance")
    intercept[RuntimeException](differ(assetTransfer)).toString should include("negative waves balance")
  }

  it should "not accept zero transfers" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress
    intercept[RuntimeException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, 0, Waves)).toString should include(
      "Transaction cancellation is not supported"
    )
    intercept[RuntimeException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, 0, TestAsset)).toString should include(
      "NonPositiveAmount"
    )
    intercept[RuntimeException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, -1, Waves)).toString should include(
      "NegativeAmount"
    )
    intercept[UnsupportedOperationException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, -1, TestAsset))
  }

  "Ethereum invoke" should "work with all types of arguments" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func deposit(amount: Int, bs: ByteVector, str: String, bool: Boolean, list: List[Int], union: ByteVector|String) = {
          |  [
          |    ScriptTransfer(i.caller, amount, unit)
          |  ]
          |}
          |""".stripMargin
      )
      sh.setScript(dAppAccount.toAddress, script)
    }

    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis()))
    val transaction = EthTxGenerator.generateEthInvoke(
      invokerAccount,
      dAppAccount.toAddress,
      "deposit",
      Seq(
        Arg.Integer(123),
        Arg.Bytes(ByteStr.empty),
        Arg.Str("123"),
        Arg.Bool(true),
        Arg.List(Arg.Integer(0), Seq(Arg.Integer(123))),
        Arg.Union(0, Seq(Arg.Str("123"), Arg.Bytes(ByteStr.empty)))
      ),
      Seq(Payment(321, IssuedAsset(ByteStr(EthStubBytes32))))
    )
    val diff = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
    Json.toJson(diff.scriptResults.values.head) should matchJson("""{
                                                                   |  "data" : [ ],
                                                                   |  "transfers" : [ {
                                                                   |    "address" : "3G9uRSP4uVjTFjGZixYW4arBZUKWHxjnfeW",
                                                                   |    "asset" : null,
                                                                   |    "amount" : 123
                                                                   |  } ],
                                                                   |  "issues" : [ ],
                                                                   |  "reissues" : [ ],
                                                                   |  "burns" : [ ],
                                                                   |  "sponsorFees" : [ ],
                                                                   |  "leases" : [ ],
                                                                   |  "leaseCancels" : [ ],
                                                                   |  "invokes" : [ ]
                                                                   |}""".stripMargin)
  }

  it should "work with no arguments" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func deposit() = {
          |  [
          |    ScriptTransfer(i.caller, 123, unit)
          |  ]
          |}
          |""".stripMargin
      )
      sh.setScript(dAppAccount.toAddress, script)
    }

    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis()))
    val transaction = EthTxGenerator.generateEthInvoke(
      invokerAccount,
      dAppAccount.toAddress,
      "deposit",
      Seq(),
      Seq(Payment(321, IssuedAsset(ByteStr(EthStubBytes32))))
    )
    val diff = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
    Json.toJson(diff.scriptResults.values.head) should matchJson("""{
                                                                   |  "data" : [ ],
                                                                   |  "transfers" : [ {
                                                                   |    "address" : "3G9uRSP4uVjTFjGZixYW4arBZUKWHxjnfeW",
                                                                   |    "asset" : null,
                                                                   |    "amount" : 123
                                                                   |  } ],
                                                                   |  "issues" : [ ],
                                                                   |  "reissues" : [ ],
                                                                   |  "burns" : [ ],
                                                                   |  "sponsorFees" : [ ],
                                                                   |  "leases" : [ ],
                                                                   |  "leaseCancels" : [ ],
                                                                   |  "invokes" : [ ]
                                                                   |}""".stripMargin)
  }

  it should "work with no payments" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func deposit() = {
          |  [
          |    ScriptTransfer(i.caller, 123, unit)
          |  ]
          |}
          |""".stripMargin
      )
      sh.setScript(dAppAccount.toAddress, script)
    }

    val differ      = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis()))
    val transaction = EthTxGenerator.generateEthInvoke(invokerAccount, dAppAccount.toAddress, "deposit", Seq(), Seq())
    val diff        = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
    Json.toJson(diff.scriptResults.values.head) should matchJson("""{
                                                                   |  "data" : [ ],
                                                                   |  "transfers" : [ {
                                                                   |    "address" : "3G9uRSP4uVjTFjGZixYW4arBZUKWHxjnfeW",
                                                                   |    "asset" : null,
                                                                   |    "amount" : 123
                                                                   |  } ],
                                                                   |  "issues" : [ ],
                                                                   |  "reissues" : [ ],
                                                                   |  "burns" : [ ],
                                                                   |  "sponsorFees" : [ ],
                                                                   |  "leases" : [ ],
                                                                   |  "leaseCancels" : [ ],
                                                                   |  "invokes" : [ ]
                                                                   |}""".stripMargin)
  }

  it should "fail with max+1 payments" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 5 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func deposit() = {
          |  [
          |    ScriptTransfer(i.caller, 123, unit)
          |  ]
          |}
          |""".stripMargin
      )
      sh.setScript(dAppAccount.toAddress, script)
    }

    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis()))
    val transaction = EthTxGenerator.generateEthInvoke(
      invokerAccount,
      dAppAccount.toAddress,
      "deposit",
      Seq(),
      (1 to com.wavesplatform.lang.v1.ContractLimits.MaxAttachedPaymentAmountV5 + 1).map(InvokeScriptTransaction.Payment(_, Waves))
    )
    differ(transaction).resultE should produceE("Script payment amount=11 should not exceed 10")
  }

  it should "work with default function" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func default() = {
          |  [
          |    ScriptTransfer(i.caller, 123, unit)
          |  ]
          |}
          |""".stripMargin
      )
      sh.setScript(dAppAccount.toAddress, script)
    }

    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis()))
    val transaction = EthTxGenerator.generateEthInvoke(
      invokerAccount,
      dAppAccount.toAddress,
      "default",
      Seq(),
      Seq(Payment(321, IssuedAsset(ByteStr(EthStubBytes32))))
    )
    val diff = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
    Json.toJson(diff.scriptResults.values.head) should matchJson("""{
                                                                   |  "data" : [ ],
                                                                   |  "transfers" : [ {
                                                                   |    "address" : "3G9uRSP4uVjTFjGZixYW4arBZUKWHxjnfeW",
                                                                   |    "asset" : null,
                                                                   |    "amount" : 123
                                                                   |  } ],
                                                                   |  "issues" : [ ],
                                                                   |  "reissues" : [ ],
                                                                   |  "burns" : [ ],
                                                                   |  "sponsorFees" : [ ],
                                                                   |  "leases" : [ ],
                                                                   |  "leaseCancels" : [ ],
                                                                   |  "invokes" : [ ]
                                                                   |}""".stripMargin)
  }
}
