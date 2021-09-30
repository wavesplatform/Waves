package com.wavesplatform.transaction.smart

import cats.syntax.monoid._
import com.wavesplatform.{BlockchainStubHelpers, TestValues}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs.produceE
import com.wavesplatform.state.Portfolio
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.{ERC20Address, EthereumTransaction, TxHelpers}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters._
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.utils.EthTxGenerator.{signRawTransaction, Arg}
import com.wavesplatform.utils.{DiffMatchers, EthEncoding, EthHelpers, EthSetChainId, JsonMatchers}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll
import org.web3j.crypto.RawTransaction
import play.api.libs.json.Json
import scala.concurrent.duration._

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
      b.stub.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls, BlockchainFeatures.Ride4DApps)
      b.stub.creditBalance(senderAddress, Waves, Long.MaxValue)
      b.stub.creditBalance(senderAddress, TestAsset, Long.MaxValue)
      (b.resolveERC20Address _).when(ERC20Address(TestAsset.id.take(20))).returning(Some(TestAsset))
    }
    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis())).andThen(_.resultE.explicitGet())

    val LongMaxMinusFee = Long.MaxValue - 200000
    val transfer        = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, LongMaxMinusFee, Waves)
    val assetTransfer   = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, Long.MaxValue, TestAsset)

    (differ(transfer) |+| differ(assetTransfer)).portfolios shouldBe Map(
      senderAddress    -> Portfolio(-Long.MaxValue, assets = Map(TestAsset  -> -Long.MaxValue)),
      recipientAddress -> Portfolio(LongMaxMinusFee, assets = Map(TestAsset -> Long.MaxValue))
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

  it should "not accept value + data" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress

    intercept[RuntimeException](
      EthTxGenerator.signRawTransaction(senderAccount, recipientAddress.chainId)(
        RawTransaction.createTransaction(
          BigInt(System.currentTimeMillis()).bigInteger,
          EthereumTransaction.GasPrice,
          BigInt(100000).bigInteger,
          EthEncoding.toHexString(recipientAddress.publicKeyHash),
          (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
          "0x0000000000"
        )
      )
    ).toString should include(
      "Transaction should have either data or value"
    )
  }

  it should "not accept fee < 100k" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress

    val differ = createBlockchainStub { b =>
      (() => b.height).when().returning(3001)
      b.stub.activateAllFeatures()
      b.stub.creditBalance(senderAccount.toWavesAddress, *)
    }.stub.transactionDiffer().andThen(_.resultE.explicitGet())

    val transaction = EthTxGenerator.signRawTransaction(senderAccount, recipientAddress.chainId)(
      RawTransaction.createTransaction(
        BigInt(System.currentTimeMillis()).bigInteger,
        EthereumTransaction.GasPrice,
        BigInt(99999).bigInteger, // fee
        EthEncoding.toHexString(recipientAddress.publicKeyHash),
        (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
        ""
      )
    )
    intercept[RuntimeException](differ(transaction)).toString should include(
      "Fee for EthereumTransaction (99999 in WAVES) does not exceed minimal value of 100000 WAVES"
    )
  }

  it should "not accept bad time" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress

    val differ = createBlockchainStub { b =>
      (() => b.height).when().returning(3001)
      b.stub.activateAllFeatures()
      b.stub.creditBalance(senderAccount.toWavesAddress, *)
    }.stub.transactionDiffer().andThen(_.resultE.explicitGet())

    val transactionFromFuture = EthTxGenerator.signRawTransaction(senderAccount, recipientAddress.chainId)(
      RawTransaction.createTransaction(
        BigInt(System.currentTimeMillis() + 1.6.hours.toMillis).bigInteger,
        EthereumTransaction.GasPrice,
        BigInt(100000).bigInteger, // fee
        EthEncoding.toHexString(recipientAddress.publicKeyHash),
        (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
        ""
      )
    )
    intercept[RuntimeException](differ(transactionFromFuture)).toString should include(
      "is more than 5400000ms in the future"
    )

    val transactionFromPast = EthTxGenerator.signRawTransaction(senderAccount, recipientAddress.chainId)(
      RawTransaction.createTransaction(
        BigInt(System.currentTimeMillis() - 3.hours.toMillis).bigInteger,
        EthereumTransaction.GasPrice,
        BigInt(100000).bigInteger, // fee
        EthEncoding.toHexString(recipientAddress.publicKeyHash),
        (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
        ""
      )
    )
    intercept[RuntimeException](differ(transactionFromPast)).toString should include(
      "is more than 7200000ms in the past"
    )
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

  it should "return money in transfers asset+waves" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      sh.issueAsset(TestAsset.id)

      val script = TxHelpers.script(
        s"""{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func default() = {
          |  [
          |    ScriptTransfer(i.caller, 123, unit),
          |    ScriptTransfer(i.caller, 123, base58'$TestAsset')
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
      Nil
    )
    val diff = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
    Json.toJson(diff.scriptResults.values.head) should matchJson(s"""{
                                                                   |  "data" : [ ],
                                                                   |  "transfers" : [ {
                                                                   |    "address" : "3G9uRSP4uVjTFjGZixYW4arBZUKWHxjnfeW",
                                                                   |    "asset" : null,
                                                                   |    "amount" : 123
                                                                   |  },
                                                                   |   {
                                                                   |    "address" : "3G9uRSP4uVjTFjGZixYW4arBZUKWHxjnfeW",
                                                                   |    "asset" : "$TestAsset",
                                                                   |    "amount" : 123
                                                                   |  }],
                                                                   |  "issues" : [ ],
                                                                   |  "reissues" : [ ],
                                                                   |  "burns" : [ ],
                                                                   |  "sponsorFees" : [ ],
                                                                   |  "leases" : [ ],
                                                                   |  "leaseCancels" : [ ],
                                                                   |  "invokes" : [ ]
                                                                   |}""".stripMargin)
  }

  it should "test minimum fee" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.SynchronousCalls)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      sh.issueAsset(TestAsset.id)

      val script = TxHelpers.script(
        s"""{-# STDLIB_VERSION 4 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |@Callable (i)
           |func default() = {
           |  [ ]
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
      Nil,
      fee = 499999
    )

    intercept[RuntimeException](differ(transaction).resultE.explicitGet()).toString should include(
      "Fee in WAVES for InvokeScriptTransaction (499999 in WAVES) does not exceed minimal value of 500000 WAVES"
    )
  }
}
