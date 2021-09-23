package com.wavesplatform.transaction.smart

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.transaction.TxHelpers
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
      'E'.toByte,
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
      'E'.toByte,
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

    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis()))
    val transaction = EthTxGenerator.generateEthInvoke(
      invokerAccount,
      dAppAccount.toAddress,
      'E'.toByte,
      "deposit",
      Seq(),
      Seq()
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
      'E'.toByte,
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
