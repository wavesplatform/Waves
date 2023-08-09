package com.wavesplatform.transaction.smart

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.{FlatSpec, TestTime, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{ERC20Address, EthTxGenerator, EthereumTransaction, TxHelpers}
import com.wavesplatform.utils.{DiffMatchers, EthEncoding, EthHelpers, JsonMatchers}
import com.wavesplatform.{BlockchainStubHelpers, TestValues}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, Inside}
import org.web3j.crypto.*
import play.api.libs.json.Json

class EthereumTransactionSpec
    extends FlatSpec
    with BeforeAndAfterAll
    with PathMockFactory
    with BlockchainStubHelpers
    with EthHelpers
    with DiffMatchers
    with JsonMatchers
    with Inside {

  val TestAsset: IssuedAsset = TestValues.asset

  "Ethereum transfer" should "recover correct key" in {
    val senderAccount = TxHelpers.defaultSigner.toEthKeyPair
    val senderAddress = TxHelpers.defaultSigner.toEthWavesAddress
    val transaction   = EthTxGenerator.generateEthTransfer(senderAccount, senderAddress, 1, Waves)
    transaction.senderAddress() shouldBe senderAccount.toWavesAddress
  }

  it should "recover correct key with leading zeros" in {
    val senderAcc = Bip32ECKeyPair.create(
      EthEncoding.toBytes("0x00db4a036ea48572bf27630c72a1513f48f0b4a6316606fd01c23318befdf984"),
      Array.emptyByteArray
    )
    val tx = EthTxGenerator.generateEthTransfer(senderAcc, senderAcc.toWavesAddress, 1, Waves)
    EthEncoding.toHexString(
      tx.signerPublicKey().arr
    ) shouldBe "0x00d7cf9ff594b07273228e7dd591707d38a1dba0a39492fd64445ba9cbb3bf66c862b9752f02bf8d1a0f00ccb11ae550a7616bd965c10f0101202d75580786ee"
  }

  it should "recover correct address chainId" in {
    val transfer      = EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 1, Waves)
    val assetTransfer = EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 1, TestValues.asset)
    val invoke        = EthTxGenerator.generateEthInvoke(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, "test", Nil, Nil)

    inside(EthereumTransaction(transfer.toSignedRawTransaction).explicitGet().payload) { case t: EthereumTransaction.Transfer =>
      t.recipient.chainId shouldBe 'T'.toByte
    }

    inside(EthereumTransaction(assetTransfer.toSignedRawTransaction).explicitGet().payload) { case t: EthereumTransaction.Transfer =>
      t.recipient.chainId shouldBe 'T'.toByte
    }

    inside(EthereumTransaction(invoke.toSignedRawTransaction).explicitGet().payload) { case t: EthereumTransaction.Invocation =>
      t.dApp.chainId shouldBe 'T'.toByte
    }
  }

  it should "change id if signature is changed" in {
    val senderAccount = TxHelpers.defaultSigner.toEthKeyPair
    val secondAccount = TxHelpers.secondSigner.toEthKeyPair
    val transaction1  = EthTxGenerator.generateEthTransfer(senderAccount, TxHelpers.defaultAddress, 1, Waves)
    val transaction2  = EthTxGenerator.signRawTransaction(secondAccount, AddressScheme.current.chainId)(transaction1.underlying)
    transaction1.id() shouldNot be(transaction2.id())
  }

  it should "reject legacy transactions" in {
    val senderAccount     = TxHelpers.defaultEthSigner
    val eip155Transaction = EthTxGenerator.generateEthTransfer(senderAccount, TxHelpers.defaultAddress, 1, Waves)

    val legacyTransaction =
      new SignedRawTransaction(
        eip155Transaction.underlying.getTransaction,
        Sign.signMessage(TransactionEncoder.encode(eip155Transaction.underlying, 1.toLong), senderAccount, true)
      )
    EthereumTransaction(legacyTransaction) should produce("Legacy transactions are not supported")
  }

  it should "work with long.max" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val senderAddress    = TxHelpers.defaultSigner.toEthWavesAddress
    val recipientAddress = TxHelpers.secondSigner.toAddress

    val blockchain = createBlockchainStub { b =>
      b.stub.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6, BlockchainFeatures.Ride4DApps)
      b.stub.issueAsset(TestAsset.id)
      b.stub.creditBalance(senderAddress, Waves, Long.MaxValue)
      b.stub.creditBalance(senderAddress, TestAsset, Long.MaxValue)
      (b.wavesBalances _).when(*).returns(Map(senderAddress -> Long.MaxValue))
      (b.resolveERC20Address _).when(ERC20Address(TestAsset.id.take(20))).returning(Some(TestAsset))
    }
    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis())).andThen(_.resultE.explicitGet())

    val LongMaxMinusFee = Long.MaxValue - 200000
    val transfer        = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, LongMaxMinusFee, Waves)
    val assetTransfer   = EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, Long.MaxValue, TestAsset)

    differ(transfer).combineF(differ(assetTransfer)).explicitGet().portfolios shouldBe Map(
      senderAddress    -> Portfolio.build(-Long.MaxValue, TestAsset, -Long.MaxValue),
      recipientAddress -> Portfolio.build(LongMaxMinusFee, TestAsset, Long.MaxValue)
    )
  }

  it should "fail with empty to field" in {
    val rawTransaction = RawTransaction.createTransaction(
      BigInt(System.currentTimeMillis()).bigInteger,
      EthereumTransaction.GasPrice,
      BigInt(100000).bigInteger, // fee
      "",                        // empty "to"
      (BigInt(1) * EthereumTransaction.AmountMultiplier).bigInteger,
      ""
    )
    a[RuntimeException] should be thrownBy EthTxGenerator.signRawTransaction(TxHelpers.defaultEthSigner, TxHelpers.defaultAddress.chainId)(
      rawTransaction
    )
  }

  it should "fail with invalid to field" in {
    val rawTransaction = RawTransaction.createTransaction(
      BigInt(System.currentTimeMillis()).bigInteger,
      EthereumTransaction.GasPrice,
      BigInt(100000).bigInteger, // fee
      "0xffffffff",              // invalid "to"
      (BigInt(1) * EthereumTransaction.AmountMultiplier).bigInteger,
      ""
    )
    a[RuntimeException] should be thrownBy EthTxGenerator.signRawTransaction(TxHelpers.defaultEthSigner, TxHelpers.defaultAddress.chainId)(
      rawTransaction
    )
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

  "Ethereum invoke" should "recover correct key" in {
    val senderAccount = TxHelpers.defaultSigner.toEthKeyPair
    val senderAddress = TxHelpers.defaultSigner.toEthWavesAddress
    val transaction   = EthTxGenerator.generateEthInvoke(senderAccount, senderAddress, "test", Nil, Nil)
    transaction.senderAddress() shouldBe senderAccount.toWavesAddress
  }

  it should "recover correct key with leading zeros" in {
    val senderAcc = Bip32ECKeyPair.create(
      EthEncoding.toBytes("0x00db4a036ea48572bf27630c72a1513f48f0b4a6316606fd01c23318befdf984"),
      Array.emptyByteArray
    )
    val tx = EthTxGenerator.generateEthInvoke(senderAcc, senderAcc.toWavesAddress, "test", Nil, Nil)
    EthEncoding.toHexString(
      tx.signerPublicKey().arr
    ) shouldBe "0x00d7cf9ff594b07273228e7dd591707d38a1dba0a39492fd64445ba9cbb3bf66c862b9752f02bf8d1a0f00ccb11ae550a7616bd965c10f0101202d75580786ee"
  }

  it should "work with all types of arguments except unions" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func deposit(amount: Int, bs: ByteVector, str: String, bool: Boolean, list: List[Int]) = {
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
        Arg.List(Arg.Integer(0), Seq(Arg.Integer(123)))
      ),
      Seq(Payment(321, IssuedAsset(ByteStr(EthStubBytes32))))
    )
    val diff = differ(transaction).resultE.explicitGet()
    diff should containAppliedTx(transaction.id())
    Json.toJson(diff.scriptResults.values.head) should matchJson("""{
                                                                   |  "data" : [ ],
                                                                   |  "transfers" : [ {
                                                                   |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
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

  it should "not work with union type" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
      sh.issueAsset(ByteStr(EthStubBytes32))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func test(union: String|Int) = []
          |""".stripMargin
      )
      sh.setScript(dAppAccount.toAddress, script)
    }

    val differ = blockchain.stub.transactionDiffer(TestTime(System.currentTimeMillis()))
    val transaction = EthTxGenerator.generateEthInvoke(
      invokerAccount,
      dAppAccount.toAddress,
      "test",
      Seq(
        Arg.Integer(123)
      ),
      Seq(Payment(321, IssuedAsset(ByteStr(EthStubBytes32))))
    )

    val diff = differ(transaction).resultE
    diff should produce("Function not defined: 1f9773e9")
  }

  it should "work with no arguments" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
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
                                                                   |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
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
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
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
                                                                   |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
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
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
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
    differ(transaction).resultE should produceRejectOrFailedDiff("Script payment amount=11 should not exceed 10")
  }

  it should "work with default function" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val blockchain = createBlockchainStub { blockchain =>
      val sh = StubHelpers(blockchain)
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
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
                                                                   |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
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
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
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
                                                                    |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
                                                                    |    "asset" : null,
                                                                    |    "amount" : 123
                                                                    |  },
                                                                    |   {
                                                                    |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
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
      sh.activateFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.RideV6)
      sh.creditBalance(invokerAccount.toWavesAddress, *)
      sh.creditBalance(dAppAccount.toAddress, *)
      (blockchain.wavesBalances _)
        .when(*)
        .returns(Map(invokerAccount.toWavesAddress -> Long.MaxValue / 3, dAppAccount.toAddress -> Long.MaxValue / 3))
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
