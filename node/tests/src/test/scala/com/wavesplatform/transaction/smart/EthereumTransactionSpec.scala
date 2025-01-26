package com.wavesplatform.transaction.smart

import com.wavesplatform.TestValues
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.test.{FlatSpec, produce}
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, TxHelpers}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.utils.{EthEncoding, EthHelpers, JsonMatchers}
import org.scalatest.Inside
import org.scalatest.ParallelTestExecution
import org.web3j.crypto.*
import play.api.libs.json.*

class EthereumTransactionSpec extends FlatSpec with EthHelpers with JsonMatchers with WithDomain with ParallelTestExecution with Inside {

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

  it should "work with Long.MaxValue when transferring waves" in {
    val sender    = TxHelpers.signer(1).toEthKeyPair
    val recipient = TxHelpers.signer(2)
    val balances  = Seq(AddrWithBalance(sender.toWavesAddress, Long.MaxValue))

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Assert before transfer
      d.blockchain.balance(sender.toWavesAddress) shouldEqual Long.MaxValue
      d.blockchain.balance(recipient.toAddress) shouldEqual 0L

      // Transfer
      val ethTxFee        = 0.001.waves
      val longMaxMinusFee = Long.MaxValue - ethTxFee
      val transfer        = EthTxGenerator.generateEthTransfer(sender, recipient.toAddress, longMaxMinusFee, Waves)
      d.appendBlock(transfer)

      // Assert after transfer
      d.blockchain.balance(sender.toWavesAddress) shouldEqual 0L
      d.blockchain.balance(recipient.toAddress) shouldEqual longMaxMinusFee
    }
  }

  it should "work with Long.MaxValue when transferring other assets" in {
    val assetIssuer = TxHelpers.defaultSigner
    val sender      = TxHelpers.signer(1).toEthKeyPair
    val recipient   = TxHelpers.signer(2)
    val balances    = Seq(AddrWithBalance(sender.toWavesAddress, 1.waves))

    withDomain(DomainPresets.RideV6, balances) { d =>
      // Issue an asset
      val issueTx   = TxHelpers.issue(assetIssuer, Long.MaxValue)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      // Transfer asset to sender
      // Note: In order to check eth transfer, we must perform waves transfer first.
      // The reason is that ethereum-compatible account
      // can not do waves transactions (including issue transaction) and vice versa.
      d.appendBlock(TxHelpers.transfer(assetIssuer, sender.toWavesAddress, Long.MaxValue, testAsset))

      // Assert before transfer
      d.portfolio(sender.toWavesAddress) shouldEqual Seq((testAsset, Long.MaxValue))
      d.portfolio(recipient.toAddress) shouldEqual Seq()

      // Transfer
      val transfer = EthTxGenerator.generateEthTransfer(sender, recipient.toAddress, Long.MaxValue, testAsset)
      d.appendBlock(transfer)

      // Assert after transfer
      d.portfolio(sender.toWavesAddress) shouldEqual Seq()
      d.portfolio(recipient.toAddress) shouldEqual Seq((testAsset, Long.MaxValue))
    }
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
    val assetIssuer    = TxHelpers.defaultSigner
    val invokerAccount = TxHelpers.signer(1).toEthKeyPair
    val dAppAccount    = TxHelpers.signer(2)
    val balances = Seq(
      AddrWithBalance(assetIssuer.toAddress, 1.waves),
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
      val issueTx   = TxHelpers.issue(assetIssuer, 1000)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      d.appendBlock(TxHelpers.transfer(assetIssuer, invokerAccount.toWavesAddress, 1000, testAsset))

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
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

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
        Seq(Payment(321, testAsset))
      )

      val snapshot = d.createDiff(transaction)
      d.appendBlock(transaction)

      val expectedScriptResults = """{
                                    |  "data" : [ ],
                                    |  "transfers" : [ {
                                    |    "address" : "3MzPAf9BvP5kpV9A6yas2svwzLxBb3pHBHs",
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
                                    |}""".stripMargin

      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
      Json.toJson(snapshot.scriptResults.values.head) should matchJson(expectedScriptResults)
    }
  }

  it should "not work with union type" in {
    val assetIssuer    = TxHelpers.defaultSigner
    val invokerAccount = TxHelpers.signer(1).toEthKeyPair
    val dAppAccount    = TxHelpers.signer(2)
    val balances = Seq(
      AddrWithBalance(assetIssuer.toAddress, 1.waves),
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
      val issueTx   = TxHelpers.issue(assetIssuer, 1000)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      d.appendBlock(TxHelpers.transfer(assetIssuer, invokerAccount.toWavesAddress, 1000, testAsset))

      val script = TxHelpers.script(
        """{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable (i)
          |func test(union: String|Int) = []
          |""".stripMargin
      )
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

      val transaction = EthTxGenerator.generateEthInvoke(
        invokerAccount,
        dAppAccount.toAddress,
        "test",
        Seq(Arg.Integer(123)),
        Seq(Payment(321, testAsset))
      )

      val snapshot = d.createDiffE(transaction)
      snapshot should produce("Function not defined: 1f9773e9")
    }
  }

  it should "work with no arguments" in {
    val assetIssuer    = TxHelpers.defaultSigner
    val invokerAccount = TxHelpers.signer(1).toEthKeyPair
    val dAppAccount    = TxHelpers.signer(2)
    val balances = Seq(
      AddrWithBalance(assetIssuer.toAddress, 1.waves),
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
      val issueTx   = TxHelpers.issue(assetIssuer, 1000)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      d.appendBlock(TxHelpers.transfer(assetIssuer, invokerAccount.toWavesAddress, 1000, testAsset))

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
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

      val transaction = EthTxGenerator.generateEthInvoke(
        invokerAccount,
        dAppAccount.toAddress,
        "deposit",
        Seq(),
        Seq(Payment(321, testAsset))
      )
      val snapshot = d.createDiff(transaction)
      d.appendBlock(transaction)

      val expectedScriptResults = """{
                                    |  "data" : [ ],
                                    |  "transfers" : [ {
                                    |    "address" : "3MzPAf9BvP5kpV9A6yas2svwzLxBb3pHBHs",
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
                                    |}""".stripMargin

      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
      Json.toJson(snapshot.scriptResults.values.head) should matchJson(expectedScriptResults)
    }
  }

  it should "work with no payments" in {
    val assetIssuer    = TxHelpers.defaultSigner
    val invokerAccount = TxHelpers.signer(1).toEthKeyPair
    val dAppAccount    = TxHelpers.signer(2)
    val balances = Seq(
      AddrWithBalance(assetIssuer.toAddress, 1.waves),
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
      val issueTx   = TxHelpers.issue(assetIssuer, 1000)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      d.appendBlock(TxHelpers.transfer(assetIssuer, invokerAccount.toWavesAddress, 1000, testAsset))

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
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

      val transaction = EthTxGenerator.generateEthInvoke(
        invokerAccount,
        dAppAccount.toAddress,
        "deposit",
        Seq(),
        Seq()
      )
      val snapshot = d.createDiff(transaction)
      d.appendBlock(transaction)

      val expectedScriptResults = """{
                                    |  "data" : [ ],
                                    |  "transfers" : [ {
                                    |    "address" : "3MzPAf9BvP5kpV9A6yas2svwzLxBb3pHBHs",
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
                                    |}""".stripMargin

      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
      Json.toJson(snapshot.scriptResults.values.head) should matchJson(expectedScriptResults)
    }
  }

  it should "fail with max+1 payments" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val balances = Seq(
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
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
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

      val transaction = EthTxGenerator.generateEthInvoke(
        invokerAccount,
        dAppAccount.toAddress,
        "deposit",
        Seq(),
        (1 to com.wavesplatform.lang.v1.ContractLimits.MaxAttachedPaymentAmountV5 + 1).map(InvokeScriptTransaction.Payment(_, Waves))
      )
      d.createDiffE(transaction) should produceRejectOrFailedDiff("Script payment amount=11 should not exceed 10")
    }
  }

  it should "work with default function" in {
    val assetIssuer    = TxHelpers.defaultSigner
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val balances = Seq(
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
      val issueTx   = TxHelpers.issue(assetIssuer, 1000)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      d.appendBlock(TxHelpers.transfer(assetIssuer, invokerAccount.toWavesAddress, 1000, testAsset))

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
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

      val transaction = EthTxGenerator.generateEthInvoke(
        invokerAccount,
        dAppAccount.toAddress,
        "default",
        Seq(),
        Seq(Payment(321, testAsset))
      )
      val snapshot = d.createDiff(transaction)
      d.appendBlock(transaction)

      val expectedScriptResults = """{
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
                                    |}""".stripMargin
      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
      Json.toJson(snapshot.scriptResults.values.head) should matchJson(expectedScriptResults)
    }
  }

  it should "return money in transfers asset+waves" in {
    val assetIssuer    = TxHelpers.defaultSigner
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val balances = Seq(
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
      val issueTx   = TxHelpers.issue(assetIssuer, 1000)
      val testAsset = issueTx.asset
      d.appendBlock(issueTx)

      d.appendBlock(TxHelpers.transfer(assetIssuer, dAppAccount.toAddress, 1000, testAsset))

      val script = TxHelpers.script(
        s"""{-# STDLIB_VERSION 4 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |@Callable (i)
           |func default() = {
           |  [
           |    ScriptTransfer(i.caller, 123, unit),
           |    ScriptTransfer(i.caller, 123, base58'$testAsset')
           |  ]
           |}
           |""".stripMargin
      )
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

      val transaction = EthTxGenerator.generateEthInvoke(
        invokerAccount,
        dAppAccount.toAddress,
        "default",
        Seq(),
        Nil
      )
      val snapshot = d.createDiff(transaction)
      d.appendBlock(transaction)

      val expectedScriptResults = s"""{
                                     |  "data" : [ ],
                                     |  "transfers" : [ {
                                     |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
                                     |    "asset" : null,
                                     |    "amount" : 123
                                     |  },
                                     |   {
                                     |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
                                     |    "asset" : "$testAsset",
                                     |    "amount" : 123
                                     |  }],
                                     |  "issues" : [ ],
                                     |  "reissues" : [ ],
                                     |  "burns" : [ ],
                                     |  "sponsorFees" : [ ],
                                     |  "leases" : [ ],
                                     |  "leaseCancels" : [ ],
                                     |  "invokes" : [ ]
                                     |}""".stripMargin
      d.blockchain.transactionMeta(transaction.id()).map(_.status == Status.Succeeded) shouldBe Some(true)
      Json.toJson(snapshot.scriptResults.values.head) should matchJson(expectedScriptResults)
    }
  }

  it should "test minimum fee" in {
    val invokerAccount = TxHelpers.defaultSigner.toEthKeyPair
    val dAppAccount    = TxHelpers.secondSigner
    val balances = Seq(
      AddrWithBalance(invokerAccount.toWavesAddress, 1.waves),
      AddrWithBalance(dAppAccount.toAddress, 1.waves)
    )
    withDomain(DomainPresets.RideV6, balances) { d =>
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
      d.appendBlock(TxHelpers.setScript(dAppAccount, script))

      val transaction = EthTxGenerator.generateEthInvoke(
        invokerAccount,
        dAppAccount.toAddress,
        "default",
        Seq(),
        Nil,
        fee = 499999
      )

      val snapshot = d.createDiffE(transaction)

      snapshot should produce("Fee for EthereumTransaction (499999 in WAVES) does not exceed minimal value of 500000 WAVES.")
    }
  }
}
