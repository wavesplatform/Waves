package com.wavesplatform.state.diffs

import com.wavesplatform.TestValues
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.ContractLimits.MaxInvokeScriptSizeInBytes
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.RewardsVotingSettings
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.EthereumTransaction.Transfer
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{Asset, EthTxGenerator, EthereumTransaction, TxHelpers}
import com.wavesplatform.utils.{DiffMatchers, EthEncoding, JsonMatchers}
import org.web3j.crypto.{Bip32ECKeyPair, RawTransaction}
import play.api.libs.json.Json

import scala.concurrent.duration.*

class EthereumTransactionDiffTest extends FlatSpec with WithDomain with DiffMatchers with JsonMatchers {

  "Ethereum transaction" should s"allow public keys with leading zeros and shortened byte representation only after ${BlockchainFeatures.ConsensusImprovements.description} activation" in {
    val senderAcc = Bip32ECKeyPair.generateKeyPair("i1".getBytes)
    senderAcc.getPublicKey.toByteArray.length shouldBe <(EthereumKeyLength)

    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(BlockchainFeatures.ConsensusImprovements -> 3),
      Seq(AddrWithBalance(senderAcc.toWavesAddress))
    ) { d =>
      val transfer = EthTxGenerator.generateEthTransfer(senderAcc, senderAcc.toWavesAddress, 1, Waves)
      d.appendAndCatchError(transfer) shouldBe TransactionDiffer.TransactionValidationError(
        GenericError("Invalid public key"),
        transfer
      )
      d.appendBlock()
      d.appendAndAssertSucceed(transfer)
    }

    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(BlockchainFeatures.ConsensusImprovements -> 4),
      Seq(AddrWithBalance(senderAcc.toWavesAddress), AddrWithBalance(TxHelpers.defaultAddress))
    ) { d =>
      val invoke = EthTxGenerator.generateEthInvoke(senderAcc, TxHelpers.defaultAddress, "test", Nil, Nil)

      val dApp = TestCompiler(V6).compileContract("""
                                                    |@Callable(i)
                                                    |func test() = []
                                                    |""".stripMargin)

      d.appendBlock(TxHelpers.setScript(TxHelpers.defaultSigner, dApp))

      d.appendAndCatchError(invoke) shouldBe TransactionDiffer.TransactionValidationError(
        GenericError("Invalid public key"),
        invoke
      )
      d.appendBlock()
      d.appendAndAssertSucceed(invoke)
    }
  }

  "Ethereum transfer" should "work correctly for issued assets" in {
    val senderKp  = TxHelpers.secondSigner.toEthKeyPair
    val recipient = TxHelpers.address(2)
    val issuer    = TxHelpers.signer(3)

    withDomain(DomainPresets.RideV6, Seq(AddrWithBalance(senderKp.toWavesAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, senderKp.toWavesAddress, issue.quantity.value, issue.asset)
      val ethTransfer    = () => EthTxGenerator.generateEthTransfer(senderKp, recipient, 1, issue.asset, 100000)

      d.appendBlock(issue, nativeTransfer)
      // check resolveERC20Address for liquid and solid states
      d.liquidAndSolidAssert { () =>
        d.appendAndAssertSucceed(ethTransfer())
      }
    }
  }

  it should "preserve waves and asset invariant" in {
    val senderKp  = TxHelpers.secondSigner.toEthKeyPair
    val recipient = TxHelpers.address(2)
    val issuer    = TxHelpers.signer(3)

    val fee = TestValues.fee

    withDomain(RideV6.copy(rewardsSettings = RewardsVotingSettings(None)), Seq(AddrWithBalance(senderKp.toWavesAddress))) { d =>
      val wavesTransfer   = EthTxGenerator.generateEthTransfer(senderKp, recipient, 1.waves, Waves, fee)
      val transferPayload = wavesTransfer.payload.asInstanceOf[Transfer]

      d.appendAndAssertSucceed(wavesTransfer)
      assertBalanceInvariant(d.liquidSnapshot, d.rocksDBWriter, 6.waves - wavesTransfer.fee * 3 / 5)
      d.blockchain.balance(recipient) shouldBe transferPayload.amount
      d.blockchain.balance(senderKp.toWavesAddress) shouldBe ENOUGH_AMT - transferPayload.amount - fee
    }

    withDomain(RideV6, Seq(AddrWithBalance(senderKp.toWavesAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, senderKp.toWavesAddress, issue.quantity.value, issue.asset)
      val assetTransfer  = EthTxGenerator.generateEthTransfer(senderKp, recipient, issue.quantity.value, issue.asset, fee)

      d.appendBlock(issue, nativeTransfer)
      d.appendAndAssertSucceed(assetTransfer)
      assertBalanceInvariant(d.liquidSnapshot, d.rocksDBWriter, 6.waves + (issue.fee.value + nativeTransfer.fee.value - assetTransfer.fee) * 3 / 5)
      d.blockchain.balance(recipient) shouldBe 0L
      d.blockchain.balance(recipient, issue.asset) shouldBe issue.quantity.value
      d.blockchain.balance(senderKp.toWavesAddress) shouldBe ENOUGH_AMT - assetTransfer.fee
      d.blockchain.balance(senderKp.toWavesAddress, issue.asset) shouldBe 0L
    }
  }

  it should "be handled with amount + fee > Long.MaxValue" in {
    val senderKp = TxHelpers.secondSigner.toEthKeyPair
    val issuer   = TxHelpers.signer(3)

    withDomain(DomainPresets.mostRecent, Seq(AddrWithBalance(senderKp.toWavesAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer, amount = Long.MaxValue)
      val nativeTransfer = TxHelpers.transfer(issuer, senderKp.toWavesAddress, issue.quantity.value, issue.asset)
      val ethTransfer    = EthTxGenerator.generateEthTransfer(senderKp, TxHelpers.defaultAddress, issue.quantity.value, issue.asset, 100000)

      d.appendBlock(issue, nativeTransfer)
      d.appendAndAssertSucceed(ethTransfer)
    }
  }

  it should "work with long.max" in {
    val sender           = TxHelpers.defaultSigner.toEthKeyPair
    val issuer           = TxHelpers.signer(2)
    val recipientAddress = TxHelpers.secondSigner.toAddress

    withDomain(RideV6, Seq(AddrWithBalance(sender.toWavesAddress, Long.MaxValue), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer, amount = Long.MaxValue)
      val nativeTransfer = TxHelpers.transfer(issuer, sender.toWavesAddress, issue.quantity.value, issue.asset)

      d.appendBlock(issue, nativeTransfer)

      val LongMaxMinusFee = Long.MaxValue - 200000
      val transfer        = EthTxGenerator.generateEthTransfer(sender, recipientAddress, LongMaxMinusFee, Waves)
      val assetTransfer   = EthTxGenerator.generateEthTransfer(sender, recipientAddress, Long.MaxValue, issue.asset)

      d.balance(recipientAddress) shouldBe 0L
      d.balance(recipientAddress, issue.asset) shouldBe 0L
      d.balance(sender.toWavesAddress) shouldBe Long.MaxValue
      d.balance(sender.toWavesAddress, issue.asset) shouldBe Long.MaxValue

      d.appendBlock(transfer, assetTransfer)

      d.balance(recipientAddress) shouldBe LongMaxMinusFee
      d.balance(recipientAddress, issue.asset) shouldBe Long.MaxValue
      d.balance(sender.toWavesAddress) shouldBe 0L
      d.balance(sender.toWavesAddress, issue.asset) shouldBe 0L
    }
  }

  it should "prevent waves balance overflow" in {
    val sender           = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress

    withDomain(RideV6, Seq(AddrWithBalance(sender.toWavesAddress, Long.MaxValue), AddrWithBalance(recipientAddress, 100001))) { d =>
      val LongMaxMinusFee = Long.MaxValue - 100000
      val transfer        = EthTxGenerator.generateEthTransfer(sender, recipientAddress, LongMaxMinusFee, Waves)

      d.balance(recipientAddress) shouldBe 100001L
      d.balance(sender.toWavesAddress) shouldBe Long.MaxValue

      d.transactionDiffer(transfer).resultE should produce(s"$recipientAddress -> Waves balance sum overflow")
    }
  }

  it should "use chainId in signer key recovery" in {
    val sender           = TxHelpers.defaultSigner.toEthKeyPair
    val issuer           = TxHelpers.signer(2)
    val recipientAddress = TxHelpers.secondSigner.toAddress('W'.toByte) // Other network

    withDomain(RideV6, Seq(AddrWithBalance(sender.toWavesAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, sender.toWavesAddress, issue.quantity.value, issue.asset)

      d.appendBlock(issue, nativeTransfer)

      val transfer      = EthTxGenerator.generateEthTransfer(sender, recipientAddress, 1, Waves)
      val assetTransfer = EthTxGenerator.generateEthTransfer(sender, recipientAddress, 1, issue.asset)

      d.transactionDiffer(transfer).resultE should produce("Address belongs to another network")
      d.transactionDiffer(assetTransfer).resultE should produce("Address belongs to another network")
    }
  }

  it should "not accept fee < 100k" in {
    val sender           = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress

    withDomain(RideV6, Seq(AddrWithBalance(sender.toWavesAddress))) { d =>
      val transaction = EthTxGenerator.signRawTransaction(sender, recipientAddress.chainId)(
        RawTransaction.createTransaction(
          BigInt(System.currentTimeMillis()).bigInteger,
          EthereumTransaction.GasPrice,
          BigInt(99999).bigInteger, // fee
          EthEncoding.toHexString(recipientAddress.publicKeyHash),
          (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
          ""
        )
      )

      d.transactionDiffer(transaction).resultE should produce(
        "Fee for EthereumTransaction (99999 in WAVES) does not exceed minimal value of 100000 WAVES"
      )
    }
  }

  it should "not accept bad time" in {
    val sender           = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress

    withDomain(RideV6, Seq(AddrWithBalance(sender.toWavesAddress))) { d =>
      val transactionFromFuture = EthTxGenerator.signRawTransaction(sender, recipientAddress.chainId)(
        RawTransaction.createTransaction(
          BigInt(System.currentTimeMillis() + 1.6.hours.toMillis).bigInteger,
          EthereumTransaction.GasPrice,
          BigInt(100000).bigInteger, // fee
          EthEncoding.toHexString(recipientAddress.publicKeyHash),
          (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
          ""
        )
      )
      val transactionFromPast = EthTxGenerator.signRawTransaction(sender, recipientAddress.chainId)(
        RawTransaction.createTransaction(
          BigInt(System.currentTimeMillis() - 3.hours.toMillis).bigInteger,
          EthereumTransaction.GasPrice,
          BigInt(100000).bigInteger, // fee
          EthEncoding.toHexString(recipientAddress.publicKeyHash),
          (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
          ""
        )
      )

      d.transactionDiffer(transactionFromFuture).resultE should produce("is more than 5400000ms in the future")
      d.transactionDiffer(transactionFromPast).resultE should produce("is more than 7200000ms in the past")
    }
  }

  it should "not be accepted before RideV6 activation" in {
    val sender           = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.address(1)

    withDomain(RideV5, Seq(AddrWithBalance(sender.toWavesAddress))) { d =>
      val transfer = EthTxGenerator.generateEthTransfer(sender, recipientAddress, 123, Waves)

      d.transactionDiffer(transfer).resultE should produceRejectOrFailedDiff(
        s"${BlockchainFeatures.RideV6.description} feature has not been activated yet"
      )
    }
  }

  "Ethereum invoke" should "work with all types of arguments except unions" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner
    val issuer  = TxHelpers.signer(2)

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

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, invoker.toWavesAddress, issue.quantity.value, issue.asset)
      val setScript      = TxHelpers.setScript(dApp, script)

      d.appendBlock(issue, nativeTransfer, setScript)

      val invoke = EthTxGenerator.generateEthInvoke(
        invoker,
        dApp.toAddress,
        "deposit",
        Seq(
          Arg.Integer(123),
          Arg.Bytes(ByteStr.empty),
          Arg.Str("123"),
          Arg.Bool(true),
          Arg.List(Arg.Integer(0), Seq(Arg.Integer(123)))
        ),
        Seq(Payment(321, issue.asset))
      )

      val snapshot = d.transactionDiffer(invoke).resultE.explicitGet()
      snapshot should containAppliedTx(invoke.id())
      Json.toJson(snapshot.scriptResults.values.head) should matchJson("""{
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
  }

  it should "not work with union type" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner
    val issuer  = TxHelpers.signer(2)

    val script = TxHelpers.script(
      """{-# STDLIB_VERSION 4 #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |@Callable (i)
        |func test(union: String|Int) = []
        |""".stripMargin
    )

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, invoker.toWavesAddress, issue.quantity.value, issue.asset)
      val setScript      = TxHelpers.setScript(dApp, script)

      d.appendBlock(issue, nativeTransfer, setScript)

      val invoke = EthTxGenerator.generateEthInvoke(
        invoker,
        dApp.toAddress,
        "test",
        Seq(
          Arg.Integer(123)
        ),
        Seq(Payment(321, issue.asset))
      )

      d.transactionDiffer(invoke).resultE should produce("Function not defined: 1f9773e9")
    }
  }

  it should "work with no arguments" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner
    val issuer  = TxHelpers.signer(2)

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

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, invoker.toWavesAddress, issue.quantity.value, issue.asset)
      val setScript      = TxHelpers.setScript(dApp, script)

      d.appendBlock(issue, nativeTransfer, setScript)

      val invoke = EthTxGenerator.generateEthInvoke(
        invoker,
        dApp.toAddress,
        "deposit",
        Seq(),
        Seq(Payment(321, issue.asset))
      )
      val snapshot = d.transactionDiffer(invoke).resultE.explicitGet()
      snapshot should containAppliedTx(invoke.id())
      Json.toJson(snapshot.scriptResults.values.head) should matchJson("""{
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
  }

  it should "work with no payments" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner

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

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress))) { d =>
      val setScript = TxHelpers.setScript(dApp, script)

      d.appendBlock(setScript)

      val invoke   = EthTxGenerator.generateEthInvoke(invoker, dApp.toAddress, "deposit", Seq(), Seq())
      val snapshot = d.transactionDiffer(invoke).resultE.explicitGet()
      snapshot should containAppliedTx(invoke.id())
      Json.toJson(snapshot.scriptResults.values.head) should matchJson(
        """ {
          |   "data" : [ ],
          |   "transfers" : [ {
          |     "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
          |     "asset" : null,
          |     "amount" : 123
          |   } ],
          |   "issues" : [ ],
          |   "reissues" : [ ],
          |   "burns" : [ ],
          |   "sponsorFees" : [ ],
          |   "leases" : [ ],
          |   "leaseCancels" : [ ],
          |   "invokes" : [ ]
          | }
        """.stripMargin
      )
    }
  }

  it should "fail with max+1 payments" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner

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

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress))) { d =>
      val setScript = TxHelpers.setScript(dApp, script)

      d.appendBlock(setScript)

      val invoke = EthTxGenerator.generateEthInvoke(
        invoker,
        dApp.toAddress,
        "deposit",
        Seq(),
        (1 to com.wavesplatform.lang.v1.ContractLimits.MaxAttachedPaymentAmountV5 + 1).map(InvokeScriptTransaction.Payment(_, Waves))
      )
      d.transactionDiffer(invoke).resultE should produceRejectOrFailedDiff("Script payment amount=11 should not exceed 10")
    }
  }

  it should "work with default function" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner
    val issuer  = TxHelpers.signer(2)

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

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, invoker.toWavesAddress, issue.quantity.value, issue.asset)
      val setScript      = TxHelpers.setScript(dApp, script)

      d.appendBlock(issue, nativeTransfer, setScript)

      val invoke = EthTxGenerator.generateEthInvoke(
        invoker,
        dApp.toAddress,
        "default",
        Seq(),
        Seq(Payment(321, issue.asset))
      )
      val snapshot = d.transactionDiffer(invoke).resultE.explicitGet()
      snapshot should containAppliedTx(invoke.id())
      Json.toJson(snapshot.scriptResults.values.head) should matchJson("""{
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
  }

  it should "return money in transfers asset+waves" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner
    val issuer  = TxHelpers.signer(2)

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress), AddrWithBalance(issuer.toAddress))) { d =>
      val issue          = TxHelpers.issue(issuer)
      val nativeTransfer = TxHelpers.transfer(issuer, dApp.toAddress, issue.quantity.value, issue.asset)

      val script = TxHelpers.script(
        s"""{-# STDLIB_VERSION 4 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |@Callable (i)
           |func default() = {
           |  [
           |    ScriptTransfer(i.caller, 123, unit),
           |    ScriptTransfer(i.caller, 123, base58'${issue.asset}')
           |  ]
           |}
           |""".stripMargin
      )
      val setScript = TxHelpers.setScript(dApp, script)

      d.appendBlock(issue, nativeTransfer, setScript)

      val invoke = EthTxGenerator.generateEthInvoke(
        invoker,
        dApp.toAddress,
        "default",
        Seq(),
        Nil
      )
      val snapshot = d.transactionDiffer(invoke).resultE.explicitGet()
      snapshot should containAppliedTx(invoke.id())
      Json.toJson(snapshot.scriptResults.values.head) should matchJson(s"""{
                                                                          |  "data" : [ ],
                                                                          |  "transfers" : [ {
                                                                          |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
                                                                          |    "asset" : null,
                                                                          |    "amount" : 123
                                                                          |  },
                                                                          |   {
                                                                          |    "address" : "3NByUD1YE9SQPzmf2KqVqrjGMutNSfc4oBC",
                                                                          |    "asset" : "${issue.asset}",
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
  }

  it should "test minimum fee" in {
    val invoker = TxHelpers.defaultSigner.toEthKeyPair
    val dApp    = TxHelpers.secondSigner

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

    withDomain(RideV6, Seq(AddrWithBalance(invoker.toWavesAddress), AddrWithBalance(dApp.toAddress))) { d =>
      val setScript = TxHelpers.setScript(dApp, script)

      d.appendBlock(setScript)

      val invoke = EthTxGenerator.generateEthInvoke(
        invoker,
        dApp.toAddress,
        "default",
        Seq(),
        Nil,
        fee = 499999
      )

      d.transactionDiffer(invoke).resultE should produce(
        "Fee for EthereumTransaction (499999 in WAVES) does not exceed minimal value of 500000 WAVES"
      )
    }
  }

  "Ethereum Transaction" should s"be checked for max allowed size after ${BlockchainFeatures.BlockRewardDistribution} activation" in {
    val dApp      = TxHelpers.signer(1)
    val ethSigner = TxHelpers.signer(2).toEthKeyPair
    val issuer    = TxHelpers.signer(3)

    def invokeWithSize(size: Int): EthereumTransaction = {
      // 46 is for the dApp address and PB fields data
      EthTxGenerator.generateEthInvoke(ethSigner, dApp.toAddress, "foo", Seq(Arg.Str("1" * (size - 46))), Seq.empty)
    }

    def invoke(withRedundantBytes: Boolean): EthereumTransaction =
      EthTxGenerator.generateEthInvoke(ethSigner, dApp.toAddress, "foo", Seq(Arg.Str("1")), Seq.empty, withRedundantBytes = withRedundantBytes)

    def transfer(asset: Asset, withRedundantBytes: Boolean): EthereumTransaction =
      EthTxGenerator.generateEthTransfer(ethSigner, issuer.toAddress, 1, asset, withRedundantBytes = withRedundantBytes)

    withDomain(
      ConsensusImprovements.setFeaturesHeight(BlockchainFeatures.BlockRewardDistribution -> 4),
      Seq(AddrWithBalance(dApp.toAddress), AddrWithBalance(ethSigner.toWavesAddress), AddrWithBalance(issuer.toAddress))
    ) { d =>
      val script = TestCompiler(V6).compileContract(
        """
          |@Callable(i)
          |func foo(s: String) = []
          |""".stripMargin
      )
      val issue = TxHelpers.issue(issuer)
      d.appendBlock(TxHelpers.setScript(dApp, script), issue, TxHelpers.transfer(issuer, ethSigner.toWavesAddress, 10, issue.asset))
      d.appendAndAssertSucceed(
        invokeWithSize(MaxInvokeScriptSizeInBytes),
        invokeWithSize(MaxInvokeScriptSizeInBytes + 1),
        invoke(false),
        invoke(true),
        transfer(issue.asset, withRedundantBytes = false),
        transfer(issue.asset, withRedundantBytes = true)
      )

      d.blockchain.isFeatureActivated(BlockchainFeatures.BlockRewardDistribution, d.blockchain.height + 1) shouldBe true
      // activation height
      val bigSizeInvoke = invokeWithSize(MaxInvokeScriptSizeInBytes + 1)
      d.appendAndCatchError(bigSizeInvoke) shouldBe TransactionValidationError(
        GenericError(s"Ethereum Invoke bytes length exceeds limit = $MaxInvokeScriptSizeInBytes"),
        bigSizeInvoke
      )
      val redundantBytesInvoke = invoke(true)
      d.appendAndCatchError(redundantBytesInvoke) shouldBe TransactionValidationError(
        GenericError("Redundant bytes were found in Ethereum Invoke"),
        redundantBytesInvoke
      )
      val redundantBytesTransfer = transfer(issue.asset, withRedundantBytes = true)
      d.appendAndCatchError(redundantBytesTransfer) shouldBe TransactionValidationError(
        GenericError("Invalid asset data size for Ethereum Transfer"),
        redundantBytesTransfer
      )
      d.appendAndAssertSucceed(invokeWithSize(MaxInvokeScriptSizeInBytes), invoke(false), transfer(issue.asset, withRedundantBytes = false))
    }
  }
}
