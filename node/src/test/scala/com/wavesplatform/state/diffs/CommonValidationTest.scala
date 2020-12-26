package com.wavesplatform.state.diffs

import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, GenesisTransaction, PaymentTransaction, Proofs, Transaction, TxVersion}
import com.wavesplatform.utils._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class CommonValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with WithState with NoShrink {

  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, TransferTransaction)] = for {
      master    <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      transfer: TransferTransaction <- wavesTransferGeneratorP(master, recipient.toAddress)
    } yield (genesis, transfer)

    forAll(preconditionsAndPayment) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, transfer))), TestBlock.create(Seq(transfer))) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }

        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer, transfer))) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }
    }
  }

  private def sponsoredTransactionsCheckFeeTest(feeInAssets: Boolean, feeAmount: Long)(f: Either[ValidationError, Unit] => Any): Unit = {
    val settings = createSettings(BlockchainFeatures.FeeSponsorship -> 0)
    val gen      = sponsorAndSetScriptGen(sponsorship = true, smartToken = false, smartAccount = false, feeInAssets, feeAmount)
    forAll(gen) {
      case (genesisBlock, transferTx) =>
        withLevelDBWriter(settings) { blockchain =>
          val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _) =
            BlockDiffer.fromBlock(blockchain, None, genesisBlock, MiningConstraint.Unlimited).explicitGet()
          blockchain.append(preconditionDiff, preconditionFees, totalFee, None, genesisBlock.header.generationSignature, genesisBlock)

          f(FeeValidation(blockchain, transferTx))
        }
    }
  }

  property("checkFee for sponsored transactions sunny") {
    sponsoredTransactionsCheckFeeTest(feeInAssets = true, feeAmount = 10)(_.explicitGet())
  }

  property("checkFee for sponsored transactions fails if the fee is not enough") {
    sponsoredTransactionsCheckFeeTest(feeInAssets = true, feeAmount = 1)(_ should produce("does not exceed minimal value of"))
  }

  private def smartAccountCheckFeeTest(feeInAssets: Boolean, feeAmount: Long)(f: Either[ValidationError, Unit] => Any): Unit = {
    val settings = createSettings(BlockchainFeatures.SmartAccounts -> 0)
    val gen      = sponsorAndSetScriptGen(sponsorship = false, smartToken = false, smartAccount = true, feeInAssets, feeAmount)
    forAll(gen) {
      case (genesisBlock, transferTx) =>
        withLevelDBWriter(settings) { blockchain =>
          val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _) =
            BlockDiffer.fromBlock(blockchain, None, genesisBlock, MiningConstraint.Unlimited).explicitGet()
          blockchain.append(preconditionDiff, preconditionFees, totalFee, None, genesisBlock.header.generationSignature, genesisBlock)

          f(FeeValidation(blockchain, transferTx))
        }
    }
  }

  property("checkFee for smart accounts sunny") {
    smartAccountCheckFeeTest(feeInAssets = false, feeAmount = 400000)(_.explicitGet())
  }

  private def sponsorAndSetScriptGen(sponsorship: Boolean, smartToken: Boolean, smartAccount: Boolean, feeInAssets: Boolean, feeAmount: Long) =
    for {
      richAcc      <- accountGen
      recipientAcc <- accountGen
      ts = System.currentTimeMillis()
    } yield {
      val script = ExprScript(TRUE).explicitGet()

      val genesisTx = GenesisTransaction.create(richAcc.toAddress, ENOUGH_AMT, ts).explicitGet()

      val issueTx =
        if (smartToken)
          IssueTransaction(
            TxVersion.V2,
            richAcc.publicKey,
            "test".utf8Bytes,
            "desc".utf8Bytes,
            Long.MaxValue,
            2,
            reissuable = false,
            Some(script),
            Constants.UnitsInWave,
            ts
          ).signWith(richAcc.privateKey)
        else
          IssueTransaction(
            TxVersion.V1,
            richAcc.publicKey,
            "test".utf8Bytes,
            "desc".utf8Bytes,
            Long.MaxValue,
            2,
            reissuable = false,
            script = None,
            Constants.UnitsInWave,
            ts
          ).signWith(richAcc.privateKey)

      val transferWavesTx = TransferTransaction.selfSigned(1.toByte, richAcc, recipientAcc.toAddress, Waves, 10 * Constants.UnitsInWave, Waves, 1 * Constants.UnitsInWave, ByteStr.empty, ts)
        .explicitGet()

      val transferAssetTx = TransferTransaction
        .selfSigned(
          1.toByte,
          richAcc,
          recipientAcc.toAddress,
          IssuedAsset(issueTx.id()),
          100,
          Waves,
          if (smartToken) {
            1 * Constants.UnitsInWave + ScriptExtraFee
          } else {
            1 * Constants.UnitsInWave
          }, ByteStr.empty,
          ts
        )
        .explicitGet()

      val sponsorTx =
        if (sponsorship)
          Seq(
            SponsorFeeTransaction
              .selfSigned(1.toByte, richAcc, IssuedAsset(issueTx.id()), Some(10), if (smartToken) {
                Constants.UnitsInWave + ScriptExtraFee
              } else {
                Constants.UnitsInWave
              }, ts)
              .explicitGet()
          )
        else Seq.empty

      val setScriptTx =
        if (smartAccount)
          Seq(
            SetScriptTransaction
              .selfSigned(1.toByte, recipientAcc, Some(script), 1 * Constants.UnitsInWave, ts)
              .explicitGet()
          )
        else Seq.empty

      val transferBackTx = TransferTransaction.selfSigned(
          1.toByte,
          recipientAcc,
          richAcc.toAddress,
          IssuedAsset(issueTx.id()),
          1,
          if (feeInAssets) IssuedAsset(issueTx.id()) else Waves,
          feeAmount, ByteStr.empty,
          ts
        )
        .explicitGet()

      (TestBlock.create(Vector[Transaction](genesisTx, issueTx, transferWavesTx, transferAssetTx) ++ sponsorTx ++ setScriptTx), transferBackTx)
    }

  private def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): FunctionalitySettings =
    TestFunctionalitySettings.Enabled
      .copy(
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }.toMap,
        blocksForFeatureActivation = 1,
        featureCheckBlocksPeriod = 1
      )

  private def smartTokensCheckFeeTest(feeInAssets: Boolean, feeAmount: Long)(f: Either[ValidationError, Unit] => Any): Unit = {
    val settings = createSettings(BlockchainFeatures.SmartAccounts -> 0, BlockchainFeatures.SmartAssets -> 0)
    val gen      = sponsorAndSetScriptGen(sponsorship = false, smartToken = true, smartAccount = false, feeInAssets, feeAmount)
    forAll(gen) {
      case (genesisBlock, transferTx) =>
        withLevelDBWriter(settings) { blockchain =>
          val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _) =
            BlockDiffer.fromBlock(blockchain, None, genesisBlock, MiningConstraint.Unlimited).explicitGet()
          blockchain.append(preconditionDiff, preconditionFees, totalFee, None, genesisBlock.header.generationSignature, genesisBlock)

          f(FeeValidation(blockchain, transferTx))
        }
    }
  }

  property("checkFee for smart tokens sunny") {
    smartTokensCheckFeeTest(feeInAssets = false, feeAmount = 1)(_.explicitGet())
  }

  property("disallows other network") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, Transaction)] = for {
      master    <- accountGen
      recipient <- accountGen
      timestamp <- positiveLongGen
      amount    <- smallFeeGen
      script    <- scriptGen
      asset     <- bytes32gen.map(bs => IssuedAsset(ByteStr(bs)))
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, timestamp).explicitGet()

      invChainId <- invalidChainIdGen
      invChainAddr  = recipient.toAddress(invChainId)
      invChainAlias = Alias.createWithChainId("test", invChainId).explicitGet()
      invChainAddrOrAlias <- Gen.oneOf(invChainAddr, invChainAlias)

      tx <- Gen.oneOf(
        GenesisTransaction.create(invChainAddr, amount, timestamp).explicitGet(),
        PaymentTransaction.create(master, invChainAddr, amount, amount, timestamp).explicitGet(),
        TransferTransaction(
          TxVersion.V3,
          master.publicKey,
          invChainAddrOrAlias,
          Waves,
          amount,
          Waves,
          amount,
          ByteStr.empty,
          timestamp,
          Proofs.empty,
          invChainId
        ).signWith(master.privateKey),
        CreateAliasTransaction(TxVersion.V3, master.publicKey, invChainAlias.name, amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        LeaseTransaction(TxVersion.V3, master.publicKey, invChainAddrOrAlias, amount, amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        InvokeScriptTransaction(
          TxVersion.V2,
          master.publicKey,
          invChainAddrOrAlias,
          None,
          Nil,
          amount,
          Waves,
          InvokeScriptTransaction.DefaultExtraFeePerStep,
          timestamp,
          Proofs.empty,
          invChainId
        ).signWith(master.privateKey),
        exchangeV1GeneratorP(master, recipient, asset, Waves, None, invChainId).sample.get,
        IssueTransaction(
          TxVersion.V2,
          master.publicKey,
          ByteString.copyFrom(asset.id.arr),
          ByteString.copyFrom(asset.id.arr),
          amount,
          8: Byte,
          reissuable = true,
          None,
          amount,
          timestamp,
          Proofs.empty,
          invChainId
        ).signWith(master.privateKey),
        MassTransferTransaction(
          TxVersion.V2,
          master.publicKey,
          Waves,
          Seq(ParsedTransfer(invChainAddrOrAlias, amount)),
          amount,
          timestamp,
          ByteStr.empty,
          Proofs.empty,
          invChainId
        ).signWith(master.privateKey),
        LeaseCancelTransaction(TxVersion.V3, master.publicKey, asset.id, amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        SetScriptTransaction(TxVersion.V2, master.publicKey, Some(script), amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        SetAssetScriptTransaction(TxVersion.V2, master.publicKey, asset, Some(script), amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        BurnTransaction(TxVersion.V2, master.publicKey, asset, amount, amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        ReissueTransaction(TxVersion.V2, master.publicKey, asset, amount, reissuable = false, amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        SponsorFeeTransaction(TxVersion.V2, master.publicKey, asset, Some(amount), amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey),
        UpdateAssetInfoTransaction(TxVersion.V2, master.publicKey, asset, "1", "2", timestamp, amount, Waves, Proofs.empty, invChainId).signWith(master.privateKey),
        DataTransaction(TxVersion.V2, master.publicKey, Nil, amount, timestamp, Proofs.empty, invChainId).signWith(master.privateKey)
      )
    } yield (genesis, tx)

    forAll(preconditionsAndPayment) {
      case (genesis, tx) =>
        tx.chainId should not be AddressScheme.current.chainId
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(tx))) { blockDiffEi =>
          blockDiffEi should produce("Data from other network")
        }
    }
  }

  property("disallow continuation in progress") {
    def compile(scriptText: String): Script =
      ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()._1

    val dApp =
      compile(
        s"""
           | {-# STDLIB_VERSION 5 #-}
           | {-# CONTENT_TYPE DAPP #-}
           |
           | @Callable(i)
           | func default() = {
           |   let a = !(${List.fill(40)("sigVerify(base64'', base64'', base64'')").mkString("||")})
           |   if (a)
           |     then
           |       [BooleanEntry("isAllowed", true)]
           |     else
           |       throw("unexpected")
           | }
         """.stripMargin
      )

    val preconditionsAndPayment = for {
      master    <- accountGen
      dAppAcc   <- accountGen
      timestamp <- positiveLongGen
      fee       <- smallFeeGen
      genesis1  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, timestamp).explicitGet()
      genesis2  = GenesisTransaction.create(dAppAcc.toAddress, ENOUGH_AMT, timestamp).explicitGet()
      setScript = SetScriptTransaction.selfSigned(TxVersion.V2, dAppAcc, Some(dApp), fee, timestamp).explicitGet()
      invoke = InvokeScriptTransaction
        .selfSigned(
          TxVersion.V3,
          master,
          dAppAcc.toAddress,
          None,
          Nil,
          fee * 10,
          Waves,
          InvokeScriptTransaction.DefaultExtraFeePerStep,
          timestamp
        )
        .explicitGet()
      continuation = ContinuationTransaction(invoke.id.value(), nonce = 0, fee = 0L, Waves, timestamp)
      transfer = TransferTransaction
        .selfSigned(TxVersion.V2, dAppAcc, master.toAddress, Waves, 1L, Waves, fee, ByteStr.empty, timestamp)
        .explicitGet()
    } yield (Seq(genesis1, genesis2), setScript, invoke, continuation, transfer)

    val rideV5Activated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.SmartAccounts.id           -> 0,
        BlockchainFeatures.Ride4DApps.id              -> 0,
        BlockchainFeatures.BlockV5.id                 -> 0,
        BlockchainFeatures.ContinuationTransaction.id -> 0
      )
    )

    forAll(preconditionsAndPayment) {
      case (genesis, setScript, invoke, continuation, transfer) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis), TestBlock.create(Seq(setScript, invoke, continuation))),
          TestBlock.create(Seq(transfer)),
          rideV5Activated
        )(_ should produce("BlockedByContinuation"))
    }
    forAll(preconditionsAndPayment) {
      case (genesis, setScript, invoke, continuation, transfer) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis), TestBlock.create(Seq(setScript, invoke, continuation, continuation.copy(nonce = 1)))),
          TestBlock.create(Seq(transfer)),
          rideV5Activated
        )(_ shouldBe Symbol("right"))
    }
  }
}
