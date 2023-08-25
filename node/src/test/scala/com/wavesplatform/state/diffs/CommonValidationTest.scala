package com.wavesplatform.state.diffs

import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxHelpers, TxNonNegativeAmount, TxVersion}

class CommonValidationTest extends PropSpec with WithState {

  property("disallows double spending") {
    val preconditionsAndPayment: Seq[(GenesisTransaction, TransferTransaction)] = {
      val master     = TxHelpers.signer(1)
      val recipients = Seq(master, TxHelpers.signer(2))

      val genesis = TxHelpers.genesis(master.toAddress)

      recipients.map { recipient =>
        val transfer = TxHelpers.transfer(master, recipient.toAddress, version = TxVersion.V1)
        (genesis, transfer)
      }
    }

    preconditionsAndPayment.foreach { case (genesis, transfer) =>
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
    val gen      = sponsorAndSetScript(sponsorship = true, smartToken = false, smartAccount = false, feeInAssets, feeAmount)
    forAll(gen) { case (genesisBlock, transferTx) =>
      withRocksDBWriter(settings) { blockchain =>
        val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _, computedStateHash) =
          BlockDiffer.fromBlock(blockchain, None, genesisBlock, MiningConstraint.Unlimited, genesisBlock.header.generationSignature).explicitGet()
        blockchain.append(
          preconditionDiff,
          preconditionFees,
          totalFee,
          None,
          genesisBlock.header.generationSignature,
          computedStateHash,
          genesisBlock
        )

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
    val settings                   = createSettings(BlockchainFeatures.SmartAccounts -> 0)
    val (genesisBlock, transferTx) = sponsorAndSetScript(sponsorship = false, smartToken = false, smartAccount = true, feeInAssets, feeAmount)
    withRocksDBWriter(settings) { blockchain =>
      val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _, computedStateHash) =
        BlockDiffer.fromBlock(blockchain, None, genesisBlock, MiningConstraint.Unlimited, genesisBlock.header.generationSignature).explicitGet()
      blockchain.append(preconditionDiff, preconditionFees, totalFee, None, genesisBlock.header.generationSignature, computedStateHash, genesisBlock)

      f(FeeValidation(blockchain, transferTx))
    }
  }

  property("checkFee for smart accounts sunny") {
    smartAccountCheckFeeTest(feeInAssets = false, feeAmount = 400000)(_.explicitGet())
  }

  private def sponsorAndSetScript(sponsorship: Boolean, smartToken: Boolean, smartAccount: Boolean, feeInAssets: Boolean, feeAmount: Long) = {
    val richAcc      = TxHelpers.signer(1)
    val recipientAcc = TxHelpers.signer(2)

    val script = ExprScript(TRUE).explicitGet()

    val genesis = TxHelpers.genesis(richAcc.toAddress)
    val issue = if (smartToken) {
      TxHelpers.issue(richAcc, Long.MaxValue, 2, script = Some(script), reissuable = false, fee = Constants.UnitsInWave)
    } else {
      TxHelpers.issue(richAcc, Long.MaxValue, 2, script = None, reissuable = false, fee = Constants.UnitsInWave, version = TxVersion.V1)
    }
    val transferWaves =
      TxHelpers.transfer(richAcc, recipientAcc.toAddress, 10 * Constants.UnitsInWave, fee = Constants.UnitsInWave, version = TxVersion.V1)
    val transferAssetFee = if (smartToken) {
      1 * Constants.UnitsInWave + ScriptExtraFee
    } else {
      1 * Constants.UnitsInWave
    }
    val transferAsset = TxHelpers.transfer(richAcc, recipientAcc.toAddress, 100, issue.asset, fee = transferAssetFee, version = TxVersion.V1)
    val sponsorTxFee = if (smartToken) {
      Constants.UnitsInWave + ScriptExtraFee
    } else {
      Constants.UnitsInWave
    }
    val sponsor =
      if (sponsorship) {
        Seq(TxHelpers.sponsor(issue.asset, Some(10), richAcc, fee = sponsorTxFee))
      } else {
        Seq.empty
      }
    val setScript =
      if (smartAccount) {
        Seq(TxHelpers.setScript(recipientAcc, script, fee = Constants.UnitsInWave))
      } else {
        Seq.empty
      }

    val transferBack = TxHelpers.transfer(
      from = recipientAcc,
      to = richAcc.toAddress,
      amount = 1,
      asset = issue.asset,
      fee = feeAmount,
      feeAsset = if (feeInAssets) issue.asset else Waves,
      version = TxVersion.V1
    )

    (TestBlock.create(Vector[Transaction](genesis, issue, transferWaves, transferAsset) ++ sponsor ++ setScript), transferBack)
  }

  private def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): FunctionalitySettings =
    TestFunctionalitySettings.Enabled
      .copy(
        featureCheckBlocksPeriod = 1,
        blocksForFeatureActivation = 1,
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }.toMap
      )

  private def smartTokensCheckFeeTest(feeInAssets: Boolean, feeAmount: Long)(f: Either[ValidationError, Unit] => Any): Unit = {
    val settings                   = createSettings(BlockchainFeatures.SmartAccounts -> 0, BlockchainFeatures.SmartAssets -> 0)
    val (genesisBlock, transferTx) = sponsorAndSetScript(sponsorship = false, smartToken = true, smartAccount = false, feeInAssets, feeAmount)
    withRocksDBWriter(settings) { blockchain =>
      val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _, computedStateHash) =
        BlockDiffer.fromBlock(blockchain, None, genesisBlock, MiningConstraint.Unlimited, genesisBlock.header.generationSignature).explicitGet()
      blockchain.append(preconditionDiff, preconditionFees, totalFee, None, genesisBlock.header.generationSignature, computedStateHash, genesisBlock)

      f(FeeValidation(blockchain, transferTx))
    }
  }

  property("checkFee for smart tokens sunny") {
    smartTokensCheckFeeTest(feeInAssets = false, feeAmount = 1)(_.explicitGet())
  }

  property("disallows other network") {
    val preconditionsAndPayment: Seq[(GenesisTransaction, Transaction)] = {
      val master    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)

      val amount = 100.waves
      val script = ExprScript(TRUE).explicitGet()
      val asset  = IssuedAsset(ByteStr.fill(32)(1))

      val genesis = TxHelpers.genesis(master.toAddress)

      val invChainId    = '#'.toByte
      val invChainAddr  = recipient.toAddress(invChainId)
      val invChainAlias = Alias.createWithChainId("test", invChainId).explicitGet()
      Seq(
        TxHelpers.genesis(invChainAddr, amount),
        TxHelpers.payment(master, invChainAddr, amount),
        TxHelpers.transfer(master, invChainAddr, amount, version = TxVersion.V3, chainId = invChainId),
        TxHelpers.transfer(master, invChainAlias, amount, version = TxVersion.V3, chainId = invChainId),
        TxHelpers.createAlias(invChainAlias.name, master, version = TxVersion.V3, chainId = invChainId),
        TxHelpers.lease(master, invChainAddr, amount, version = TxVersion.V3),
        TxHelpers.lease(master, invChainAlias, amount, version = TxVersion.V3),
        TxHelpers.invoke(invChainAddr, invoker = master),
        TxHelpers.invoke(invChainAlias, invoker = master),
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, asset, Waves, Waves, amount, 1_0000_0000L, fee = 1L, sender = master),
          TxHelpers.order(OrderType.SELL, asset, Waves, Waves, amount, 1_0000_0000L, fee = 1L, sender = recipient),
          master,
          chainId = invChainId
        ),
        TxHelpers.issue(master, amount, chainId = invChainId),
        TxHelpers.massTransfer(master, Seq(ParsedTransfer(invChainAddr, TxNonNegativeAmount.unsafeFrom(amount))), chainId = invChainId),
        TxHelpers.leaseCancel(asset.id, master, version = TxVersion.V3, chainId = invChainId),
        TxHelpers.setScript(master, script, version = TxVersion.V2, chainId = invChainId),
        TxHelpers.setAssetScript(master, asset, script, version = TxVersion.V2, chainId = invChainId),
        TxHelpers.burn(asset, amount, master, version = TxVersion.V2, chainId = invChainId),
        TxHelpers.reissue(asset, master, amount, chainId = invChainId),
        TxHelpers.sponsor(asset, Some(amount), master, version = TxVersion.V2, chainId = invChainId),
        TxHelpers.updateAssetInfo(asset.id, sender = master, chainId = invChainId),
        TxHelpers.dataV2(master, Seq.empty, chainId = invChainId)
      ).map(genesis -> _)
    }

    preconditionsAndPayment.foreach { case (genesis, tx) =>
      tx.chainId should not be AddressScheme.current.chainId
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(tx))) { blockDiffEi =>
        blockDiffEi should produce("Address belongs to another network")
      }
    }
  }
}
