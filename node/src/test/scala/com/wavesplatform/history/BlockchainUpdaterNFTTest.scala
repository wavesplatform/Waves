package com.wavesplatform.history

import com.wavesplatform._
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.diffs
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterNFTTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with EitherMatchers
    with TransactionGen
    with BlocksTransactionsHelpers
    with NoShrink {

  property("nft list should be consistent with transfer") {
    forAll(Preconditions.nftTransfer()) {
      case (issue, Seq(firstAccount, secondAccount), Seq(genesisBlock, issueBlock, keyBlock, postBlock), Seq(microBlock)) =>
        withDomain(settingsWithFeatures(BlockchainFeatures.NG, BlockchainFeatures.ReduceNFTFee)) { d =>
          d.blockchainUpdater.processBlock(genesisBlock) should beRight
          d.blockchainUpdater.processBlock(issueBlock) should beRight
          d.blockchainUpdater.processBlock(keyBlock) should beRight

          d.nftList(firstAccount).map(_._1.id) shouldBe Seq(issue.id())
          d.nftList(secondAccount) shouldBe Nil

          d.blockchainUpdater.processMicroBlock(microBlock) should beRight
          d.nftList(firstAccount) shouldBe Nil
          d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())

          d.blockchainUpdater.processBlock(postBlock) should beRight
          d.nftList(firstAccount) shouldBe Nil
          d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())
        }
    }
  }

  property("nft list should be consistent with invokescript") {
    forAll(Preconditions.nftInvokeScript()) {
      case (issue, Seq(firstAccount, secondAccount), Seq(genesisBlock, issueBlock, keyBlock, postBlock), Seq(microBlock)) =>
        withDomain(
          settingsWithFeatures(
            BlockchainFeatures.NG,
            BlockchainFeatures.ReduceNFTFee,
            BlockchainFeatures.SmartAccounts,
            BlockchainFeatures.Ride4DApps
          )
        ) { d =>
          d.blockchainUpdater.processBlock(genesisBlock) should beRight
          d.blockchainUpdater.processBlock(issueBlock) should beRight
          d.blockchainUpdater.processBlock(keyBlock) should beRight

          d.nftList(firstAccount).map(_._1.id) shouldBe Seq(issue.id())
          d.nftList(secondAccount) shouldBe Nil

          d.blockchainUpdater.processMicroBlock(microBlock) should beRight
          d.nftList(firstAccount) shouldBe Nil
          d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())

          d.blockchainUpdater.processBlock(postBlock) should beRight
          d.nftList(firstAccount) shouldBe Nil
          d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())
        }
    }
  }

  private[this] object Preconditions {
    import UnsafeBlocks._

    def nftTransfer(): Gen[(IssueTransaction, Seq[Address], Seq[Block], Seq[MicroBlock])] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen
        blockTime = ntpNow
        issue    <- QuickTX.nftIssue(richAccount, Gen.const(blockTime))
        transfer <- QuickTX.transferAsset(IssuedAsset(issue.assetId), richAccount, secondAccount.toAddress, 1, Gen.const(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount.toAddress, diffs.ENOUGH_AMT, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3.toByte,
          timestamp = 0
        )

        val issueBlock = unsafeBlock(
          genesisBlock.signature,
          Seq(issue),
          richAccount,
          3.toByte,
          blockTime
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = issueBlock.signature,
          base = Seq(),
          micros = Seq(Seq(transfer)),
          signer = richAccount,
          version = 3.toByte,
          blockTime
        )

        val (postBlock, _) = unsafeChainBaseAndMicro(
          totalRefTo = microBlocks.last.totalResBlockSig,
          base = Seq(),
          micros = Seq(),
          signer = richAccount,
          version = 3.toByte,
          blockTime
        )
        (issue, Seq(richAccount.toAddress, secondAccount.toAddress), Seq(genesisBlock, issueBlock, keyBlock, postBlock), microBlocks)
      }
    }

    def nftInvokeScript(): Gen[(IssueTransaction, Seq[Address], Seq[Block], Seq[MicroBlock])] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen
        blockTime = ntpNow
        issue <- QuickTX.nftIssue(richAccount, Gen.const(blockTime))
        setScript <- {
          val scriptText =
            s"""
               |{-# STDLIB_VERSION 3 #-}
               |{-# CONTENT_TYPE DAPP #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |
               |@Callable(i)
               |func nftTransfer() = {
               |    let pmt = i.payment.extract()
               |    TransferSet([
               |            ScriptTransfer(this, pmt.amount, pmt.assetId)
               |        ])
               |}
               |
               | @Verifier(t)
               | func verify() = {
               |  true
               | }
               |
               |
              """.stripMargin
          val (script, _) = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()
          QuickTX.setScript(secondAccount, script, Gen.const(blockTime))
        }
        invokeScript <- {
          val fc = FUNCTION_CALL(FunctionHeader.User("nftTransfer"), Nil)
          QuickTX.invokeScript(
            richAccount,
            secondAccount.toAddress,
            fc,
            Seq(InvokeScriptTransaction.Payment(1, IssuedAsset(issue.assetId))),
            Gen.const(blockTime)
          )
        }
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(
            GenesisTransaction.create(richAccount.toAddress, diffs.ENOUGH_AMT, 0).explicitGet(),
            GenesisTransaction.create(secondAccount.toAddress, 1000000, 0).explicitGet()
          ),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val issueBlock = unsafeBlock(
          genesisBlock.signature,
          Seq(issue, setScript),
          richAccount,
          3,
          blockTime
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = issueBlock.signature,
          base = Seq(),
          micros = Seq(Seq(invokeScript)),
          signer = richAccount,
          version = 3,
          blockTime
        )

        val (postBlock, _) = unsafeChainBaseAndMicro(
          totalRefTo = microBlocks.last.totalResBlockSig,
          base = Seq(),
          micros = Seq(),
          signer = richAccount,
          version = 3,
          blockTime
        )
        (issue, Seq(richAccount.toAddress, secondAccount.toAddress), Seq(genesisBlock, issueBlock, keyBlock, postBlock), microBlocks)
      }
    }
  }
}
