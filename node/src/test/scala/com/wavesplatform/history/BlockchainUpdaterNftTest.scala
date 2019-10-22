package com.wavesplatform.history

import com.wavesplatform._
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.diffs
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class BlockchainUpdaterNftTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen
    with BlocksTransactionsHelpers
    with NoShrink {

  property("nft list should be consistent") {
    forAll(Preconditions.nftTransfer()) {
      case (issue, Seq(firstAccount, secondAccount), Seq(genesisBlock, issueBlock, keyBlock, postBlock), Seq(microBlock)) =>
        withDomain(settingsWithFeatures(BlockchainFeatures.NG, BlockchainFeatures.ReduceNFTFee)) { d =>
          def nftList(address: Address): Seq[IssueTransaction] =
            Await.result(d.blockchainUpdater.nftObservable(address, None).toListL.runToFuture, Duration.Inf)

          d.blockchainUpdater.processBlock(genesisBlock) shouldBe 'right
          d.blockchainUpdater.processBlock(issueBlock) shouldBe 'right
          d.blockchainUpdater.processBlock(keyBlock) shouldBe 'right

          nftList(firstAccount) shouldBe Seq(issue)
          nftList(secondAccount) shouldBe Nil

          d.blockchainUpdater.processMicroBlock(microBlock) shouldBe 'right
          nftList(firstAccount) shouldBe Nil
          nftList(secondAccount) shouldBe Seq(issue)

          d.blockchainUpdater.processBlock(postBlock) shouldBe 'right
          nftList(firstAccount) shouldBe Nil
          nftList(secondAccount) shouldBe Seq(issue)
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
        transfer <- QuickTX.transferAsset(IssuedAsset(issue.assetId), richAccount, secondAccount, 1, Gen.const(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount, diffs.ENOUGH_AMT, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val issueBlock = unsafeBlock(
          genesisBlock.signerData.signature,
          Seq(issue),
          richAccount,
          3,
          blockTime
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = issueBlock.signerData.signature,
          base = Seq(),
          micros = Seq(Seq(transfer)),
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
