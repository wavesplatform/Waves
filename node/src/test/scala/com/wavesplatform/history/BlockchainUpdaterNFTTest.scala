package com.wavesplatform.history

import com.wavesplatform.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{KeyTags, Keys}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.diffs
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalacheck.Gen
import org.scalatest.*

class BlockchainUpdaterNFTTest extends PropSpec with DomainScenarioDrivenPropertyCheck with BlocksTransactionsHelpers {

  property("nft list should be consistent with transfer") {
    forAll(Preconditions.nftTransfer) { case (issue, (firstAccount, secondAccount), (genesisBlock, issueBlock, keyBlock, postBlock), mbs) =>
      withDomain(settingsWithFeatures(BlockchainFeatures.NG, BlockchainFeatures.ReduceNFTFee)) { d =>
        d.blockchainUpdater.processBlock(genesisBlock) should beRight
        d.blockchainUpdater.processBlock(issueBlock) should beRight
        d.blockchainUpdater.processBlock(keyBlock) should beRight

        d.nftList(firstAccount).map(_._1.id) shouldBe Seq(issue.id())
        d.nftList(secondAccount) shouldBe Nil

        d.blockchainUpdater.processMicroBlock(mbs.head, None) should beRight
        d.nftList(firstAccount) shouldBe Nil
        d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())

        d.blockchainUpdater.processBlock(postBlock) should beRight
        d.nftList(firstAccount) shouldBe Nil
        d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())
      }
    }
  }

  property("nft list should be consistent with invokescript") {
    forAll(Preconditions.nftInvokeScript) { case (issue, (firstAccount, secondAccount), (genesisBlock, issueBlock, keyBlock, postBlock), mbs) =>
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

        d.blockchainUpdater.processMicroBlock(mbs.head, None) should beRight
        d.nftList(firstAccount) shouldBe Nil
        d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())

        d.blockchainUpdater.processBlock(postBlock) should beRight
        d.nftList(firstAccount) shouldBe Nil
        d.nftList(secondAccount).map(_._1.id) shouldBe Seq(issue.id())
      }
    }
  }

  property("nft list should be persisted only once independently to using bloom filters") {
    forAll(Preconditions.nftList) { case (issue, (firstAccount, secondAccount), (genesisBlock, firstBlock, secondBlock, postBlock)) =>
      def assert(d: Domain): Assertion = {
        import com.wavesplatform.database.DBExt

        d.blockchainUpdater.processBlock(genesisBlock) should beRight
        d.blockchainUpdater.processBlock(firstBlock) should beRight
        d.blockchainUpdater.processBlock(secondBlock) should beRight
        d.blockchainUpdater.processBlock(postBlock) should beRight

        d.nftList(firstAccount).map(_._1.id) shouldBe Seq(issue.id())
        d.nftList(secondAccount) shouldBe Nil

        val persistedNfts = Seq.newBuilder[IssuedAsset]
        d.rdb.db.readOnly { ro =>
          val addressId = ro.get(Keys.addressId(firstAccount)).get
          ro.iterateOver(KeyTags.NftPossession.prefixBytes ++ addressId.toByteArray) { e =>
            persistedNfts += IssuedAsset(ByteStr(e.getKey.takeRight(32)))
          }
        }

        persistedNfts.result() shouldBe Seq(IssuedAsset(issue.id()))
      }

      val settings = settingsWithFeatures(BlockchainFeatures.NG, BlockchainFeatures.ReduceNFTFee)
      withDomain(settings)(assert)
      withDomain(settings.copy(dbSettings = settings.dbSettings.copy(useBloomFilter = true)))(assert)
    }
  }

  private[this] object Preconditions {
    import UnsafeBlocks.*

    val nftTransfer: Gen[(IssueTransaction, (Address, Address), (Block, Block, Block, Block), Seq[MicroBlock])] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen
        blockTime = ntpNow
        issue    <- QuickTX.nftIssue(richAccount, Gen.const(blockTime))
        transfer <- QuickTX.transferAsset(issue.asset, richAccount, secondAccount.toAddress, 1, Gen.const(blockTime))
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
        (issue, (richAccount.toAddress, secondAccount.toAddress), (genesisBlock, issueBlock, keyBlock, postBlock), microBlocks)
      }
    }

    val nftInvokeScript: Gen[(IssueTransaction, (Address, Address), (Block, Block, Block, Block), Seq[MicroBlock])] =
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
            Seq(InvokeScriptTransaction.Payment(1, issue.asset)),
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
        (issue, (richAccount.toAddress, secondAccount.toAddress), (genesisBlock, issueBlock, keyBlock, postBlock), microBlocks)
      }

    val nftList: Gen[(IssueTransaction, (Address, Address), (Block, Block, Block, Block))] =
      for {
        firstAccount  <- accountGen
        secondAccount <- accountGen
        blockTime = ntpNow
        issue     <- QuickTX.nftIssue(firstAccount, Gen.const(blockTime))
        transfer1 <- QuickTX.transferAsset(issue.asset, firstAccount, secondAccount.toAddress, 1, Gen.const(blockTime))
        transfer2 <- QuickTX.transferAsset(issue.asset, secondAccount, firstAccount.toAddress, 1, Gen.const(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(
            GenesisTransaction.create(firstAccount.toAddress, diffs.ENOUGH_AMT / 2, 0).explicitGet(),
            GenesisTransaction.create(secondAccount.toAddress, diffs.ENOUGH_AMT / 2, 0).explicitGet(),
            issue
          ),
          signer = TestBlock.defaultSigner,
          version = 3.toByte,
          timestamp = blockTime
        )

        val firstBlock = unsafeBlock(
          genesisBlock.signature,
          Seq(transfer1),
          firstAccount,
          3.toByte,
          blockTime
        )

        val secondBlock = unsafeBlock(
          firstBlock.signature,
          Seq(transfer2),
          firstAccount,
          3.toByte,
          blockTime
        )

        val postBlock = unsafeBlock(
          secondBlock.signature,
          Seq.empty,
          firstAccount,
          3.toByte,
          blockTime
        )

        (issue, (firstAccount.toAddress, secondAccount.toAddress), (genesisBlock, firstBlock, secondBlock, postBlock))
      }
  }
}
