package com.wavesplatform.state

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.features._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.assets.{IssueTransactionV1, ReissueTransactionV1}
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseTransactionV1}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{CreateAliasTransactionV1, DataTransaction, GenesisTransaction, Transaction}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen, history}
import org.scalacheck.Gen
import org.scalatest.{Assertions, FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class RollbackSpec extends FreeSpec with Matchers with WithDomain with TransactionGen with PropertyChecks with NoShrink {
  private val time   = new TestTime
  private def nextTs = time.getTimestamp()

  private def genesisBlock(genesisTs: Long, address: Address, initialBalance: Long) = TestBlock.create(
    genesisTs,
    ByteStr(Array.fill[Byte](SignatureLength)(0)),
    Seq(GenesisTransaction.create(address, initialBalance, genesisTs).explicitGet())
  )

  private def transfer(sender: KeyPair, recipient: Address, amount: Long) =
    TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, nextTs, Waves, 1, Array.empty[Byte]).explicitGet()

  private def randomOp(sender: KeyPair, recipient: Address, amount: Long, op: Int, nextTs: => Long = nextTs) = {
    import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
    op match {
      case 1 =>
        val lease = LeaseTransactionV1.selfSigned(sender, amount, 100000, nextTs, recipient).explicitGet()
        List(lease, LeaseCancelTransactionV1.selfSigned(sender, lease.id(), 1, nextTs).explicitGet())
      case 2 =>
        List(
          MassTransferTransaction
            .selfSigned(Waves, sender, List(ParsedTransfer(recipient, amount), ParsedTransfer(recipient, amount)), nextTs, 10000, Array.empty[Byte])
            .explicitGet())
      case _ => List(TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, nextTs, Waves, 1000, Array.empty[Byte]).explicitGet())
    }
  }

  "Rollback resets" - {
    "Rollback save dropped blocks order" in forAll(accountGen, positiveLongGen, Gen.choose(1, 10)) {
      case (sender, initialBalance, blocksCount) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisSignature = d.lastBlockId
          def newBlocks(i: Int): List[ByteStr] = {
            if (i == blocksCount) {
              Nil
            } else {
              val block = TestBlock.create(nextTs + i, d.lastBlockId, Seq())
              d.appendBlock(block)
              block.uniqueId :: newBlocks(i + 1)
            }
          }
          val blocks        = newBlocks(0)
          val droppedBlocks = d.removeAfter(genesisSignature)
          droppedBlocks(0).reference shouldBe genesisSignature
          droppedBlocks.map(_.uniqueId).toList shouldBe blocks
          droppedBlocks foreach d.appendBlock
        }
    }

    "forget rollbacked transaction for querying" in forAll(accountGen, accountGen, Gen.nonEmptyListOf(Gen.choose(1, 10))) {
      case (sender, recipient, txCount) =>
        withDomain(createSettings(MassTransfer -> 0)) { d =>
          d.appendBlock(genesisBlock(nextTs, sender, com.wavesplatform.state.diffs.ENOUGH_AMT))

          val genesisSignature = d.lastBlockId

          val transferAmount = 100

          val transfers = txCount.map(tc => Seq.fill(tc)(randomOp(sender, recipient, transferAmount, tc % 3)).flatten)

          for (transfer <- transfers) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                transfer
              ))
          }

          val stransactions1 = d.addressTransactions(sender).sortBy(_._2.timestamp)
          val rtransactions1 = d.addressTransactions(recipient).sortBy(_._2.timestamp)

          d.removeAfter(genesisSignature)

          for (transfer <- transfers) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                transfer
              ))
          }

          val stransactions2 = d.addressTransactions(sender).sortBy(_._2.timestamp)
          val rtransactions2 = d.addressTransactions(recipient).sortBy(_._2.timestamp)

          stransactions1 shouldBe stransactions2
          rtransactions1 shouldBe rtransactions2
        }
    }

    "waves balances" in forAll(accountGen, positiveLongGen, accountGen, Gen.nonEmptyListOf(Gen.choose(1, 10))) {
      case (sender, initialBalance, recipient, txCount) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))

          val genesisSignature = d.lastBlockId

          d.portfolio(sender.toAddress).balance shouldBe initialBalance
          d.portfolio(recipient.toAddress).balance shouldBe 0

          val totalTxCount   = txCount.sum
          val transferAmount = initialBalance / (totalTxCount * 2)

          for (tc <- txCount) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                Seq.fill(tc)(transfer(sender, recipient, transferAmount))
              ))
          }

          d.portfolio(recipient).balance shouldBe (transferAmount * totalTxCount)
          d.portfolio(sender).balance shouldBe (initialBalance - (transferAmount + 1) * totalTxCount)

          d.removeAfter(genesisSignature)

          d.portfolio(sender).balance shouldBe initialBalance
          d.portfolio(recipient).balance shouldBe 0
        }
    }

    "lease balances and states" in forAll(accountGen, positiveLongGen, accountGen) {
      case (sender, initialBalance, recipient) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          d.blockchainUpdater.height shouldBe 1
          val genesisBlockId = d.lastBlockId

          val leaseAmount = initialBalance - 2
          val lt          = LeaseTransactionV1.selfSigned(sender, leaseAmount, 1, nextTs, recipient).explicitGet()
          d.appendBlock(TestBlock.create(nextTs, genesisBlockId, Seq(lt)))
          d.blockchainUpdater.height shouldBe 2
          val blockWithLeaseId = d.lastBlockId
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender, recipient, 2, leaseAmount, true))
          d.portfolio(sender).lease.out shouldEqual leaseAmount
          d.portfolio(recipient).lease.in shouldEqual leaseAmount

          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockWithLeaseId,
              Seq(LeaseCancelTransactionV1.selfSigned(sender, lt.id(), 1, nextTs).explicitGet())
            ))
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender, recipient, 2, leaseAmount, false))
          d.portfolio(sender).lease.out shouldEqual 0
          d.portfolio(recipient).lease.in shouldEqual 0

          d.removeAfter(blockWithLeaseId)
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender, recipient, 2, leaseAmount, true))
          d.portfolio(sender).lease.out shouldEqual leaseAmount
          d.portfolio(recipient).lease.in shouldEqual leaseAmount

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.leaseDetails(lt.id()) shouldBe 'empty
          d.portfolio(sender).lease.out shouldEqual 0
          d.portfolio(recipient).lease.in shouldEqual 0
        }
    }

    "asset balances" in forAll(accountGen, positiveLongGen, positiveLongGen, accountGen) {
      case (sender, initialBalance, assetAmount, recipient) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId
          val issueTransaction =
            IssueTransactionV1.selfSigned(sender, "test".getBytes("UTF-8"), Array.empty[Byte], assetAmount, 8, true, 1, nextTs).explicitGet()

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            ))

          val blockIdWithIssue = d.lastBlockId

          d.portfolio(sender).assets.get(IssuedAsset(issueTransaction.id())) should contain(assetAmount)
          d.portfolio(recipient).assets.get(IssuedAsset(issueTransaction.id())) shouldBe 'empty

          d.appendBlock(
            TestBlock.create(
              nextTs,
              d.lastBlockId,
              Seq(
                TransferTransactionV1
                  .selfSigned(IssuedAsset(issueTransaction.id()), sender, recipient, assetAmount, nextTs, Waves, 1, Array.empty[Byte])
                  .explicitGet())
            ))

          d.portfolio(sender).assets.getOrElse(IssuedAsset(issueTransaction.id()), 0) shouldEqual 0
          d.portfolio(recipient).assets.getOrElse(IssuedAsset(issueTransaction.id()), 0) shouldEqual assetAmount

          d.removeAfter(blockIdWithIssue)

          d.portfolio(sender).assets.getOrElse(IssuedAsset(issueTransaction.id()), 0) shouldEqual assetAmount
          d.portfolio(recipient).assets.getOrElse(IssuedAsset(issueTransaction.id()), 0) shouldEqual 0
        }
    }

    "asset quantity and reissuability" in forAll(accountGen, positiveLongGen, byteArrayGen(10), byteArrayGen(12)) {
      case (sender, initialBalance, name, description) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          val issueTransaction = IssueTransactionV1.selfSigned(sender, name, description, 2000, 8, true, 1, nextTs).explicitGet()
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe 'empty

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            ))

          val blockIdWithIssue = d.lastBlockId

          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) should contain(
            AssetDescription(sender, name, description, 8, true, BigInt(2000), None, 0))

          d.appendBlock(
            TestBlock.create(nextTs,
                             blockIdWithIssue,
                             Seq(
                               ReissueTransactionV1.selfSigned(sender, IssuedAsset(issueTransaction.id()), 2000, false, 1, nextTs).explicitGet()
                             )))

          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) should contain(
            AssetDescription(sender, name, description, 8, false, BigInt(4000), None, 0))

          d.removeAfter(blockIdWithIssue)
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) should contain(
            AssetDescription(sender, name, description, 8, true, BigInt(2000), None, 0))

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe 'empty
        }
    }

    "aliases" in forAll(accountGen, positiveLongGen, aliasGen) {
      case (sender, initialBalance, alias) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          d.blockchainUpdater.resolveAlias(alias) shouldBe Left(AliasDoesNotExist(alias))
          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(CreateAliasTransactionV1.selfSigned(sender, alias, 1, nextTs).explicitGet())
            ))

          d.blockchainUpdater.resolveAlias(alias) shouldBe Right(sender.toAddress)
          d.removeAfter(genesisBlockId)

          d.blockchainUpdater.resolveAlias(alias) shouldBe Left(AliasDoesNotExist(alias))
        }
    }

    "data transaction" in forAll(accountGen, positiveLongGen, dataEntryGen(1000)) {
      case (sender, initialBalance, dataEntry) =>
        withDomain(createSettings(BlockchainFeatures.DataTransaction -> 0)) { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(DataTransaction.selfSigned(sender, List(dataEntry), 1, nextTs).explicitGet())
            ))

          d.blockchainUpdater.accountData(sender, dataEntry.key) should contain(dataEntry)

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.accountData(sender, dataEntry.key) shouldBe 'empty
        }
    }

    "address script" in forAll(accountGen, positiveLongGen) {
      case (sender, initialBalance) =>
        withDomain(createSettings(SmartAccounts -> 0)) { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val script = ExprScript(TRUE).explicitGet()

          val genesisBlockId = d.lastBlockId
          d.blockchainUpdater.accountScript(sender) shouldBe 'empty
          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(SetScriptTransaction.selfSigned(sender, Some(script), 400000, nextTs).explicitGet())
            ))

          val blockWithScriptId = d.lastBlockId

          d.blockchainUpdater.accountScript(sender) should contain(script)

          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockWithScriptId,
              Seq(SetScriptTransaction.selfSigned(sender, None, 800000, nextTs).explicitGet())
            ))

          d.blockchainUpdater.accountScript(sender) shouldBe 'empty

          d.removeAfter(blockWithScriptId)
          d.blockchainUpdater.accountScript(sender) should contain(script)

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.accountScript(sender) shouldBe 'empty
        }
    }

    def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): WavesSettings = {
      val tfs = TestFunctionalitySettings.Enabled.copy(
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }(collection.breakOut),
        blocksForFeatureActivation = 1,
        featureCheckBlocksPeriod = 1
      )

      history.DefaultWavesSettings.copy(blockchainSettings = history.DefaultWavesSettings.blockchainSettings.copy(functionalitySettings = tfs))
    }

    "asset sponsorship" in forAll(for {
      sender      <- accountGen
      sponsorship <- sponsorFeeCancelSponsorFeeGen(sender)
    } yield {
      (sender, sponsorship)
    }) {
      case (sender, (issueTransaction, sponsor1, sponsor2, cancel)) =>
        val ts = issueTransaction.timestamp
        withDomain(createSettings(FeeSponsorship -> 0)) { d =>
          d.appendBlock(genesisBlock(ts, sender, Long.MaxValue / 3))
          val genesisBlockId = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              ts,
              genesisBlockId,
              Seq(issueTransaction)
            ))

          val blockIdWithIssue = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              ts + 2,
              d.lastBlockId,
              Seq(sponsor1)
            ))

          val blockIdWithSponsor = d.lastBlockId

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor1.minSponsoredAssetFee.get
          d.portfolio(sender).assets.get(IssuedAsset(issueTransaction.id())) should contain(issueTransaction.quantity)

          d.appendBlock(
            TestBlock.create(
              ts + 2,
              d.lastBlockId,
              Seq(cancel)
            ))

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe 0

          d.removeAfter(blockIdWithSponsor)

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor1.minSponsoredAssetFee.get
          d.portfolio(sender).assets.get(IssuedAsset(issueTransaction.id())) should contain(issueTransaction.quantity)

          d.appendBlock(
            TestBlock.create(
              ts + 2,
              d.lastBlockId,
              Seq(sponsor2)
            ))

          d.portfolio(sender).assets.get(IssuedAsset(issueTransaction.id())) should contain(issueTransaction.quantity)
          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor2.minSponsoredAssetFee.get

          d.removeAfter(blockIdWithIssue)

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe 0
        }
    }

    "carry fee" in forAll(for {
      sender      <- accountGen
      sponsorship <- sponsorFeeCancelSponsorFeeGen(sender)
      transfer    <- transferGeneratorP(sponsorship._1.timestamp, sender, sender, 10000000000L)
    } yield {
      (sender, sponsorship, transfer)
    }) {
      case (sender, (issue, sponsor1, sponsor2, cancel), transfer) =>
        withDomain(createSettings(NG -> 0, FeeSponsorship -> 0)) { d =>
          val ts = issue.timestamp
          def appendBlock(tx: Transaction) = {
            d.appendBlock(TestBlock.create(ts, d.lastBlockId, Seq(tx)))
            d.lastBlockId
          }
          def carry(fee: Long) = fee - fee / 5 * 2

          d.appendBlock(genesisBlock(ts, sender, Long.MaxValue / 3))
          d.carryFee shouldBe carry(0)

          val issueBlockId = appendBlock(issue)
          d.carryFee shouldBe carry(issue.fee)

          val sponsorBlockId = appendBlock(sponsor1)
          d.carryFee shouldBe carry(sponsor1.fee)

          appendBlock(transfer)
          d.carryFee shouldBe carry(transfer.fee)

          d.removeAfter(sponsorBlockId)
          d.carryFee shouldBe carry(sponsor1.fee)

          d.removeAfter(issueBlockId)
          d.carryFee shouldBe carry(issue.fee)

          val transferBlockId = appendBlock(transfer)
          d.carryFee shouldBe carry(transfer.fee)

          appendBlock(sponsor2)
          d.carryFee shouldBe carry(sponsor2.fee)

          d.removeAfter(transferBlockId)
          d.carryFee shouldBe carry(transfer.fee)
        }
    }

    "relean rollbacked transaction" in forAll(accountGen, accountGen, Gen.listOfN(66, Gen.choose(1, 10))) {
      case (sender, recipient, txCount) =>
        withDomain(createSettings(MassTransfer -> 0)) { d =>
          val ts = nextTs

          d.appendBlock(genesisBlock(ts, sender, com.wavesplatform.state.diffs.ENOUGH_AMT))

          val transferAmount = 100

          val interval = (3 * 60 * 60 * 1000 + 30 * 60 * 1000) / txCount.size

          val transfers =
            txCount.zipWithIndex.map(tc =>
              Range(0, tc._1).map(i => randomOp(sender, recipient, transferAmount, tc._1 % 3, ts + interval * tc._2 + i)).flatten)

          val blocks = for ((transfer, i) <- transfers.zipWithIndex) yield {
            val tsb   = ts + interval * i
            val block = TestBlock.create(tsb, d.lastBlockId, transfer)
            d.appendBlock(block)
            (d.lastBlockId, tsb)
          }

          val middleBlock = blocks(txCount.size / 2)

          d.removeAfter(middleBlock._1)

          try {
            d.appendBlock(
              TestBlock.create(
                middleBlock._2 + 10,
                middleBlock._1,
                transfers(0)
              ))
            throw new Exception("Duplicate transaction wasn't checked")
          } catch {
            case e: Throwable => Assertions.assert(e.getMessage().contains("AlreadyInTheState"))
          }
        }
    }
  }
}
