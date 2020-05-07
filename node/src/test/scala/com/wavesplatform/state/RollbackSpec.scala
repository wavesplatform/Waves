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
import com.wavesplatform.transaction.assets.{IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, GenesisTransaction, Transaction, TxVersion}
import com.wavesplatform.utils.StringBytes
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
    TransferTransaction.selfSigned(1.toByte, sender, recipient, Waves, amount, Waves, 1, None, nextTs).explicitGet()

  private def randomOp(sender: KeyPair, recipient: Address, amount: Long, op: Int, nextTs: => Long = nextTs) = {
    import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
    op match {
      case 1 =>
        val lease = LeaseTransaction.selfSigned(1.toByte, sender, recipient, amount, 100000, nextTs).explicitGet()
        List(lease, LeaseCancelTransaction.signed(1.toByte, sender.publicKey, lease.id(), 1, nextTs, sender.privateKey).explicitGet())
      case 2 =>
        List(
          MassTransferTransaction
            .selfSigned(
              1.toByte,
              sender,
              Waves,
              List(ParsedTransfer(recipient, amount), ParsedTransfer(recipient, amount)),
              10000,
              nextTs,
              None
            )
            .explicitGet()
        )
      case _ => List(TransferTransaction.selfSigned(1.toByte, sender, recipient, Waves, amount, Waves, 1000, None, nextTs).right.get)
    }
  }

  def nonEmptyStringGen(lb: Int, ub: Int): Gen[String] = {
    for {
      len <- Gen.chooseNum(lb, ub)
      arr <- Gen.containerOfN[Array, Char](len, Gen.alphaNumChar)
    } yield String.copyValueOf(arr)
  }

  "Rollback resets" - {
    "Rollback save dropped blocks order" in forAll(accountGen, positiveLongGen, Gen.choose(1, 10)) {
      case (sender, initialBalance, blocksCount) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val genesisSignature = d.lastBlockId
          def newBlocks(i: Int): List[ByteStr] = {
            if (i == blocksCount) {
              Nil
            } else {
              val block = TestBlock.create(nextTs + i, d.lastBlockId, Seq())
              d.appendBlock(block)
              block.id() :: newBlocks(i + 1)
            }
          }
          val blocks        = newBlocks(0)
          val droppedBlocks = d.removeAfter(genesisSignature).map(_._1)
          droppedBlocks(0).header.reference shouldBe genesisSignature
          droppedBlocks.map(_.id()).toList shouldBe blocks
          droppedBlocks foreach d.appendBlock
        }
    }

    "forget rollbacked transaction for querying" in forAll(accountGen, accountGen, Gen.nonEmptyListOf(Gen.choose(1, 10))) {
      case (sender, recipient, txCount) =>
        withDomain(createSettings(MassTransfer -> 0)) { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, com.wavesplatform.state.diffs.ENOUGH_AMT))

          val genesisSignature = d.lastBlockId

          val transferAmount = 100

          val transfers = txCount.map(tc => Seq.fill(tc)(randomOp(sender, recipient.toAddress, transferAmount, tc % 3)).flatten)

          for (transfer <- transfers) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                transfer
              )
            )
          }

          val stransactions1 = d.addressTransactions(sender.toAddress).sortBy(_._2.timestamp)
          val rtransactions1 = d.addressTransactions(recipient.toAddress).sortBy(_._2.timestamp)

          d.removeAfter(genesisSignature)

          for (transfer <- transfers) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                transfer
              )
            )
          }

          val stransactions2 = d.addressTransactions(sender.toAddress).sortBy(_._2.timestamp)
          val rtransactions2 = d.addressTransactions(recipient.toAddress).sortBy(_._2.timestamp)

          stransactions1 shouldBe stransactions2
          rtransactions1 shouldBe rtransactions2
        }
    }

    "waves balances" in forAll(accountGen, positiveLongGen, accountGen, Gen.nonEmptyListOf(Gen.choose(1, 10))) {
      case (sender, initialBalance, recipient, txCount) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))

          val genesisSignature = d.lastBlockId

          d.balance(sender.toAddress) shouldBe initialBalance
          d.balance(recipient.toAddress) shouldBe 0

          val totalTxCount   = txCount.sum
          val transferAmount = initialBalance / (totalTxCount * 2)

          for (tc <- txCount) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                Seq.fill(tc)(transfer(sender, recipient.toAddress, transferAmount))
              )
            )
          }

          d.balance(recipient.toAddress) shouldBe (transferAmount * totalTxCount)
          d.balance(sender.toAddress) shouldBe (initialBalance - (transferAmount + 1) * totalTxCount)

          d.removeAfter(genesisSignature)

          d.balance(sender.toAddress) shouldBe initialBalance
          d.balance(recipient.toAddress) shouldBe 0
        }
    }

    "lease balances and states" in forAll(accountGen, positiveLongGen, accountGen) {
      case (sender, initialBalance, recipient) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          d.blockchainUpdater.height shouldBe 1
          val genesisBlockId = d.lastBlockId

          val leaseAmount = initialBalance - 2
          val lt          = LeaseTransaction.selfSigned(1.toByte, sender, recipient.toAddress, leaseAmount, 1, nextTs).explicitGet()
          d.appendBlock(TestBlock.create(nextTs, genesisBlockId, Seq(lt)))
          d.blockchainUpdater.height shouldBe 2
          val blockWithLeaseId = d.lastBlockId
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender.publicKey, recipient.toAddress, 2, leaseAmount, isActive = true))
          d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual leaseAmount
          d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual leaseAmount

          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockWithLeaseId,
              Seq(LeaseCancelTransaction.signed(1.toByte, sender.publicKey, lt.id(), 1, nextTs, sender.privateKey).explicitGet())
            )
          )
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender.publicKey, recipient.toAddress, 2, leaseAmount, isActive = false))
          d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual 0
          d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual 0

          d.removeAfter(blockWithLeaseId)
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender.publicKey, recipient.toAddress, 2, leaseAmount, isActive = true))
          d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual leaseAmount
          d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual leaseAmount

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.leaseDetails(lt.id()) shouldBe 'empty
          d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual 0
          d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual 0
        }
    }

    "asset balances" in forAll(accountGen, positiveLongGen, positiveLongGen, accountGen) {
      case (sender, initialBalance, assetAmount, recipient) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val genesisBlockId = d.lastBlockId
          val issueTransaction =
            IssueTransaction(TxVersion.V1, sender.publicKey, "test".utf8Bytes, Array.emptyByteArray, assetAmount, 8, reissuable = true, script = None, 1, nextTs)
              .signWith(sender.privateKey)

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            )
          )

          val blockIdWithIssue = d.lastBlockId

          d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) should be(assetAmount)
          d.balance(recipient.toAddress, IssuedAsset(issueTransaction.id())) shouldBe 0

          d.appendBlock(
            TestBlock.create(
              nextTs,
              d.lastBlockId,
              Seq(
                TransferTransaction
                  .selfSigned(1.toByte, sender, recipient.toAddress, IssuedAsset(issueTransaction.id()), assetAmount, Waves, 1, None, nextTs)
                  .explicitGet()
              )
            )
          )

          d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual 0
          d.balance(recipient.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual assetAmount

          d.removeAfter(blockIdWithIssue)

          d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual assetAmount
          d.balance(recipient.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual 0
        }
    }


    "asset quantity and reissuability" in forAll(accountGen, positiveLongGen, nonEmptyStringGen(4, 16), nonEmptyStringGen(0, 1000)) {
      case (sender, initialBalance, name, description) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val genesisBlockId = d.lastBlockId

          val issueTransaction =
            IssueTransaction(TxVersion.V1, sender.publicKey, name.utf8Bytes, description.utf8Bytes, 2000, 8.toByte, reissuable = true, script = None, 1, nextTs)
              .signWith(sender.privateKey)
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe 'empty

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            )
          )

          val blockIdWithIssue = d.lastBlockId

          val actualDesc = d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id()))
          val nameBytes = name.toByteString
          val descriptionBytes = description.toByteString
          val desc1 = AssetDescription(issueTransaction.id(), sender.publicKey, nameBytes, descriptionBytes, 8, reissuable = true, BigInt(2000), Height @@ 2, None, 0, false)
          actualDesc shouldBe Some(desc1)

          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockIdWithIssue,
              Seq(
                ReissueTransaction.selfSigned(1.toByte, sender, IssuedAsset(issueTransaction.id()), 2000, reissuable = false, 1, nextTs).explicitGet()
              )
            )
          )

          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) should contain(
            AssetDescription(issueTransaction.id(), sender.publicKey, nameBytes, descriptionBytes, 8, reissuable = false, BigInt(4000), Height @@ 2, None, 0, false)
          )

          d.removeAfter(blockIdWithIssue)
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) should contain(
            AssetDescription(issueTransaction.id(), sender.publicKey, nameBytes, descriptionBytes, 8, reissuable = true, BigInt(2000), Height @@ 2, None, 0, false)
          )

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe 'empty
        }
    }

    "aliases" in forAll(accountGen, positiveLongGen, aliasGen) {
      case (sender, initialBalance, alias) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val genesisBlockId = d.lastBlockId

          d.blockchainUpdater.resolveAlias(alias) shouldBe Left(AliasDoesNotExist(alias))
          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(CreateAliasTransaction.selfSigned(1.toByte, sender, alias, 1, nextTs).explicitGet())
            )
          )

          d.blockchainUpdater.resolveAlias(alias) shouldBe Right(sender.toAddress)
          d.removeAfter(genesisBlockId)

          d.blockchainUpdater.resolveAlias(alias) shouldBe Left(AliasDoesNotExist(alias))
        }
    }

    "data transaction" in forAll(accountGen, positiveLongGen, dataEntryGen(1000)) {
      case (sender, initialBalance, dataEntry) =>
        withDomain(createSettings(BlockchainFeatures.DataTransaction -> 0)) { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val genesisBlockId = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(DataTransaction.selfSigned(1.toByte, sender, List(dataEntry), 1, nextTs).explicitGet())
            )
          )

          d.blockchainUpdater.accountData(sender.toAddress, dataEntry.key) should contain(dataEntry)

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.accountData(sender.toAddress, dataEntry.key) shouldBe 'empty
        }
    }

    "address script" in forAll(accountGen, positiveLongGen) {
      case (sender, initialBalance) =>
        withDomain(createSettings(SmartAccounts -> 0)) { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val script = ExprScript(TRUE).explicitGet()

          val genesisBlockId = d.lastBlockId
          d.blockchainUpdater.accountScript(sender.toAddress) shouldBe 'empty
          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(SetScriptTransaction.selfSigned(1.toByte, sender, Some(script), 400000, nextTs).explicitGet())
            )
          )

          val blockWithScriptId = d.lastBlockId

          d.blockchainUpdater.accountScript(sender.toAddress) should contain(AccountScriptInfo(sender.publicKey, script, 1))

          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockWithScriptId,
              Seq(SetScriptTransaction.selfSigned(1.toByte, sender, None, 800000, nextTs).explicitGet())
            )
          )

          d.blockchainUpdater.accountScript(sender.toAddress) shouldBe 'empty

          d.removeAfter(blockWithScriptId)
          d.blockchainUpdater.accountScript(sender.toAddress) should contain(AccountScriptInfo(sender.publicKey, script, 1))

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.accountScript(sender.toAddress) shouldBe 'empty
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
      sponsorship <- sponsorFeeCancelSponsorFeeGen(sender, reducedFee = false)
    } yield {
      (sender, sponsorship)
    }) {
      case (sender, (issueTransaction, sponsor1, sponsor2, cancel)) =>
        val ts = issueTransaction.timestamp
        withDomain(createSettings(FeeSponsorship -> 0)) { d =>
          d.appendBlock(genesisBlock(ts, sender.toAddress, Long.MaxValue / 3))
          val genesisBlockId = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              ts,
              genesisBlockId,
              Seq(issueTransaction)
            )
          )

          val blockIdWithIssue = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              ts + 2,
              d.lastBlockId,
              Seq(sponsor1)
            )
          )

          val blockIdWithSponsor = d.lastBlockId

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor1.minSponsoredAssetFee.get
          d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual issueTransaction.quantity

          d.appendBlock(
            TestBlock.create(
              ts + 2,
              d.lastBlockId,
              Seq(cancel)
            )
          )

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe 0

          d.removeAfter(blockIdWithSponsor)

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor1.minSponsoredAssetFee.get
          d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual issueTransaction.quantity

          d.appendBlock(
            TestBlock.create(
              ts + 2,
              d.lastBlockId,
              Seq(sponsor2)
            )
          )

          d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual issueTransaction.quantity
          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor2.minSponsoredAssetFee.get

          d.removeAfter(blockIdWithIssue)

          d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe 0
        }
    }

    "carry fee" in forAll(for {
      sender      <- accountGen
      sponsorship <- sponsorFeeCancelSponsorFeeGen(sender, reducedFee = false)
      transfer    <- transferGeneratorP(sponsorship._1.timestamp, sender, sender.toAddress, 10000000000L)
    } yield {
      (sender, sponsorship, transfer)
    }) {
      case (sender, (issue, sponsor1, sponsor2, _), transfer) =>
        withDomain(createSettings(NG -> 0, FeeSponsorship -> 0)) { d =>
          val ts = issue.timestamp
          def appendBlock(tx: Transaction): ByteStr = {
            d.appendBlock(TestBlock.create(ts, d.lastBlockId, Seq(tx)))
            d.lastBlockId
          }
          def carry(fee: Long): Long = fee - fee / 5 * 2

          d.appendBlock(genesisBlock(ts, sender.toAddress, Long.MaxValue / 3))
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

          d.appendBlock(genesisBlock(ts, sender.toAddress, com.wavesplatform.state.diffs.ENOUGH_AMT))

          val transferAmount = 100

          val interval = (3 * 60 * 60 * 1000 + 30 * 60 * 1000) / txCount.size

          val transfers =
            txCount.zipWithIndex.map(
              tc => Range(0, tc._1).flatMap(i => randomOp(sender, recipient.toAddress, transferAmount, tc._1 % 3, ts + interval * tc._2 + i))
            )

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
              )
            )
            throw new Exception("Duplicate transaction wasn't checked")
          } catch {
            case e: Throwable => Assertions.assert(e.getMessage().contains("AlreadyInTheState"))
          }
        }
    }
  }
}
