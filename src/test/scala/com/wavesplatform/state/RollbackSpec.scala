package com.wavesplatform.state

import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.db.WithState
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.{Address, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.{IssueTransactionV1, ReissueTransactionV1}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransactionV1}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.transfer._
import scorex.transaction.{CreateAliasTransaction, DataTransaction, GenesisTransaction}

class RollbackSpec extends FreeSpec with Matchers with WithState with TransactionGen with PropertyChecks with NoShrink {
  private val time   = new TestTime
  private def nextTs = time.getTimestamp()

  private def genesisBlock(genesisTs: Long, address: Address, initialBalance: Long) = TestBlock.create(
    genesisTs,
    ByteStr(Array.fill[Byte](SignatureLength)(0)),
    Seq(GenesisTransaction.create(address, initialBalance, genesisTs).explicitGet())
  )

  private def transfer(sender: PrivateKeyAccount, recipient: Address, amount: Long) =
    TransferTransactionV1.create(None, sender, recipient, amount, nextTs, None, 1, Array.empty[Byte]).explicitGet()

  "Rollback resets" - {
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
          val genesisBlockId = d.lastBlockId

          val leaseAmount = initialBalance - 2
          val lt          = LeaseTransactionV1.create(sender, leaseAmount, 1, nextTs, recipient).explicitGet()
          d.appendBlock(TestBlock.create(nextTs, genesisBlockId, Seq(lt)))
          val blockWithLeaseId = d.lastBlockId
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender, recipient, 2, leaseAmount, true))
          d.portfolio(sender).lease.out shouldEqual leaseAmount
          d.portfolio(recipient).lease.in shouldEqual leaseAmount

          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockWithLeaseId,
              Seq(LeaseCancelTransaction.create(sender, lt.id(), 1, nextTs).explicitGet())
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
          val genesisBlockId   = d.lastBlockId
          val issueTransaction = IssueTransactionV1.create(sender, "test".getBytes, Array.empty[Byte], assetAmount, 8, true, 1, nextTs).explicitGet()

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            ))

          val blockIdWithIssue = d.lastBlockId

          d.portfolio(sender).assets.get(issueTransaction.id()) should contain(assetAmount)
          d.portfolio(recipient).assets.get(issueTransaction.id()) shouldBe 'empty

          d.appendBlock(
            TestBlock.create(
              nextTs,
              d.lastBlockId,
              Seq(
                TransferTransactionV1
                  .create(Some(issueTransaction.id()), sender, recipient, assetAmount, nextTs, None, 1, Array.empty[Byte])
                  .explicitGet())
            ))

          d.portfolio(sender).assets.getOrElse(issueTransaction.id(), 0) shouldEqual 0
          d.portfolio(recipient).assets.getOrElse(issueTransaction.id(), 0) shouldEqual assetAmount

          d.removeAfter(blockIdWithIssue)

          d.portfolio(sender).assets.getOrElse(issueTransaction.id(), 0) shouldEqual assetAmount
          d.portfolio(recipient).assets.getOrElse(issueTransaction.id(), 0) shouldEqual 0
        }
    }

    "asset quantity and reissuability" in forAll(accountGen, positiveLongGen, byteArrayGen(10), byteArrayGen(12)) {
      case (sender, initialBalance, name, description) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          val issueTransaction = IssueTransactionV1.create(sender, name, description, 2000, 8, true, 1, nextTs).explicitGet()
          d.blockchainUpdater.assetDescription(issueTransaction.id()) shouldBe 'empty

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            ))

          val blockIdWithIssue = d.lastBlockId

          d.blockchainUpdater.assetDescription(issueTransaction.id()) should contain(
            AssetDescription(sender, name, description, 8, true, BigInt(2000), None, 0))

          d.appendBlock(
            TestBlock.create(nextTs,
                             blockIdWithIssue,
                             Seq(
                               ReissueTransactionV1.create(sender, issueTransaction.id(), 2000, false, 1, nextTs).explicitGet()
                             )))

          d.blockchainUpdater.assetDescription(issueTransaction.id()) should contain(
            AssetDescription(sender, name, description, 8, false, BigInt(4000), None, 0))

          d.removeAfter(blockIdWithIssue)
          d.blockchainUpdater.assetDescription(issueTransaction.id()) should contain(
            AssetDescription(sender, name, description, 8, true, BigInt(2000), None, 0))

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.assetDescription(issueTransaction.id()) shouldBe 'empty
        }
    }

    "aliases" in forAll(accountGen, positiveLongGen, aliasGen) {
      case (sender, initialBalance, alias) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          d.blockchainUpdater.resolveAlias(alias) shouldBe 'empty
          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(CreateAliasTransaction.create(sender, alias, 1, nextTs).explicitGet())
            ))

          d.blockchainUpdater.resolveAlias(alias) should contain(sender.toAddress)
          d.removeAfter(genesisBlockId)

          d.blockchainUpdater.resolveAlias(alias) shouldBe 'empty
        }
    }

    "data transaction" in pendingUntilFixed(forAll(accountGen, positiveLongGen, dataEntryGen) {
      case (sender, initialBalance, dataEntry) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(DataTransaction.selfSigned(1, sender, List(dataEntry), 1, nextTs).explicitGet())
            ))

          d.blockchainUpdater.accountData(sender, dataEntry.key) should contain(dataEntry)

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.accountData(sender, dataEntry.key) shouldBe 'empty
        }
    })

    "address script" in pendingUntilFixed(forAll(accountGen, positiveLongGen, scriptGen) {
      case (sender, initialBalance, script) =>
        withDomain() {
          d =>
            d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
            val genesisBlockId = d.lastBlockId

            d.blockchainUpdater.accountScript(sender) shouldBe 'empty
            d.appendBlock(
              TestBlock.create(
                nextTs,
                genesisBlockId,
                Seq(SetScriptTransaction.selfSigned(1, sender, Some(script), 1, nextTs).explicitGet())
              ))

            val blockWithScriptId = d.lastBlockId

            d.blockchainUpdater.accountScript(sender) should contain(script)

            d.appendBlock(
              TestBlock.create(
                nextTs,
                genesisBlockId,
                Seq(SetScriptTransaction.selfSigned(1, sender, None, 1, nextTs).explicitGet())
              ))

            d.blockchainUpdater.accountScript(sender) shouldBe 'empty

            d.removeAfter(blockWithScriptId)
            d.blockchainUpdater.accountScript(sender) should contain(script)

            d.removeAfter(genesisBlockId)
            d.blockchainUpdater.accountScript(sender) shouldBe 'empty
        }
    })
  }
}
