package com.wavesplatform.state

import com.wavesplatform.account.{Address, Alias, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.*
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.history
import com.wavesplatform.history.Domain
import com.wavesplatform.it.util.AddressOrAliasExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.traits.domain.Lease
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.{InvokeTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxNonNegativeAmount, TxVersion}
import org.scalatest.{Assertion, Assertions}

class RollbackSpec extends FreeSpec with WithDomain {
  private val time   = new TestTime
  private def nextTs = time.getTimestamp()

  private def randomOp(sender: KeyPair, recipient: Address, amount: Long, op: Int, nextTs: => Long = nextTs) = {
    import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
    op match {
      case 1 =>
        val lease       = TxHelpers.lease(sender, recipient, amount, fee = 100000L, timestamp = nextTs, version = TxVersion.V1)
        val cancelLease = TxHelpers.leaseCancel(lease.id(), sender, fee = 1, timestamp = nextTs, version = TxVersion.V1)
        List(lease, cancelLease)
      case 2 =>
        List(
          TxHelpers.massTransfer(
            sender,
            Seq(ParsedTransfer(recipient, TxNonNegativeAmount.unsafeFrom(amount)), ParsedTransfer(recipient, TxNonNegativeAmount.unsafeFrom(amount))),
            fee = 10000,
            timestamp = nextTs,
            version = TxVersion.V1
          )
        )
      case _ =>
        List(TxHelpers.transfer(sender, recipient, amount, fee = 1000, timestamp = nextTs, version = TxVersion.V1))
    }
  }

  "NODE-1143, NODE-1144. Rollback resets" - {
    "Rollback save dropped blocks order" in {
      val sender         = TxHelpers.signer(1)
      val initialBalance = 100.waves
      val blocksCount    = 10
      withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        val genesisSignature = d.lastBlockId

        def newBlocks(i: Int): List[ByteStr] = {
          if (i == blocksCount) {
            Nil
          } else {
            val block = TestBlock.create(nextTs + i, d.lastBlockId, Seq()).block
            d.appendBlock(block)
            block.id() :: newBlocks(i + 1)
          }
        }

        val blocks        = newBlocks(0)
        val droppedBlocks = d.rollbackTo(genesisSignature).map(_._1)
        droppedBlocks(0).header.reference shouldBe genesisSignature
        droppedBlocks.map(_.id()).toList shouldBe blocks
        droppedBlocks.foreach { block =>
          d.appendBlockE(block) should beRight
        }
      }
    }

    "forget rollbacked transaction for querying" in {
      val sender    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)
      val txCount   = (1 to 10).toList
      withDomain(createSettings(MassTransfer -> 0), Seq(AddrWithBalance(sender.toAddress))) { d =>
        val genesisSignature = d.lastBlockId

        val transferAmount = 100

        val transfers = txCount.map(tc => Seq.fill(tc)(randomOp(sender, recipient.toAddress, transferAmount, tc % 3)).flatten)

        for (transfer <- transfers) {
          d.appendBlock(
            TestBlock
              .create(
                nextTs,
                d.lastBlockId,
                transfer
              )
              .block
          )
        }

        val stransactions1 = d.addressTransactions(sender.toAddress).sortBy(_._2.timestamp)
        val rtransactions1 = d.addressTransactions(recipient.toAddress).sortBy(_._2.timestamp)

        d.rollbackTo(genesisSignature)

        for (transfer <- transfers) {
          d.appendBlock(
            TestBlock
              .create(
                nextTs,
                d.lastBlockId,
                transfer
              )
              .block
          )
        }

        val stransactions2 = d.addressTransactions(sender.toAddress).sortBy(_._2.timestamp)
        val rtransactions2 = d.addressTransactions(recipient.toAddress).sortBy(_._2.timestamp)

        stransactions1 shouldBe stransactions2
        rtransactions1 shouldBe rtransactions2
      }
    }

    "waves balances" in {
      val sender         = TxHelpers.signer(1)
      val recipient      = TxHelpers.signer(2)
      val txCount        = (1 to 10).toList
      val initialBalance = 100.waves
      val fee            = 1
      withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        val genesisSignature = d.lastBlockId

        d.balance(sender.toAddress) shouldBe initialBalance
        d.balance(recipient.toAddress) shouldBe 0

        val totalTxCount   = txCount.sum
        val transferAmount = initialBalance / (totalTxCount * 2)

        for (tc <- txCount) {
          d.appendBlock(
            TestBlock
              .create(
                nextTs,
                d.lastBlockId,
                Seq.fill(tc)(TxHelpers.transfer(sender, recipient.toAddress, transferAmount, fee = fee, version = TxVersion.V1))
              )
              .block
          )
        }

        d.balance(recipient.toAddress) shouldBe (transferAmount * totalTxCount)
        d.balance(sender.toAddress) shouldBe (initialBalance - (transferAmount + fee) * totalTxCount)

        d.rollbackTo(genesisSignature)

        d.balance(sender.toAddress) shouldBe initialBalance
        d.balance(recipient.toAddress) shouldBe 0
      }
    }

    "lease balances and states" in {
      val sender         = TxHelpers.signer(1)
      val recipient      = TxHelpers.signer(2)
      val initialBalance = 100.waves
      withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        d.blockchainUpdater.height shouldBe 1
        val genesisBlockId = d.lastBlockId

        val leaseAmount = initialBalance - 2
        val lt          = TxHelpers.lease(sender, recipient.toAddress, leaseAmount, version = TxVersion.V1)
        d.appendBlock(TestBlock.create(nextTs, genesisBlockId, Seq(lt)).block)
        d.blockchainUpdater.height shouldBe 2
        val blockWithLeaseId = d.lastBlockId
        d.blockchainUpdater.leaseDetails(lt.id()) should contain(
          LeaseDetails(sender.publicKey, recipient.toAddress, leaseAmount, LeaseDetails.Status.Active, lt.id(), 2)
        )
        d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual leaseAmount
        d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual leaseAmount

        val leaseCancel = TxHelpers.leaseCancel(lt.id(), sender, version = TxVersion.V1)
        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              blockWithLeaseId,
              Seq(leaseCancel)
            )
            .block
        )
        d.blockchainUpdater.leaseDetails(lt.id()) should contain(
          LeaseDetails(
            sender.publicKey,
            recipient.toAddress,
            leaseAmount,
            LeaseDetails.Status.Cancelled(d.blockchain.height, Some(leaseCancel.id())),
            lt.id(),
            2
          )
        )
        d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual 0
        d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual 0

        d.rollbackTo(blockWithLeaseId)
        d.blockchainUpdater.leaseDetails(lt.id()) should contain(
          LeaseDetails(sender.publicKey, recipient.toAddress, leaseAmount, LeaseDetails.Status.Active, lt.id(), 2)
        )
        d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual leaseAmount
        d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual leaseAmount

        d.rollbackTo(genesisBlockId)
        d.blockchainUpdater.leaseDetails(lt.id()) shouldBe empty
        d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual 0
        d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual 0
      }
    }

    "asset balances" in {
      val sender         = TxHelpers.signer(1)
      val recipient      = TxHelpers.signer(2)
      val initialBalance = 100.waves
      val assetAmount    = 100
      withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        val genesisBlockId   = d.lastBlockId
        val issueTransaction = TxHelpers.issue(sender, assetAmount, version = TxVersion.V1)

        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            )
            .block
        )

        val blockIdWithIssue = d.lastBlockId

        d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) should be(assetAmount)
        d.balance(recipient.toAddress, IssuedAsset(issueTransaction.id())) shouldBe 0

        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              d.lastBlockId,
              Seq(
                TransferTransaction
                  .selfSigned(1.toByte, sender, recipient.toAddress, IssuedAsset(issueTransaction.id()), assetAmount, Waves, 1, ByteStr.empty, nextTs)
                  .explicitGet()
              )
            )
            .block
        )

        d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual 0
        d.balance(recipient.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual assetAmount

        d.rollbackTo(blockIdWithIssue)

        d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual assetAmount
        d.balance(recipient.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual 0
      }
    }

    "asset quantity and reissuability" in {
      val sender         = TxHelpers.signer(1)
      val initialBalance = 100.waves
      withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        val genesisBlockId = d.lastBlockId

        val issueTransaction = TxHelpers.issue(sender, amount = 2000, decimals = 8, version = TxVersion.V1)
        d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe empty

        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            )
            .block
        )

        val blockIdWithIssue = d.lastBlockId

        val actualDesc       = d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id()))
        val nameBytes        = issueTransaction.name
        val descriptionBytes = issueTransaction.description
        val desc1 = AssetDescription(
          issueTransaction.id(),
          sender.publicKey,
          nameBytes,
          descriptionBytes,
          8,
          reissuable = true,
          BigInt(2000),
          Height @@ 2,
          None,
          0,
          false,
          1,
          Height(2)
        )
        actualDesc shouldBe Some(desc1)

        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              blockIdWithIssue,
              Seq(TxHelpers.reissue(issueTransaction.asset, sender, 2000, reissuable = false, version = TxVersion.V1))
            )
            .block
        )

        d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) should contain(
          AssetDescription(
            issueTransaction.id(),
            sender.publicKey,
            nameBytes,
            descriptionBytes,
            8,
            reissuable = false,
            BigInt(4000),
            Height @@ 2,
            None,
            0,
            false,
            1,
            Height(2)
          )
        )

        d.rollbackTo(blockIdWithIssue)
        d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) should contain(
          AssetDescription(
            issueTransaction.id(),
            sender.publicKey,
            nameBytes,
            descriptionBytes,
            8,
            reissuable = true,
            BigInt(2000),
            Height @@ 2,
            None,
            0,
            false,
            1,
            Height(2)
          )
        )

        d.rollbackTo(genesisBlockId)
        d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe empty
      }
    }

    "aliases" in {
      val sender         = TxHelpers.signer(1)
      val initialBalance = 100.waves
      val alias          = Alias.create("alias").explicitGet()
      withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        val genesisBlockId = d.lastBlockId

        d.blockchainUpdater.resolveAlias(alias) shouldBe Left(AliasDoesNotExist(alias))
        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              genesisBlockId,
              Seq(TxHelpers.createAlias(alias.name, sender))
            )
            .block
        )

        d.blockchainUpdater.resolveAlias(alias) shouldBe Right(sender.toAddress)
        d.rollbackTo(genesisBlockId)

        d.blockchainUpdater.resolveAlias(alias) shouldBe Left(AliasDoesNotExist(alias))
      }
    }

    "data transaction" in {
      val sender         = TxHelpers.signer(1)
      val initialBalance = 100.waves
      val dataEntry      = StringDataEntry("str", "test-1")
      withDomain(createSettings(BlockchainFeatures.DataTransaction -> 0), Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        val genesisBlockId = d.lastBlockId

        val firstBlock = TestBlock
          .create(
            nextTs,
            genesisBlockId,
            Seq(TxHelpers.dataEntry(sender, dataEntry))
          )
          .block
        d.appendBlock(firstBlock)
        d.blockchainUpdater.accountData(sender.toAddress, dataEntry.key) should contain(dataEntry)

        val secondEntry = StringDataEntry("str", "test-2")
        d.appendBlock(TxHelpers.data(sender, Seq(secondEntry)))
        d.appendBlock()
        d.blockchain.accountData(sender.toAddress, "str") shouldEqual Some(secondEntry)

        d.rollbackTo(firstBlock.id())
        d.blockchainUpdater.accountData(sender.toAddress, dataEntry.key) shouldEqual Some(dataEntry)
      }
    }

    "invoke script transaction actions" - {
      val issueFunctionCall: (Long, Terms.FUNCTION_CALL) =
        (
          Long.MaxValue / 100,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("issue"),
            List(
              Terms.CONST_STRING("name").explicitGet(),
              Terms.CONST_STRING("description").explicitGet(),
              Terms.CONST_LONG(Long.MaxValue / 100),
              Terms.CONST_LONG(8),
              Terms.CONST_BOOLEAN(true)
            )
          )
        )

      def reissueFunctionCall(assetId: ByteStr): (Long, Terms.FUNCTION_CALL) =
        (
          Long.MaxValue / 100,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("reissue"),
            List(
              Terms.CONST_BYTESTR(assetId).explicitGet(),
              Terms.CONST_BOOLEAN(true),
              Terms.CONST_LONG(Long.MaxValue / 100)
            )
          )
        )

      def burnFunctionCall(assetId: ByteStr): (Long, Terms.FUNCTION_CALL) =
        (
          1,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("burn"),
            List(
              Terms.CONST_BYTESTR(assetId).explicitGet(),
              Terms.CONST_LONG(1)
            )
          )
        )

      def sponsorFunctionCall(assetId: ByteStr): (Long, Terms.FUNCTION_CALL) =
        (
          100000L,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("sponsor"),
            List(
              Terms.CONST_BYTESTR(assetId).explicitGet(),
              Terms.CONST_LONG(100000L)
            )
          )
        )

      def leaseFunctionCall(address: Address): (Long, Terms.FUNCTION_CALL) =
        (
          100000L,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("lease"),
            List(
              Terms.CONST_BYTESTR(ByteStr(address.bytes)).explicitGet(),
              Terms.CONST_LONG(100000L)
            )
          )
        )

      def leaseCancelFunctionCall(leaseId: ByteStr): Terms.FUNCTION_CALL =
        Terms.FUNCTION_CALL(
          FunctionHeader.User("leaseCancel"),
          List(
            Terms.CONST_BYTESTR(leaseId).explicitGet()
          )
        )

      def getAsset(d: Domain, txId: ByteStr): IssuedAsset = {
        val sr = d.blockchainUpdater.bestLiquidSnapshot.get.scriptResults(txId)
        sr.error shouldBe empty
        IssuedAsset(sr.issues.head.id)
      }

      val scenario =
        Seq(true, false).map { useInvokeExpression =>
          val dApp                  = TxHelpers.signer(1)
          val sender                = TxHelpers.signer(2)
          val leaseRecipientAddress = TxHelpers.signer(3)
          val setScript             = TxHelpers.setScript(dApp, RollbackSpec.issueReissueBurnScript)

          (dApp, sender, setScript, useInvokeExpression, leaseRecipientAddress)
        }

      def appendBlock(d: Domain, invoker: KeyPair, dApp: KeyPair, ss: Option[SetScriptTransaction])(
          parentBlockId: ByteStr,
          fc: Terms.FUNCTION_CALL
      ): ByteStr = {
        val fee = 150000000L
        val invoke =
          ss.fold[InvokeTransaction](
            TxHelpers.invoke(dApp.toAddress, func = Some(fc.function.funcName), args = fc.args, invoker = invoker, fee = fee)
          )(setScript => diffs.ci.toInvokeExpression(setScript, invoker, Some(fee), Some(fc)))

        d.appendBlock(invoke)
        invoke.id()
      }

      "issue" in {
        scenario.foreach { case (dApp, invoker, setScript, useInvokeExpression, _) =>
          withDomain(
            createSettings(maybeActivateInvokeExpression(useInvokeExpression, Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0, RideV6 -> 0)*),
            Seq(AddrWithBalance(dApp.toAddress), AddrWithBalance(invoker.toAddress))
          ) { d =>
            val (setScriptToConvert, checkAddress) = if (useInvokeExpression) (Some(setScript), invoker.toAddress) else (None, dApp.toAddress)
            val append                             = appendBlock(d, invoker, dApp, setScriptToConvert) _

            d.appendBlock(setScript)

            val startBlockId = d.lastBlockId

            val (quantity, issueFc) = issueFunctionCall

            // / liquid block rollback
            val liquidIssueTxId = append(startBlockId, issueFc)
            val liquidAsset     = getAsset(d, liquidIssueTxId)
            d.balance(checkAddress, liquidAsset) shouldBe quantity
            d.blockchainUpdater.removeAfter(startBlockId).explicitGet()
            d.balance(checkAddress, liquidAsset) shouldBe 0L
            d.blockchainUpdater.assetDescription(liquidAsset) shouldBe None

            // hardened block rollback
            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock()
            d.balance(checkAddress, asset) shouldBe quantity
            d.blockchainUpdater.removeAfter(startBlockId).explicitGet()
            d.balance(checkAddress, asset) shouldBe 0L
            d.blockchainUpdater.assetDescription(asset) shouldBe None
          }
        }
      }

      "reissue" in {
        scenario.foreach { case (dApp, invoker, setScript, useInvokeExpression, _) =>
          withDomain(
            createSettings(maybeActivateInvokeExpression(useInvokeExpression, Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0, RideV6 -> 0)*),
            Seq(AddrWithBalance(dApp.toAddress), AddrWithBalance(invoker.toAddress))
          ) { d =>
            val (setScriptToConvert, checkAddress) = if (useInvokeExpression) (Some(setScript), invoker.toAddress) else (None, dApp.toAddress)
            val append                             = appendBlock(d, invoker, dApp, setScriptToConvert) _

            d.appendBlock(setScript)

            val startBlockId = d.lastBlockId

            val (quantity, issueFc) = issueFunctionCall

            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock()

            val issueBlockId     = d.lastBlockId
            val issueDescription = d.blockchainUpdater.assetDescription(asset)

            val (reissued, reissueFc) = reissueFunctionCall(asset.id)

            // liquid block rollback
            append(issueBlockId, reissueFc)
            d.balance(checkAddress, asset) shouldBe reissued + quantity
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(checkAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription

            // hardened block rollback
            append(issueBlockId, reissueFc)
            d.balance(checkAddress, asset) shouldBe reissued + quantity
            d.appendBlock()
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(checkAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription
          }
        }
      }

      "burn" in {
        scenario.foreach { case (dApp, invoker, setScript, useInvokeExpression, _) =>
          withDomain(
            createSettings(maybeActivateInvokeExpression(useInvokeExpression, Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0, RideV6 -> 0)*),
            AddrWithBalance.enoughBalances(dApp, invoker)
          ) { d =>
            val (setScriptToConvert, checkAddress) = if (useInvokeExpression) (Some(setScript), invoker.toAddress) else (None, dApp.toAddress)
            val append                             = appendBlock(d, invoker, dApp, setScriptToConvert) _
            d.appendBlock(setScript)

            val startBlockId = d.lastBlockId

            val (quantity, issueFc) = issueFunctionCall

            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock()

            val issueBlockId     = d.lastBlockId
            val issueDescription = d.blockchainUpdater.assetDescription(asset)

            val (burnt, burntFc) = burnFunctionCall(asset.id)

            // liquid block rollback
            append(issueBlockId, burntFc)
            d.balance(checkAddress, asset) shouldBe quantity - burnt
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(checkAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription

            // hardened block rollback
            append(issueBlockId, burntFc)
            d.balance(checkAddress, asset) shouldBe quantity - burnt
            d.appendBlock()
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(checkAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription
          }
        }
      }

      "sponsorFee" in {
        scenario.foreach { case (dApp, invoker, setScript, useInvokeExpression, _) =>
          withDomain(
            createSettings(maybeActivateInvokeExpression(useInvokeExpression, Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0, RideV6 -> 0)*),
            AddrWithBalance.enoughBalances(dApp, invoker)
          ) { d =>
            val setScriptToConvert = if (useInvokeExpression) Some(setScript) else None
            val append             = appendBlock(d, invoker, dApp, setScriptToConvert) _

            d.appendBlock(setScript)

            val startBlockId = d.lastBlockId

            val (_, issueFc) = issueFunctionCall

            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock()

            val issueBlockId     = d.lastBlockId
            val issueDescription = d.blockchainUpdater.assetDescription(asset)

            val (sponsorship, sponsorFc) = sponsorFunctionCall(asset.id)

            // liquid block rollback
            append(issueBlockId, sponsorFc)
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe sponsorship
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe 0L

            // hardened block rollback
            append(issueBlockId, sponsorFc)
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe sponsorship
            d.appendBlock()
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe 0L
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription
          }
        }
      }

      "lease" in {
        scenario.foreach { case (dApp, invoker, setScript, useInvokeExpression, leaseRecipientAddress) =>
          withDomain(
            createSettings(maybeActivateInvokeExpression(useInvokeExpression, Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0, RideV6 -> 0)*),
            Seq(AddrWithBalance(dApp.toAddress), AddrWithBalance(invoker.toAddress))
          ) { d =>
            val (setScriptToConvert, checkAddress, checkPk) =
              if (useInvokeExpression) (Some(setScript), invoker.toAddress, invoker.publicKey) else (None, dApp.toAddress, dApp.publicKey)
            val append = appendBlock(d, invoker, dApp, setScriptToConvert) _

            d.appendBlock(setScript)
            val beforeInvoke1 = d.lastBlockId

            val (leaseAmount, leaseFc) = leaseFunctionCall(leaseRecipientAddress.toAddress)

            def leaseDetails(invokeId: ByteStr) =
              Some(LeaseDetails(checkPk, leaseRecipientAddress.toAddress, leaseAmount, LeaseDetails.Status.Active, invokeId, 3))

            // liquid block rollback
            val invokeId1 = append(d.lastBlockId, leaseFc)
            val leaseId1  = Lease.calculateId(Lease(leaseRecipientAddress.toAddress.toRide, leaseAmount, 0), invokeId1)

            d.blockchain.leaseBalance(leaseRecipientAddress.toAddress) shouldBe LeaseBalance(in = leaseAmount, out = 0)
            d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance(in = 0, out = leaseAmount)
            d.blockchain.leaseDetails(leaseId1) shouldBe leaseDetails(invokeId1)
            d.rocksDBWriter.leaseDetails(leaseId1) shouldBe None
            d.appendBlock()
            d.rocksDBWriter.leaseDetails(leaseId1) shouldBe leaseDetails(invokeId1)

            d.blockchain.removeAfter(beforeInvoke1).explicitGet()

            d.blockchain.leaseBalance(leaseRecipientAddress.toAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseDetails(leaseId1) shouldBe None
            d.rocksDBWriter.leaseDetails(leaseId1) shouldBe None

            // hardened block rollback
            val beforeInvoke2 = d.lastBlockId
            val invokeId2     = append(d.lastBlockId, leaseFc)
            val leaseId2      = Lease.calculateId(Lease(leaseRecipientAddress.toAddress.toRide, leaseAmount, 0), invokeId2)

            d.blockchain.leaseBalance(leaseRecipientAddress.toAddress) shouldBe LeaseBalance(in = leaseAmount, out = 0)
            d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance(in = 0, out = leaseAmount)
            d.blockchain.leaseDetails(leaseId2) shouldBe leaseDetails(invokeId2)
            d.rocksDBWriter.leaseDetails(leaseId2) shouldBe None
            d.appendBlock()
            d.rocksDBWriter.leaseDetails(leaseId2) shouldBe leaseDetails(invokeId2)

            d.appendBlock()
            d.blockchain.removeAfter(beforeInvoke2).explicitGet()

            d.blockchain.leaseBalance(leaseRecipientAddress.toAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseDetails(leaseId2) shouldBe None
            d.rocksDBWriter.leaseDetails(leaseId2) shouldBe None
          }
        }
      }

      def assertLeaseCancel(
          dApp: KeyPair,
          invoker: KeyPair,
          d: Domain,
          leaseAmount: Long,
          leaseId: ByteStr,
          leaseHeight: Int,
          sourceId: ByteStr,
          setScriptToConvert: Option[SetScriptTransaction],
          checkAddress: Address,
          checkPk: PublicKey,
          leaseRecipientAddress: Address
      ): Assertion = {
        val append        = appendBlock(d, invoker, dApp, setScriptToConvert) _
        val beforeInvoke1 = d.lastBlockId

        val call = leaseCancelFunctionCall(leaseId)

        def leaseDetails(leaseHeight: Int, cancelHeight: Int = 0, cancelId: ByteStr = ByteStr.empty) =
          Some(
            LeaseDetails(
              checkPk,
              leaseRecipientAddress,
              leaseAmount,
              if (cancelId.isEmpty) LeaseDetails.Status.Active
              else LeaseDetails.Status.Cancelled(cancelHeight, Some(cancelId)),
              sourceId,
              leaseHeight
            )
          )

        // liquid block rollback
        val leaseCancelId = append(d.lastBlockId, call)
        val cancelHeight  = d.blockchain.transactionMeta(leaseCancelId).get.height

        d.blockchain.leaseBalance(leaseRecipientAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight, cancelHeight, leaseCancelId)
        d.rocksDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight)
        d.appendBlock()
        d.rocksDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight, cancelHeight, leaseCancelId)

        d.blockchain.removeAfter(beforeInvoke1).explicitGet()

        d.blockchain.leaseBalance(leaseRecipientAddress) shouldBe LeaseBalance(in = leaseAmount, 0)
        d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance(0, out = leaseAmount)
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight)
        d.rocksDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight)

        // hardened block rollback
        val beforeInvoke2  = d.lastBlockId
        val leaseCancelId1 = append(d.lastBlockId, call)

        d.blockchain.leaseBalance(leaseRecipientAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight, cancelHeight, leaseCancelId1)
        d.rocksDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight)
        d.appendBlock()
        d.rocksDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight, cancelHeight, leaseCancelId1)

        d.appendBlock()
        d.blockchain.removeAfter(beforeInvoke2).explicitGet()

        d.blockchain.leaseBalance(leaseRecipientAddress) shouldBe LeaseBalance(in = leaseAmount, 0)
        d.blockchain.leaseBalance(checkAddress) shouldBe LeaseBalance(0, out = leaseAmount)
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight)
        d.rocksDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(leaseHeight)
      }

      "leaseCancel with lease tx" in {
        scenario.foreach { case (dApp, invoker, setScript, useInvokeExpression, leaseRecipientAddress) =>
          withDomain(
            createSettings(
              maybeActivateInvokeExpression(
                useInvokeExpression,
                Ride4DApps       -> 0,
                BlockV5          -> 0,
                SmartAccounts    -> 0,
                SynchronousCalls -> 0,
                RideV6           -> 0
              )*
            ),
            Seq(AddrWithBalance(dApp.toAddress), AddrWithBalance(invoker.toAddress))
          ) { d =>
            val (setScriptToConvert, leaseSender) =
              if (useInvokeExpression)
                (Some(setScript), invoker)
              else
                (None, dApp)

            val leaseAmount = smallFeeGen.sample.get
            val leaseTx =
              LeaseTransaction
                .selfSigned(2.toByte, leaseSender, leaseRecipientAddress.toAddress, leaseAmount, setScript.fee.value, nextTs)
                .explicitGet()
            val leaseId = leaseTx.id()

            d.appendBlock(setScript, leaseTx)

            assertLeaseCancel(
              dApp,
              invoker,
              d,
              leaseAmount,
              leaseId,
              2,
              leaseId,
              setScriptToConvert,
              leaseSender.toAddress,
              leaseSender.publicKey,
              leaseRecipientAddress.toAddress
            )
          }
        }
      }

      "leaseCancel with lease action" in {
        scenario.foreach { case (dApp, invoker, setScript, useInvokeExpression, leaseRecipientAddress) =>
          withDomain(
            createSettings(
              maybeActivateInvokeExpression(
                useInvokeExpression,
                Ride4DApps       -> 0,
                BlockV5          -> 0,
                SmartAccounts    -> 0,
                SynchronousCalls -> 0,
                RideV6           -> 0
              )*
            ),
            Seq(AddrWithBalance(dApp.toAddress), AddrWithBalance(invoker.toAddress))
          ) { d =>
            d.appendBlock(setScript)

            val (setScriptToConvert, leaseSender) =
              if (useInvokeExpression)
                (Some(setScript), invoker)
              else
                (None, dApp)

            val (leaseAmount, leaseFc) = leaseFunctionCall(leaseRecipientAddress.toAddress)
            val leaseInvokeId          = appendBlock(d, invoker, dApp, setScriptToConvert)(d.lastBlockId, leaseFc)
            val leaseId                = Lease.calculateId(Lease(leaseRecipientAddress.toAddress.toRide, leaseAmount, 0), leaseInvokeId)

            assertLeaseCancel(
              dApp,
              invoker,
              d,
              leaseAmount,
              leaseId,
              3,
              leaseInvokeId,
              setScriptToConvert,
              leaseSender.toAddress,
              leaseSender.publicKey,
              leaseRecipientAddress.toAddress
            )
          }
        }
      }
    }

    "address script" in {
      val sender         = TxHelpers.signer(1)
      val initialBalance = 100.waves
      withDomain(createSettings(SmartAccounts -> 0), Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        val script = ExprScript(TRUE).explicitGet()

        val genesisBlockId = d.lastBlockId
        d.blockchainUpdater.accountScript(sender.toAddress) shouldBe empty
        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              genesisBlockId,
              Seq(TxHelpers.setScript(sender, script, fee = 400000))
            )
            .block
        )

        val blockWithScriptId = d.lastBlockId

        d.blockchainUpdater.accountScript(sender.toAddress) should contain(AccountScriptInfo(sender.publicKey, script, 1))

        d.appendBlock(
          TestBlock
            .create(
              nextTs,
              blockWithScriptId,
              Seq(SetScriptTransaction.selfSigned(1.toByte, sender, None, 800000, nextTs).explicitGet())
            )
            .block
        )

        d.blockchainUpdater.accountScript(sender.toAddress) shouldBe empty

        d.rollbackTo(blockWithScriptId)
        d.blockchainUpdater.accountScript(sender.toAddress) should contain(AccountScriptInfo(sender.publicKey, script, 1))

        d.rollbackTo(genesisBlockId)
        d.blockchainUpdater.accountScript(sender.toAddress) shouldBe empty
      }
    }

    "asset sponsorship" in {
      val sender           = TxHelpers.signer(1)
      val issueTransaction = TxHelpers.issue(sender, version = TxVersion.V1)
      val sponsor1         = TxHelpers.sponsor(issueTransaction.asset, Some(400000), sender)
      val sponsor2         = TxHelpers.sponsor(issueTransaction.asset, Some(100000000), sender)
      val cancel           = TxHelpers.sponsor(issueTransaction.asset, None, sender)

      val ts = issueTransaction.timestamp
      withDomain(createSettings(FeeSponsorship -> 0), Seq(AddrWithBalance(sender.toAddress))) { d =>
        val genesisBlockId = d.lastBlockId

        d.appendBlock(
          TestBlock
            .create(
              ts,
              genesisBlockId,
              Seq(issueTransaction)
            )
            .block
        )

        val blockIdWithIssue = d.lastBlockId

        d.appendBlock(
          TestBlock
            .create(
              ts + 2,
              d.lastBlockId,
              Seq(sponsor1)
            )
            .block
        )

        val blockIdWithSponsor = d.lastBlockId

        d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor1.minSponsoredAssetFee.get.value
        d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual issueTransaction.quantity.value

        d.appendBlock(
          TestBlock
            .create(
              ts + 2,
              d.lastBlockId,
              Seq(cancel)
            )
            .block
        )

        d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe 0

        d.rollbackTo(blockIdWithSponsor)

        d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor1.minSponsoredAssetFee.get.value
        d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual issueTransaction.quantity.value

        d.appendBlock(
          TestBlock
            .create(
              ts + 2,
              d.lastBlockId,
              Seq(sponsor2)
            )
            .block
        )

        d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual issueTransaction.quantity.value
        d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe sponsor2.minSponsoredAssetFee.get.value

        d.rollbackTo(blockIdWithIssue)

        d.blockchainUpdater.assetDescription(sponsor1.asset).get.sponsorship shouldBe 0
      }
    }

    "carry fee" in {
      val sender   = TxHelpers.signer(1)
      val issue    = TxHelpers.issue(sender, 100, version = TxVersion.V1)
      val sponsor1 = TxHelpers.sponsor(issue.asset, Some(400000), sender)
      val sponsor2 = TxHelpers.sponsor(issue.asset, Some(100000000), sender)
      val transfer = TxHelpers.transfer(sender, sender.toAddress, 10000000000L, version = TxVersion.V1)

      withDomain(createSettings(NG -> 0, FeeSponsorship -> 0), Seq(AddrWithBalance(sender.toAddress))) { d =>
        val ts = issue.timestamp

        def appendBlock(tx: Transaction): ByteStr = {
          d.appendBlock(TestBlock.create(ts, d.lastBlockId, Seq(tx)).block)
          d.lastBlockId
        }

        def carry(fee: Long): Long = fee - fee / 5 * 2

        d.carryFee shouldBe carry(0)

        val issueBlockId = appendBlock(issue)
        d.carryFee shouldBe carry(issue.fee.value)

        val sponsorBlockId = appendBlock(sponsor1)
        d.carryFee shouldBe carry(sponsor1.fee.value)

        appendBlock(transfer)
        d.carryFee shouldBe carry(transfer.fee.value)

        d.rollbackTo(sponsorBlockId)
        d.carryFee shouldBe carry(sponsor1.fee.value)

        d.rollbackTo(issueBlockId)
        d.carryFee shouldBe carry(issue.fee.value)

        val transferBlockId = appendBlock(transfer)
        d.carryFee shouldBe carry(transfer.fee.value)

        appendBlock(sponsor2)
        d.carryFee shouldBe carry(sponsor2.fee.value)

        d.rollbackTo(transferBlockId)
        d.carryFee shouldBe carry(transfer.fee.value)
      }
    }

    "relean rollbacked transaction" in {
      val sender    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)
      val txCount   = (1 to 66).map(_ % 10 + 1).toList
      withDomain(createSettings(MassTransfer -> 0), Seq(AddrWithBalance(sender.toAddress))) { d =>
        val ts = nextTs

        val transferAmount = 100

        val interval = (3 * 60 * 60 * 1000 + 30 * 60 * 1000) / txCount.size

        val transfers =
          txCount.zipWithIndex.map(tc =>
            Range(0, tc._1).flatMap(i => randomOp(sender, recipient.toAddress, transferAmount, tc._1 % 3, ts + interval * tc._2 + i))
          )

        val blocks = for ((transfer, i) <- transfers.zipWithIndex) yield {
          val tsb   = ts + interval * i
          val block = TestBlock.create(tsb, d.lastBlockId, transfer).block
          d.appendBlock(block)
          (d.lastBlockId, tsb)
        }

        val middleBlock = blocks(txCount.size / 2)

        d.rollbackTo(middleBlock._1)

        try {
          d.appendBlock(
            TestBlock
              .create(
                middleBlock._2 + 10,
                middleBlock._1,
                transfers(0)
              )
              .block
          )
          throw new Exception("Duplicate transaction wasn't checked")
        } catch {
          case e: Throwable => Assertions.assert(e.getMessage.contains("AlreadyInTheState"))
        }
      }
    }
  }

  private def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): WavesSettings = {
    val tfs = TestFunctionalitySettings.Enabled.copy(
      featureCheckBlocksPeriod = 1,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }.toMap
    )

    history.DefaultWavesSettings.copy(blockchainSettings = history.DefaultWavesSettings.blockchainSettings.copy(functionalitySettings = tfs))
  }

  private def maybeActivateInvokeExpression(useInvokeExpression: Boolean, preActivatedFeatures: (BlockchainFeature, Int)*) =
    if (useInvokeExpression) {
      preActivatedFeatures :+ ContinuationTransaction -> 0
    } else {
      preActivatedFeatures
    }
}

object RollbackSpec {
  private val issueReissueBurnScript = {
    val stdLibVersion = V5

    val script =
      s"""
         |{-# STDLIB_VERSION ${stdLibVersion.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-#SCRIPT_TYPE ACCOUNT#-}
         |
         |@Callable(i)
         |func issue(name: String, description: String, quantity: Int, decimals: Int, isReissuable: Boolean) =
         |  [Issue(name, description, quantity, decimals, isReissuable, unit, 0)]
         |
         |@Callable(i)
         |func reissue(assetId: ByteVector, isReissuable: Boolean, quantity: Int) =
         |  [Reissue(assetId, quantity, isReissuable)]
         |
         |@Callable(i)
         |func burn(assetId: ByteVector, quantity: Int) =
         |  [Burn(assetId, quantity)]
         |  
         |@Callable(i)
         |func sponsor(assetId: ByteVector, minSponsoredAssetFee: Int) =
         |  [SponsorFee(assetId, minSponsoredAssetFee)]
         |
         |@Callable(i)
         |func lease(address: ByteVector, amount: Int) =
         |  [Lease(Address(address), amount)]
         |
         |@Callable(i)
         |func leaseCancel(leaseId: ByteVector) =
         |  [LeaseCancel(leaseId)]
         |
         |""".stripMargin

    TestCompiler(stdLibVersion).compileContract(script)
  }
}
