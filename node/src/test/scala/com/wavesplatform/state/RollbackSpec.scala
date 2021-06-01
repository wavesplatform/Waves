package com.wavesplatform.state

import com.wavesplatform.{history, NoShrink, TestTime, TransactionGen}
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features._
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.history.Domain
import com.wavesplatform.it.util.AddressOrAliasExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.traits.domain.Lease
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, GenesisTransaction, Transaction, TxVersion}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.assets.{IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.StringBytes
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.alphaLowerChar
import org.scalatest.{Assertion, Assertions, FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class RollbackSpec extends FreeSpec with Matchers with WithDomain with TransactionGen with PropertyChecks with NoShrink {
  private val time   = new TestTime
  private def nextTs = time.getTimestamp()

  private def genesisBlock(genesisTs: Long, address: Address, initialBalance: Long): Block =
    genesisBlock(genesisTs, Map(address -> initialBalance))

  private def genesisBlock(genesisTs: Long, initialBalances: Map[Address, Long]): Block = TestBlock.create(
    genesisTs,
    ByteStr(Array.fill[Byte](SignatureLength)(0)),
    initialBalances.map { case (address, initialBalance) => GenesisTransaction.create(address, initialBalance, genesisTs).explicitGet() }.toSeq
  )

  private def transfer(sender: KeyPair, recipient: Address, amount: Long) =
    TransferTransaction.selfSigned(1.toByte, sender, recipient, Waves, amount, Waves, 1, ByteStr.empty, nextTs).explicitGet()

  private def randomOp(sender: KeyPair, recipient: Address, amount: Long, op: Int, nextTs: => Long = nextTs) = {
    import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
    op match {
      case 1 =>
        val lease = LeaseTransaction.selfSigned(1.toByte, sender, recipient, amount, 100000L, nextTs).explicitGet()
        List(lease, LeaseCancelTransaction.selfSigned(1.toByte, sender, lease.id(), 1, nextTs).explicitGet())
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
              ByteStr.empty
            )
            .explicitGet()
        )
      case _ => List(TransferTransaction.selfSigned(1.toByte, sender, recipient, Waves, amount, Waves, 1000, ByteStr.empty, nextTs).explicitGet())
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
          val droppedBlocks = d.rollbackTo(genesisSignature).map(_._1)
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

          d.rollbackTo(genesisSignature)

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

          d.rollbackTo(genesisSignature)

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
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(
            LeaseDetails(sender.publicKey, recipient.toAddress, leaseAmount, LeaseDetails.Status.Active, lt.id(), 2)
          )
          d.blockchainUpdater.leaseBalance(sender.toAddress).out shouldEqual leaseAmount
          d.blockchainUpdater.leaseBalance(recipient.toAddress).in shouldEqual leaseAmount

          val leaseCancel = LeaseCancelTransaction.selfSigned(1.toByte, sender, lt.id(), 1, nextTs).explicitGet()
          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockWithLeaseId,
              Seq(leaseCancel)
            )
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

    "asset balances" in forAll(accountGen, positiveLongGen, positiveLongGen, accountGen) {
      case (sender, initialBalance, assetAmount, recipient) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val genesisBlockId = d.lastBlockId
          val issueTransaction =
            IssueTransaction(
              TxVersion.V1,
              sender.publicKey,
              "test".utf8Bytes,
              Array.emptyByteArray,
              assetAmount,
              8,
              reissuable = true,
              script = None,
              1,
              nextTs
            ).signWith(sender.privateKey)

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
                  .selfSigned(1.toByte, sender, recipient.toAddress, IssuedAsset(issueTransaction.id()), assetAmount, Waves, 1, ByteStr.empty, nextTs)
                  .explicitGet()
              )
            )
          )

          d.balance(sender.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual 0
          d.balance(recipient.toAddress, IssuedAsset(issueTransaction.id())) shouldEqual assetAmount

          d.rollbackTo(blockIdWithIssue)

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
            IssueTransaction(
              TxVersion.V1,
              sender.publicKey,
              name.utf8Bytes,
              description.utf8Bytes,
              2000,
              8.toByte,
              reissuable = true,
              script = None,
              1,
              nextTs
            ).signWith(sender.privateKey)
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe empty

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(issueTransaction)
            )
          )

          val blockIdWithIssue = d.lastBlockId

          val actualDesc       = d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id()))
          val nameBytes        = name.toByteString
          val descriptionBytes = description.toByteString
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
            false
          )
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
              false
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
              false
            )
          )

          d.rollbackTo(genesisBlockId)
          d.blockchainUpdater.assetDescription(IssuedAsset(issueTransaction.id())) shouldBe empty
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
          d.rollbackTo(genesisBlockId)

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

          d.rollbackTo(genesisBlockId)
          d.blockchainUpdater.accountData(sender.toAddress, dataEntry.key) shouldBe empty
        }
    }

    "invoke script transaction actions" - {
      val issueFunctionCallGen: Gen[(Long, Terms.FUNCTION_CALL)] =
        for {
          nameLen        <- Gen.choose(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
          descriptionLen <- Gen.choose(0, IssueTransaction.MaxAssetDescriptionLength)
          name           <- Gen.listOfN(nameLen, alphaLowerChar).map(_.mkString)
          description    <- Gen.listOfN(descriptionLen, alphaLowerChar).map(_.mkString)
          quantity       <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
          decimals       <- Gen.choose(0: Byte, 8: Byte)
        } yield (
          quantity,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("issue"),
            List(
              Terms.CONST_STRING(name).explicitGet(),
              Terms.CONST_STRING((description)).explicitGet(),
              Terms.CONST_LONG(quantity),
              Terms.CONST_LONG(decimals),
              Terms.CONST_BOOLEAN(true)
            )
          )
        )

      def reissueFunctionCallGen(assetId: ByteStr): Gen[(Long, Terms.FUNCTION_CALL)] =
        for {
          reissuable <- Arbitrary.arbBool.arbitrary
          quantity   <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
        } yield (
          quantity,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("reissue"),
            List(
              Terms.CONST_BYTESTR(assetId).explicitGet(),
              Terms.CONST_BOOLEAN(reissuable),
              Terms.CONST_LONG(quantity)
            )
          )
        )

      def burnFunctionCallGen(assetId: ByteStr, quantity: Long): Gen[(Long, Terms.FUNCTION_CALL)] =
        for {
          burnt <- Gen.choose(1, quantity)
        } yield (
          burnt,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("burn"),
            List(
              Terms.CONST_BYTESTR(assetId).explicitGet(),
              Terms.CONST_LONG(burnt)
            )
          )
        )

      def sponsorFunctionCallGen(assetId: ByteStr): Gen[(Long, Terms.FUNCTION_CALL)] =
        for {
          minSponsoredAssetFee <- Gen.choose(1L, 100000L)
        } yield (
          minSponsoredAssetFee,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("sponsor"),
            List(
              Terms.CONST_BYTESTR(assetId).explicitGet(),
              Terms.CONST_LONG(minSponsoredAssetFee)
            )
          )
        )

      def leaseFunctionCallGen(address: Address): Gen[(Long, Terms.FUNCTION_CALL)] =
        for {
          leaseAmount <- Gen.choose(1L, 100000L)
        } yield (
          leaseAmount,
          Terms.FUNCTION_CALL(
            FunctionHeader.User("lease"),
            List(
              Terms.CONST_BYTESTR(ByteStr(address.bytes)).explicitGet(),
              Terms.CONST_LONG(leaseAmount)
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
        val sr = d.blockchainUpdater.bestLiquidDiff.get.scriptResults(txId)
        sr.error shouldBe empty
        IssuedAsset(sr.issues.head.id)
      }

      val scenario =
        for {
          dApp                 <- accountGen
          initialDAppBalance   <- positiveLongGen
          sender               <- accountGen
          initialSenderBalance <- positiveLongGen
          fee                  <- smallFeeGen
          _                    <- issueParamGen
          genesis     = genesisBlock(nextTs, Map(dApp.toAddress -> initialDAppBalance, sender.toAddress -> initialSenderBalance))
          setScriptTx = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(RollbackSpec.issueReissueBurnScript), fee, nextTs).explicitGet()
        } yield (dApp, sender, genesis, setScriptTx)

      def appendBlock(d: Domain, invoker: KeyPair, dApp: KeyPair)(parentBlockId: ByteStr, fc: Terms.FUNCTION_CALL): ByteStr = {
        val fee = 150000000L
        val invoke =
          InvokeScriptTransaction
            .selfSigned(2.toByte, invoker, dApp.toAddress, Some(fc), Seq.empty, fee, Waves, nextTs)
            .explicitGet()

        d.appendBlock(
          TestBlock.create(
            nextTs,
            parentBlockId,
            Seq(invoke)
          )
        )
        invoke.id()
      }

      "issue" in forAll(scenario) {
        case (dApp, invoker, genesis, setScript) =>
          withDomain(createSettings(Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0)) { d =>
            val append = appendBlock(d, invoker, dApp) _

            d.appendBlock(genesis)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq(setScript)))

            val startBlockId = d.lastBlockId

            val (quantity, issueFc) = issueFunctionCallGen.sample.get

            /// liquid block rollback
            val liquidIssueTxId = append(startBlockId, issueFc)
            val liquidAsset     = getAsset(d, liquidIssueTxId)
            d.balance(dApp.toAddress, liquidAsset) shouldBe quantity
            d.blockchainUpdater.removeAfter(startBlockId).explicitGet()
            d.balance(dApp.toAddress, liquidAsset) shouldBe 0L
            d.blockchainUpdater.assetDescription(liquidAsset) shouldBe None

            // hardened block rollback
            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq()))
            d.balance(dApp.toAddress, asset) shouldBe quantity
            d.blockchainUpdater.removeAfter(startBlockId).explicitGet()
            d.balance(dApp.toAddress, asset) shouldBe 0L
            d.blockchainUpdater.assetDescription(asset) shouldBe None
          }
      }

      "reissue" in forAll(scenario) {
        case (dApp, invoker, genesis, setScript) =>
          withDomain(createSettings(Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0)) { d =>
            val append = appendBlock(d, invoker, dApp) _

            d.appendBlock(genesis)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq(setScript)))

            val startBlockId = d.lastBlockId

            val (quantity, issueFc) = issueFunctionCallGen.sample.get

            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq()))

            val issueBlockId     = d.lastBlockId
            val issueDescription = d.blockchainUpdater.assetDescription(asset)

            val (reissued, reissueFc) = reissueFunctionCallGen(asset.id).sample.get

            // liquid block rollback
            append(issueBlockId, reissueFc)
            d.balance(dApp.toAddress, asset) shouldBe reissued + quantity
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(dApp.toAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription

            // hardened block rollback
            append(issueBlockId, reissueFc)
            d.balance(dApp.toAddress, asset) shouldBe reissued + quantity
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq()))
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(dApp.toAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription
          }
      }

      "burn" in forAll(scenario) {
        case (dApp, invoker, genesis, setScript) =>
          withDomain(createSettings(Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0)) { d =>
            val append = appendBlock(d, invoker, dApp) _

            d.appendBlock(genesis)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq(setScript)))

            val startBlockId = d.lastBlockId

            val (quantity, issueFc) = issueFunctionCallGen.sample.get

            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq()))

            val issueBlockId     = d.lastBlockId
            val issueDescription = d.blockchainUpdater.assetDescription(asset)

            val (burnt, burntFc) = burnFunctionCallGen(asset.id, quantity).sample.get

            // liquid block rollback
            append(issueBlockId, burntFc)
            d.balance(dApp.toAddress, asset) shouldBe quantity - burnt
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(dApp.toAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription

            // hardened block rollback
            append(issueBlockId, burntFc)
            d.balance(dApp.toAddress, asset) shouldBe quantity - burnt
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq()))
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.balance(dApp.toAddress, asset) shouldBe quantity
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription
          }
      }

      "sponsorFee" in forAll(scenario) {
        case (dApp, invoker, genesis, setScript) =>
          withDomain(createSettings(Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0)) { d =>
            val append = appendBlock(d, invoker, dApp) _

            d.appendBlock(genesis)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq(setScript)))

            val startBlockId = d.lastBlockId

            val (_, issueFc) = issueFunctionCallGen.sample.get

            val issueTxId = append(startBlockId, issueFc)
            val asset     = getAsset(d, issueTxId)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq()))

            val issueBlockId     = d.lastBlockId
            val issueDescription = d.blockchainUpdater.assetDescription(asset)

            val (sponsorship, sponsorFc) = sponsorFunctionCallGen(asset.id).sample.get

            // liquid block rollback
            append(issueBlockId, sponsorFc)
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe sponsorship
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe 0L

            // hardened block rollback
            append(issueBlockId, sponsorFc)
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe sponsorship
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq()))
            d.blockchainUpdater.removeAfter(issueBlockId).explicitGet()
            d.blockchainUpdater.assetDescription(asset).get.sponsorship shouldBe 0L
            d.blockchainUpdater.assetDescription(asset) shouldBe issueDescription
          }
      }

      "lease" in forAll(scenario) {
        case (dApp, invoker, genesis, setScript) =>
          withDomain(createSettings(Ride4DApps -> 0, BlockV5 -> 0, SynchronousCalls -> 0)) { d =>
            val append = appendBlock(d, invoker, dApp) _

            d.appendBlock(genesis)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq(setScript)))
            val beforeInvoke1 = d.lastBlockId

            val (leaseAmount, leaseFc) = leaseFunctionCallGen(invoker.toAddress).sample.get
            def leaseDetails(invokeId: ByteStr) =
              Some(LeaseDetails(dApp.publicKey, invoker.toAddress, leaseAmount, LeaseDetails.Status.Active, invokeId, 3))

            // liquid block rollback
            val invokeId1 = append(d.lastBlockId, leaseFc)
            val leaseId1  = Lease.calculateId(Lease(invoker.toAddress.toRide, leaseAmount, 0), invokeId1)

            d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(in = leaseAmount, out = 0)
            d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance(in = 0, out = leaseAmount)
            d.blockchain.leaseDetails(leaseId1) shouldBe leaseDetails(invokeId1)
            d.levelDBWriter.leaseDetails(leaseId1) shouldBe None
            d.appendBlock()
            d.levelDBWriter.leaseDetails(leaseId1) shouldBe leaseDetails(invokeId1)

            d.blockchain.removeAfter(beforeInvoke1).explicitGet()

            d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseDetails(leaseId1) shouldBe None
            d.levelDBWriter.leaseDetails(leaseId1) shouldBe None

            // hardened block rollback
            val beforeInvoke2 = d.lastBlockId
            val invokeId2     = append(d.lastBlockId, leaseFc)
            val leaseId2      = Lease.calculateId(Lease(invoker.toAddress.toRide, leaseAmount, 0), invokeId2)

            d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(in = leaseAmount, out = 0)
            d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance(in = 0, out = leaseAmount)
            d.blockchain.leaseDetails(leaseId2) shouldBe leaseDetails(invokeId2)
            d.levelDBWriter.leaseDetails(leaseId2) shouldBe None
            d.appendBlock()
            d.levelDBWriter.leaseDetails(leaseId2) shouldBe leaseDetails(invokeId2)

            d.appendBlock()
            d.blockchain.removeAfter(beforeInvoke2).explicitGet()

            d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance.empty
            d.blockchain.leaseDetails(leaseId2) shouldBe None
            d.levelDBWriter.leaseDetails(leaseId2) shouldBe None
          }
      }

      def assertLeaseCancel(dApp: KeyPair, invoker: KeyPair, d: Domain, leaseAmount: Long, leaseId: ByteStr, sourceId: ByteStr): Assertion = {
        val append        = appendBlock(d, invoker, dApp) _
        val beforeInvoke1 = d.lastBlockId

        val call = leaseCancelFunctionCall(leaseId)

        def leaseDetails(cancelHeight: Int = 0, cancelId: ByteStr = ByteStr.empty) =
          Some(
            LeaseDetails(
              dApp.publicKey,
              invoker.toAddress,
              leaseAmount,
              if (cancelId.isEmpty) LeaseDetails.Status.Active
              else LeaseDetails.Status.Cancelled(cancelHeight, Some(cancelId)),
              sourceId,
              2
            )
          )

        // liquid block rollback
        val leaseCancelId     = append(d.lastBlockId, call)
        val (cancelHeight, _) = d.blockchain.transactionMeta(leaseCancelId).get

        d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails(cancelHeight, leaseCancelId)
        d.levelDBWriter.leaseDetails(leaseId) shouldBe leaseDetails()
        d.appendBlock()
        d.levelDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(cancelHeight, leaseCancelId)

        d.blockchain.removeAfter(beforeInvoke1).explicitGet()

        d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(in = leaseAmount, 0)
        d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance(0, out = leaseAmount)
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails()
        d.levelDBWriter.leaseDetails(leaseId) shouldBe leaseDetails()

        // hardened block rollback
        val beforeInvoke2  = d.lastBlockId
        val leaseCancelId1 = append(d.lastBlockId, call)

        d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance.empty
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails(cancelHeight, leaseCancelId1)
        d.levelDBWriter.leaseDetails(leaseId) shouldBe leaseDetails()
        d.appendBlock()
        d.levelDBWriter.leaseDetails(leaseId) shouldBe leaseDetails(cancelHeight, leaseCancelId1)

        d.appendBlock()
        d.blockchain.removeAfter(beforeInvoke2).explicitGet()

        d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(in = leaseAmount, 0)
        d.blockchain.leaseBalance(dApp.toAddress) shouldBe LeaseBalance(0, out = leaseAmount)
        d.blockchain.leaseDetails(leaseId) shouldBe leaseDetails()
        d.levelDBWriter.leaseDetails(leaseId) shouldBe leaseDetails()
      }

      "leaseCancel with lease tx" in forAll(scenario) {
        case (dApp, invoker, genesis, setScript) =>
          withDomain(createSettings(Ride4DApps -> 0, BlockV5 -> 0, SmartAccounts -> 0, SynchronousCalls -> 0)) { d =>
            val leaseAmount = smallFeeGen.sample.get
            val leaseTx     = LeaseTransaction.selfSigned(2.toByte, dApp, invoker.toAddress, leaseAmount, setScript.fee, nextTs).explicitGet()
            val leaseId     = leaseTx.id.value()

            d.appendBlock(genesis)
            d.appendBlock(TestBlock.create(nextTs, d.lastBlockId, Seq(setScript, leaseTx)))

            assertLeaseCancel(dApp, invoker, d, leaseAmount, leaseId, leaseId)
          }
      }

      "leaseCancel with lease action" in forAll(scenario) {
        case (dApp, invoker, genesis, setScript) =>
          withDomain(createSettings(Ride4DApps -> 0, BlockV5 -> 0, SmartAccounts -> 0, SynchronousCalls -> 0)) { d =>
            d.appendBlock(genesis.transactionData :+ setScript: _*)

            val (leaseAmount, leaseFc) = leaseFunctionCallGen(invoker.toAddress).sample.get
            val leaseInvokeId          = appendBlock(d, invoker, dApp)(d.lastBlockId, leaseFc)
            val leaseId                = Lease.calculateId(Lease(invoker.toAddress.toRide, leaseAmount, 0), leaseInvokeId)

            assertLeaseCancel(dApp, invoker, d, leaseAmount, leaseId, leaseInvokeId)
          }
      }
    }

    "address script" in forAll(accountGen, positiveLongGen) {
      case (sender, initialBalance) =>
        withDomain(createSettings(SmartAccounts -> 0)) { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          val script = ExprScript(TRUE).explicitGet()

          val genesisBlockId = d.lastBlockId
          d.blockchainUpdater.accountScript(sender.toAddress) shouldBe empty
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

          d.blockchainUpdater.accountScript(sender.toAddress) shouldBe empty

          d.rollbackTo(blockWithScriptId)
          d.blockchainUpdater.accountScript(sender.toAddress) should contain(AccountScriptInfo(sender.publicKey, script, 1))

          d.rollbackTo(genesisBlockId)
          d.blockchainUpdater.accountScript(sender.toAddress) shouldBe empty
        }
    }

    def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): WavesSettings = {
      val tfs = TestFunctionalitySettings.Enabled.copy(
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }.toMap,
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

          d.rollbackTo(blockIdWithSponsor)

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

          d.rollbackTo(blockIdWithIssue)

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

          d.rollbackTo(sponsorBlockId)
          d.carryFee shouldBe carry(sponsor1.fee)

          d.rollbackTo(issueBlockId)
          d.carryFee shouldBe carry(issue.fee)

          val transferBlockId = appendBlock(transfer)
          d.carryFee shouldBe carry(transfer.fee)

          appendBlock(sponsor2)
          d.carryFee shouldBe carry(sponsor2.fee)

          d.rollbackTo(transferBlockId)
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

          d.rollbackTo(middleBlock._1)

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
            case e: Throwable => Assertions.assert(e.getMessage.contains("AlreadyInTheState"))
          }
        }
    }
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
