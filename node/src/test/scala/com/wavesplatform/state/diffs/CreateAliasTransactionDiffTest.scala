package com.wavesplatform.state.diffs

import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.SmartAccounts
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.utils.addressTransactions
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.{NG, RideV3, WavesSettingsOps}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.defaultAddress
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, GenesisTransaction, TransactionType, TxHelpers, TxVersion}
import monix.execution.Scheduler.Implicits.global

class CreateAliasTransactionDiffTest extends PropSpec with WithDomain {

  val fs: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0)
    )

  val preconditionsAndAliasCreations
      : Seq[(GenesisTransaction, CreateAliasTransaction, CreateAliasTransaction, CreateAliasTransaction, CreateAliasTransaction)] = {
    val master = TxHelpers.signer(1)
    val other  = TxHelpers.signer(2)

    val fee = 400000

    val genesis = TxHelpers.genesis(master.toAddress)
    val alias   = Alias.create("alias").explicitGet()
    val alias2  = Alias.create("alias2").explicitGet()
    val aliasTxs = Seq(
      TxHelpers.createAlias(alias.name, master, fee = fee),
      TxHelpers.createAlias(alias.name, master, fee = fee, version = TxVersion.V1)
    )
    val sameAliasTxs = Seq(
      TxHelpers.createAlias(alias.name, master, fee = fee + 100),
      TxHelpers.createAlias(alias.name, master, fee = fee + 100, version = TxVersion.V1)
    )
    val sameAliasOtherSenderTxs = Seq(
      TxHelpers.createAlias(alias.name, other, fee = fee + 200),
      TxHelpers.createAlias(alias.name, other, fee = fee + 200, version = TxVersion.V1)
    )
    val anotherAliasTxs = Seq(
      TxHelpers.createAlias(alias2.name, master, fee = fee + 300),
      TxHelpers.createAlias(alias2.name, master, fee = fee + 300, version = TxVersion.V1)
    )

    for {
      aliasTx                <- aliasTxs
      sameAliasTx            <- sameAliasTxs
      sameAliasOtherSenderTx <- sameAliasOtherSenderTxs
      anotherAliasTx         <- anotherAliasTxs
    } yield (genesis, aliasTx, sameAliasTx, sameAliasOtherSenderTx, anotherAliasTx)
  }

  property("can create and resolve aliases preserving waves invariant") {
    preconditionsAndAliasCreations.foreach { case (gen, aliasTx, _, _, anotherAliasTx) =>
      withDomain(RideV3) { d =>
        d.appendBlock(gen)
        d.appendBlock(aliasTx)
        d.appendBlock(anotherAliasTx)
        d.liquidSnapshot.balances.collect {
          case ((`defaultAddress`, Waves), balance) =>
            val carryFee = (anotherAliasTx.fee.value - aliasTx.fee.value) / 5 * 3
            balance - d.rocksDBWriter.balance(defaultAddress) + carryFee
          case ((address, Waves), balance) =>
            balance - d.rocksDBWriter.balance(address)
        }.sum shouldBe 0
        val senderAcc = anotherAliasTx.sender.toAddress
        d.liquidSnapshot.aliases shouldBe Map(anotherAliasTx.alias -> senderAcc)
        addressTransactions(
          rdb,
          Some(Height(d.blockchain.height + 1) -> d.liquidSnapshot),
          senderAcc,
          Set(TransactionType.CreateAlias),
          None
        ).collect { case (_, cat: CreateAliasTransaction) => cat.alias }.toSet shouldBe Set(anotherAliasTx.alias, aliasTx.alias)
        d.blockchain.resolveAlias(aliasTx.alias) shouldBe Right(senderAcc)
        d.blockchain.resolveAlias(anotherAliasTx.alias) shouldBe Right(senderAcc)
      }
    }
  }

  property("cannot recreate existing alias") {
    preconditionsAndAliasCreations.foreach { case (gen, aliasTx, sameAliasTx, sameAliasOtherSenderTx, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(gen, aliasTx))), TestBlock.create(Seq(sameAliasTx)), fs) { blockDiffEi =>
        blockDiffEi should produce("AlreadyInTheState")
      }
      assertDiffEi(Seq(TestBlock.create(Seq(gen, aliasTx))), TestBlock.create(Seq(sameAliasOtherSenderTx)), fs) { blockDiffEi =>
        blockDiffEi should produce("AlreadyInTheState")
      }
    }
  }

  val preconditionsTransferLease
      : Seq[(Seq[GenesisTransaction], IssueTransaction, IssueTransaction, CreateAliasTransaction, TransferTransaction, LeaseTransaction)] = {
    val master = TxHelpers.signer(1)

    for {
      aliasedRecipient <- Seq(master, TxHelpers.signer(2))
      genesis = Seq(master, aliasedRecipient).map(acc => TxHelpers.genesis(acc.toAddress))
      issue1  = TxHelpers.issue(master, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      issue2  = TxHelpers.issue(master, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)
      maybeAsset    <- Seq(None, Some(issue1))
      maybeAsset2   <- Seq(None, Some(issue2))
      maybeFeeAsset <- Seq(maybeAsset, maybeAsset2)
      alias   = Alias.create("alias").explicitGet()
      aliasTx = TxHelpers.createAlias(alias.name, aliasedRecipient)
      transfer = TxHelpers.transfer(
        master,
        alias,
        asset = Asset.fromCompatId(maybeAsset.map(_.id())),
        feeAsset = Asset.fromCompatId(maybeFeeAsset.map(_.id())),
        version = TxVersion.V1
      )
      lease <- Seq(
        TxHelpers.lease(master, alias),
        TxHelpers.lease(master, alias, version = TxVersion.V1)
      )
    } yield (genesis, issue1, issue2, aliasTx, transfer, lease)
  }

  property("Can transfer to alias") {
    preconditionsTransferLease.foreach { case (genesis, issue1, issue2, aliasTx, transfer, _) =>
      withDomain(NG.addFeatures(SmartAccounts)) { d =>
        d.appendBlock(genesis*)
        d.appendBlock(issue1, issue2, aliasTx)
        d.appendBlock(transfer)
        if (transfer.sender.toAddress != aliasTx.sender.toAddress) {
          d.liquidSnapshot.balances((aliasTx.sender.toAddress, transfer.assetId)) shouldBe
            transfer.amount.value + d.rocksDBWriter.balance(aliasTx.sender.toAddress, transfer.assetId)
        }
      }
    }
  }

  property("Can lease to alias except for self") {
    preconditionsTransferLease.foreach { case (genesis, issue1, issue2, aliasTx, _, lease) =>
      assertDiffEi(Seq(TestBlock.create(genesis :+ issue1 :+ issue2 :+ aliasTx)), TestBlock.create(Seq(lease))) { blockDiffEi =>
        if (lease.sender.toAddress != aliasTx.sender.toAddress) {
          blockDiffEi.explicitGet().balances.get((aliasTx.sender.toAddress, Waves)) shouldBe None
          blockDiffEi.explicitGet().leaseBalances(aliasTx.sender.toAddress) shouldBe LeaseBalance(lease.amount.value, 0)
        } else {
          blockDiffEi should produce("Cannot lease to self")
        }
      }
    }
  }
}
