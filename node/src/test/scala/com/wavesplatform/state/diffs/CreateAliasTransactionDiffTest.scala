package com.wavesplatform.state.diffs

import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.utils.addressTransactions
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, GenesisTransaction, TransactionType, TxHelpers, TxVersion}
import monix.execution.Scheduler.Implicits.global

class CreateAliasTransactionDiffTest extends PropSpec with WithState {

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
      TxHelpers.createAlias(alias.name, master, fee = fee + 1),
      TxHelpers.createAlias(alias.name, master, fee = fee + 1, version = TxVersion.V1)
    )
    val sameAliasOtherSenderTxs = Seq(
      TxHelpers.createAlias(alias.name, other, fee = fee + 2),
      TxHelpers.createAlias(alias.name, other, fee = fee + 2, version = TxVersion.V1)
    )
    val anotherAliasTxs = Seq(
      TxHelpers.createAlias(alias2.name, master, fee = fee + 3),
      TxHelpers.createAlias(alias2.name, master, fee = fee + 3, version = TxVersion.V1)
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
      assertDiffAndState(Seq(TestBlock.create(Seq(gen, aliasTx))), TestBlock.create(Seq(anotherAliasTx)), fs) { case (blockDiff, newState) =>
        val totalPortfolioDiff = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0

        val senderAcc = anotherAliasTx.sender.toAddress
        blockDiff.aliases shouldBe Map(anotherAliasTx.alias -> senderAcc)

        addressTransactions(rdb, Some(Height(newState.height + 1) -> blockDiff), senderAcc, Set(TransactionType.CreateAlias), None).collect {
          case (_, cat: CreateAliasTransaction) => cat.alias
        }.toSet shouldBe Set(
          anotherAliasTx.alias,
          aliasTx.alias
        )
        newState.resolveAlias(aliasTx.alias) shouldBe Right(senderAcc)
        newState.resolveAlias(anotherAliasTx.alias) shouldBe Right(senderAcc)
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
      assertDiffAndState(Seq(TestBlock.create(genesis :+ issue1 :+ issue2 :+ aliasTx)), TestBlock.create(Seq(transfer))) { case (blockDiff, _) =>
        if (transfer.sender.toAddress != aliasTx.sender.toAddress) {
          val recipientPortfolioDiff = blockDiff.portfolios(aliasTx.sender.toAddress)
          transfer.assetId match {
            case aid @ IssuedAsset(_) => recipientPortfolioDiff shouldBe Portfolio.build(aid, transfer.amount.value)
            case Waves                => recipientPortfolioDiff shouldBe Portfolio(transfer.amount.value)
          }
        }
      }
    }
  }

  property("Can lease to alias except for self") {
    preconditionsTransferLease.foreach { case (genesis, issue1, issue2, aliasTx, _, lease) =>
      assertDiffEi(Seq(TestBlock.create(genesis :+ issue1 :+ issue2 :+ aliasTx)), TestBlock.create(Seq(lease))) { blockDiffEi =>
        if (lease.sender.toAddress != aliasTx.sender.toAddress) {
          val recipientPortfolioDiff = blockDiffEi.explicitGet().portfolios(aliasTx.sender.toAddress)
          recipientPortfolioDiff shouldBe Portfolio(0, LeaseBalance(lease.amount.value, 0))
        } else {
          blockDiffEi should produce("Cannot lease to self")
        }
      }
    }
  }
}
