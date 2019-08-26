package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, CreateAliasTransactionV1, GenesisTransaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class CreateAliasTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val fs =
    TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0)
    )

  val preconditionsAndAliasCreations
    : Gen[(GenesisTransaction, CreateAliasTransaction, CreateAliasTransaction, CreateAliasTransaction, CreateAliasTransaction)] = for {
    master <- accountGen
    ts     <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    alias                    <- aliasGen
    alias2                   <- aliasGen suchThat (_.name != alias.name)
    fee                      <- smallFeeGen
    other: KeyPair <- accountGen
    aliasTx                  <- createAliasGen(master, alias, fee, ts)
    sameAliasTx              <- createAliasGen(master, alias, fee + 1, ts)
    sameAliasOtherSenderTx   <- createAliasGen(other, alias, fee + 2, ts)
    anotherAliasTx           <- createAliasGen(master, alias2, fee + 3, ts)
  } yield (genesis, aliasTx, sameAliasTx, sameAliasOtherSenderTx, anotherAliasTx)

  property("can create and resolve aliases preserving waves invariant") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, aliasTx, _, _, anotherAliasTx) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen, aliasTx))), TestBlock.create(Seq(anotherAliasTx)), fs) {
          case (blockDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0

            val senderAcc = anotherAliasTx.sender.toAddress
            blockDiff.aliases shouldBe Map(anotherAliasTx.alias -> senderAcc)

            newState.aliasesOfAddress(senderAcc).toSet shouldBe Set(anotherAliasTx.alias, aliasTx.alias)
            newState.resolveAlias(aliasTx.alias) shouldBe Right(senderAcc)
            newState.resolveAlias(anotherAliasTx.alias) shouldBe Right(senderAcc)
        }
    }
  }

  property("cannot recreate existing alias") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, aliasTx, sameAliasTx, sameAliasOtherSenderTx, _) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, aliasTx))), TestBlock.create(Seq(sameAliasTx)), fs) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }

        assertDiffEi(Seq(TestBlock.create(Seq(gen, aliasTx))), TestBlock.create(Seq(sameAliasOtherSenderTx)), fs) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }
    }
  }

  val preconditionsTransferLease = for {
    master           <- accountGen
    aliasedRecipient <- otherAccountGen(candidate = master)
    ts               <- positiveIntGen
    gen: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    gen2: GenesisTransaction = GenesisTransaction.create(aliasedRecipient, ENOUGH_AMT + 1, ts).explicitGet()
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    maybeAsset               <- Gen.option(issue1)
    maybeAsset2              <- Gen.option(issue2)
    maybeFeeAsset            <- Gen.oneOf(maybeAsset, maybeAsset2)
    alias                    <- aliasGen
    fee                      <- smallFeeGen
    aliasTx = CreateAliasTransactionV1.selfSigned(aliasedRecipient, alias, fee, ts).explicitGet()
    transfer <- transferGeneratorP(
      master,
      alias,
      Asset.fromCompatId(maybeAsset.map(_.id())),
      Asset.fromCompatId(maybeFeeAsset.map(_.id()))
    )
    lease <- leaseAndCancelGeneratorP(master, alias).map(_._1)
  } yield (gen, gen2, issue1, issue2, aliasTx, transfer, lease)

  property("Can transfer to alias") {
    forAll(preconditionsTransferLease) {
      case (gen, gen2, issue1, issue2, aliasTx, transfer, _) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen, gen2, issue1, issue2, aliasTx))), TestBlock.create(Seq(transfer))) {
          case (blockDiff, _) =>
            if (transfer.sender.toAddress != aliasTx.sender.toAddress) {
              val recipientPortfolioDiff = blockDiff.portfolios(aliasTx.sender)
              transfer.assetId match {
                case aid @ IssuedAsset(_) => recipientPortfolioDiff shouldBe Portfolio(0, LeaseBalance.empty, Map(aid -> transfer.amount))
                case Waves                => recipientPortfolioDiff shouldBe Portfolio(transfer.amount, LeaseBalance.empty, Map.empty)
              }
            }
        }
    }
  }

  property("Can lease to alias except for self") {
    forAll(preconditionsTransferLease) {
      case (gen, gen2, issue1, issue2, aliasTx, _, lease) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen, gen2, issue1, issue2, aliasTx))), TestBlock.create(Seq(lease))) { blockDiffEi =>
          if (lease.sender.toAddress != aliasTx.sender.toAddress) {
            val recipientPortfolioDiff = blockDiffEi.explicitGet().portfolios(aliasTx.sender)
            recipientPortfolioDiff shouldBe Portfolio(0, LeaseBalance(lease.amount, 0), Map.empty)
          } else {
            blockDiffEi should produce("Cannot lease to self")
          }
        }
    }
  }
}
