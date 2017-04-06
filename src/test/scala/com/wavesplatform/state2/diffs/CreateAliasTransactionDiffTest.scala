package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{AssetInfo, EqByteArray, portfolioMonoid}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}
import scorex.transaction.{CreateAliasTransaction, GenesisTransaction, TransactionGen}

class CreateAliasTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndAliases: Gen[(GenesisTransaction, CreateAliasTransaction, CreateAliasTransaction, CreateAliasTransaction, CreateAliasTransaction)] = for {
    master <- accountGen
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    alias <- aliasGen
    alias2 <- aliasGen
    fee <- smallFeeGen
    other: PrivateKeyAccount <- accountGen
    aliasTx = CreateAliasTransaction.create(master, alias, fee, ts).right.get
    sameAliasTx = CreateAliasTransaction.create(master, alias, fee + 1, ts).right.get
    sameAliasOtherSenderTx = CreateAliasTransaction.create(other, alias, fee + 2, ts).right.get
    anotherAliasTx = CreateAliasTransaction.create(master, alias2, fee + 3, ts).right.get
  } yield (genesis, aliasTx, sameAliasTx, sameAliasOtherSenderTx, anotherAliasTx)

  property("can create and resolve aliases preserving waves invariant") {
    forAll(preconditionsAndAliases) { case (gen, aliasTx, _, _, anotherAliasTx) =>
      assertDiffAndState(Seq(TestBlock(Seq(gen, aliasTx))), TestBlock(Seq(anotherAliasTx))) { case (blockDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0

        val senderAcc = anotherAliasTx.sender.toAccount
        blockDiff.txsDiff.aliases shouldBe Map(anotherAliasTx.alias -> senderAcc)

        newState.aliasesOfAddress(senderAcc).toSet shouldBe Set(anotherAliasTx.alias, aliasTx.alias)
        newState.resolveAlias(aliasTx.alias) shouldBe Some(senderAcc)
        newState.resolveAlias(anotherAliasTx.alias) shouldBe Some(senderAcc)
      }
    }
  }

  property("cannot recreate existing alias") {
    forAll(preconditionsAndAliases) { case (gen, aliasTx, sameAliasTx, sameAliasOtherSenderTx, _) =>
      assertDiffEi(Seq(TestBlock(Seq(gen, aliasTx))), TestBlock(Seq(sameAliasTx))) { blockDiffEi =>
        blockDiffEi shouldBe 'left
      }

      assertDiffEi(Seq(TestBlock(Seq(gen, aliasTx))), TestBlock(Seq(sameAliasOtherSenderTx))) { blockDiffEi =>
        blockDiffEi shouldBe 'left
      }
    }
  }
}