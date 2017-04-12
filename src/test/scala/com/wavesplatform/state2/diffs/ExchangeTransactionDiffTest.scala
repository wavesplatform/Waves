package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{Portfolio, portfolioMonoid}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.{GenesisTransaction, TransactionGen}
import scorex.utils.ByteArrayExtension

class ExchangeTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndExchange: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
    buyer <- accountGen
    seller <- accountGen
    ts <- timestampGen
    gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).right.get
    gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).right.get
    issue1: IssueTransaction <- issueReissueGeneratorP(ENOUGH_AMT, seller).map(_._1)
    issue2: IssueTransaction <- issueReissueGeneratorP(ENOUGH_AMT, buyer).map(_._1)
    maybeAsset1 <- Gen.option(issue1.id)
    maybeAsset2 <- Gen.option(issue2.id) suchThat (x => !ByteArrayExtension.sameOption(x, maybeAsset1))
    exchange <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2).map(_._1)
  } yield (gen1, gen2, issue1, issue2, exchange)


  // This might fail from time to time.
  // The logic defining max matched amount is a bit complex
  // so it's not easy to setup 100% correct pair of orders
  property("preserves waves invariant") {
    forAll(preconditionsAndExchange) { case (gen1, gen2, issue1, issue2, exchange) =>
      assertDiffAndState(Seq(TestBlock(Seq(gen1, gen2, issue1, issue2))), TestBlock(Seq(exchange))) { case (blockDiff, state) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

      }
    }
  }
}