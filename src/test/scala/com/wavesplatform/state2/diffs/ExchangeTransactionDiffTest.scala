package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.GenesisTransaction
import scorex.utils.ByteArrayExtension

class ExchangeTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  // This might fail from time to time.
  // The logic defining max matched amount is a bit complex
  // so it's not easy to setup 100% correct pair of orders

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("preserves waves invariant, stores match info, rewards matcher") {

    val preconditionsAndExchange: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer <- accountGen
      seller <- accountGen
      ts <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).right.get
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).right.get
      issue1: IssueTransaction <- issueReissueBurnMakeAssetNameUniqueGeneratorP(ENOUGH_AMT, seller).map(_._1)
      issue2: IssueTransaction <- issueReissueBurnMakeAssetNameUniqueGeneratorP(ENOUGH_AMT, buyer).map(_._1)
      maybeAsset1 <- Gen.option(issue1.id)
      maybeAsset2 <- Gen.option(issue2.id) suchThat (x => x != maybeAsset1)
      exchange <- exchangeGeneratorP(buyer, seller, maybeAsset1.map(_.arr), maybeAsset2.map(_.arr))
    } yield (gen1, gen2, issue1, issue2, exchange)

    forAll(preconditionsAndExchange, accountGen) { case ((gen1, gen2, issue1, issue2, exchange), miner) =>
      assertDiffAndState(Seq(TestBlock(Seq(gen1, gen2, issue1, issue2))), TestBlock(Seq(exchange), miner)) { case (blockDiff, state) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

        blockDiff.txsDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
      }
    }
  }
}
