package com.wavesplatform.state.diffs

import cats.{Order => _, _}
import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.ExchangeTransactionDiff.getOrderFeePortfolio
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AccountBalanceError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{Order, _}
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV1, IssueTransactionV2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, crypto}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Random

class ExchangeTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with Inside with NoShrink {

  val MATCHER: KeyPair = KeyPair(Base58.decode("matcher"))

  val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id       -> 0,
      BlockchainFeatures.SmartAssets.id         -> 0,
      BlockchainFeatures.SmartAccountTrading.id -> 0,
      BlockchainFeatures.Ride4DApps.id          -> 0
    )
  )

  val fsWithOrderV3Feature: FunctionalitySettings = fs.copy(preActivatedFeatures = fs.preActivatedFeatures + (BlockchainFeatures.OrderV3.id -> 0))

  val fsOrderV3MassTransfer =
    fsWithOrderV3Feature.copy(preActivatedFeatures = fsWithOrderV3Feature.preActivatedFeatures + (BlockchainFeatures.MassTransfer.id -> 0))

  private val estimator = ScriptEstimatorV2

  property("Validation fails when OrderV3 feature is not activation yet") {

    val preconditionsAndExchange
      : Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id())
      maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1)
      exchange <- exchangeV2GeneratorP(
        buyer = buyer,
        seller = seller,
        amountAssetId = Asset.fromCompatId(maybeAsset2),
        priceAssetId = Asset.fromCompatId(maybeAsset1),
        orderVersions = Set(3)
      )
    } yield (gen1, gen2, gen3, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, gen3, issue1, issue2, exchange) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2))), TestBlock.create(Seq(exchange)), fs) { blockDiffEi =>
          blockDiffEi should produce("Order Version 3 feature has not been activated yet")
        }
    }
  }

  property("Preserves waves invariant, stores match info, rewards matcher") {

    val preconditionsAndExchange: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id()) map Asset.fromCompatId
      maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1.compatId) map Asset.fromCompatId
      exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
    } yield (gen1, gen2, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1, issue2))), TestBlock.create(Seq(exchange)), fsWithOrderV3Feature) {
          case (blockDiff, state) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            blockDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
        }
    }
  }

  property("Preserves assets invariant (matcher's fee in one of the assets of the pair or in Waves), stores match info, rewards matcher") {

    val preconditionsAndExchange
      : Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id()).map(Asset.fromCompatId)
      maybeAsset2              <- (Gen.option(issue2.id()) suchThat (x => x != maybeAsset1.compatId)).map(Asset.fromCompatId)
      buyMatcherFeeAssetId     <- Gen.oneOf(maybeAsset1, maybeAsset2)
      sellMatcherFeeAssetId    <- Gen.oneOf(maybeAsset1, maybeAsset2)
      exchange <- exchangeV2GeneratorP(
        buyer = buyer,
        seller = seller,
        amountAssetId = maybeAsset2,
        priceAssetId = maybeAsset1,
        buyMatcherFeeAssetId = buyMatcherFeeAssetId,
        sellMatcherFeeAssetId = sellMatcherFeeAssetId,
        fixedMatcher = Some(matcher)
      ) retryUntil transactionWithOrdersV3IsValid
    } yield (gen1, gen2, gen3, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, gen3, issue1, issue2, exchange) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2))), TestBlock.create(Seq(exchange)), fsWithOrderV3Feature) {
          case (blockDiff, state) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            val matcherPortfolio = Monoid.combineAll(blockDiff.portfolios.filterKeys(_.stringRepr == exchange.sender.stringRepr).values)

            val restoredMatcherPortfolio =
              Monoid.combineAll(
                Seq(
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.buyOrder, exchange.buyMatcherFee),
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.sellOrder, exchange.sellMatcherFee),
                  ExchangeTransactionDiff.wavesPortfolio(-exchange.fee)
                )
              )

            matcherPortfolio shouldBe restoredMatcherPortfolio
        }
    }
  }

  property("Validation fails when received amount of asset is less than fee in that asset (Orders V3 are used)") {
    val preconditionsAndExchange
      : Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      buyerIssuedAsset  = IssuedAsset(issue1.id())
      sellerIssuedAsset = IssuedAsset(issue2.id())
      exchange <- exchangeV2GeneratorP(
        buyer = buyer,
        seller = seller,
        amountAssetId = sellerIssuedAsset, // buyer buys sellerIssuedAsset (received asset)
        priceAssetId = buyerIssuedAsset, // buyer sells buyerIssuedAsset
        buyMatcherFeeAssetId = sellerIssuedAsset, // buyer pays fee in sellerIssuedAsset (received asset)
        sellMatcherFeeAssetId = buyerIssuedAsset,
        fixedMatcher = Some(matcher),
        orderVersions = Set(3)
      ).retryUntil(ex => !transactionWithOrdersV3IsValid(ex)) // fee in sellerIssuedAsset (received asset) is greater than amount of received sellerIssuedAsset
    } yield (gen1, gen2, gen3, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, gen3, issue1, issue2, exchange) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2))), TestBlock.create(Seq(exchange)), fsWithOrderV3Feature) {
          blockDiffEi =>
            blockDiffEi should produce("negative asset balance")
        }
    }
  }

  property("Preserves assets invariant (matcher's fee in separately issued asset), stores match info, rewards matcher (Orders V3 are used)") {

    val preconditionsAndExchange: Gen[(GenesisTransaction,
                                       GenesisTransaction,
                                       GenesisTransaction,
                                       IssueTransaction,
                                       IssueTransaction,
                                       IssueTransaction,
                                       IssueTransaction,
                                       ExchangeTransaction)] = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue3: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue4: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- (Gen.option(issue1.id())).map(Asset.fromCompatId)
      maybeAsset2              <- (Gen.option(issue2.id()) suchThat (x => x != maybeAsset1.compatId)).map(Asset.fromCompatId)
      buyMatcherFeeAssetId  = IssuedAsset(issue3.id())
      sellMatcherFeeAssetId = IssuedAsset(issue4.id())
      exchange <- exchangeV2GeneratorP(
        buyer = buyer,
        seller = seller,
        amountAssetId = maybeAsset2,
        priceAssetId = maybeAsset1,
        buyMatcherFeeAssetId = buyMatcherFeeAssetId,
        sellMatcherFeeAssetId = sellMatcherFeeAssetId,
        fixedMatcher = Some(matcher),
        orderVersions = Set(3)
      )
    } yield (gen1, gen2, gen3, issue1, issue2, issue3, issue4, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, gen3, issue1, issue2, issue3, issue4, exchange) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2, issue3, issue4))),
                           TestBlock.create(Seq(exchange)),
                           fsWithOrderV3Feature) {
          case (blockDiff, state) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            val matcherPortfolio = Monoid.combineAll(blockDiff.portfolios.filterKeys(_.stringRepr == exchange.sender.stringRepr).values)

            val restoredMatcherPortfolio =
              Monoid.combineAll(
                Seq(
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.buyOrder, exchange.buyMatcherFee),
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.sellOrder, exchange.sellMatcherFee),
                  ExchangeTransactionDiff.wavesPortfolio(-exchange.fee)
                )
              )

            matcherPortfolio shouldBe restoredMatcherPortfolio
        }
    }
  }

  property("Validation fails in case of attempt to pay fee in unissued asset (Orders V3 are used)") {

    val preconditionsAndExchange
      : Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id()).map(Asset.fromCompatId)
      maybeAsset2              <- (Gen.option(issue2.id()) suchThat (x => x != maybeAsset1.compatId)).map(Asset.fromCompatId)
      matcherFeeAssetId        <- assetIdGen retryUntil (_.nonEmpty) map (s => IssuedAsset(s.get))
      exchange <- exchangeV2GeneratorP(
        buyer = buyer,
        seller = seller,
        amountAssetId = maybeAsset2,
        priceAssetId = maybeAsset1,
        buyMatcherFeeAssetId = matcherFeeAssetId,
        sellMatcherFeeAssetId = matcherFeeAssetId,
        fixedMatcher = Some(matcher),
        orderVersions = Set(3)
      )
    } yield (gen1, gen2, gen3, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, gen3, issue1, issue2, exchange) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2))), TestBlock.create(Seq(exchange)), fsWithOrderV3Feature) {
          blockDiffEi =>
            blockDiffEi should produce("negative asset balance")
        }
    }
  }

  property("Validation fails when balance of asset issued separately (asset is not in the pair) is less than fee in that asset (Orders V3 are used)") {

    val preconditionsAndExchange: Gen[(GenesisTransaction,
                                       GenesisTransaction,
                                       GenesisTransaction,
                                       IssueTransaction,
                                       IssueTransaction,
                                       IssueTransaction,
                                       IssueTransaction,
                                       ExchangeTransaction)] = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue3: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT / 1000000, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue4: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT / 1000000, seller).map(_._1).retryUntil(_.script.isEmpty)
      buyerIssuedAsset      = IssuedAsset(issue1.id())
      sellerIssuedAsset     = IssuedAsset(issue2.id())
      buyMatcherFeeAssetId  = IssuedAsset(issue3.id())
      sellMatcherFeeAssetId = IssuedAsset(issue4.id())
      exchange <- exchangeV2GeneratorP(
        buyer = buyer,
        seller = seller,
        amountAssetId = sellerIssuedAsset,
        priceAssetId = buyerIssuedAsset,
        fixedMatcherFee = Some(ENOUGH_AMT / 10),
        buyMatcherFeeAssetId = buyMatcherFeeAssetId,
        sellMatcherFeeAssetId = sellMatcherFeeAssetId,
        fixedMatcher = Some(matcher),
        orderVersions = Set(3)
      )
    } yield (gen1, gen2, gen3, issue1, issue2, issue3, issue4, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, gen3, issue1, issue2, issue3, issue4, exchange) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2, issue3, issue4))),
                     TestBlock.create(Seq(exchange)),
                     fsWithOrderV3Feature) { blockDiffEi =>
          blockDiffEi should produce("negative asset balance")
        }
    }
  }

  property("Total matcher's fee (sum of matcher's fees in exchange transactions) is less than or equal to order's matcher fee") {

    val preconditions =
      oneBuyFewSellsPreconditions(
        totalBuyMatcherFeeBoundaries = (bigBuyOrderMatcherFee: Long) => (bigBuyOrderMatcherFee - 1000L, bigBuyOrderMatcherFee), // sum of buyMatcherFee in ex trs <= specified in bigBuyOrder
        sellersTotalAmount = identity
      )

    forAll(preconditions) {
      case (genesises, issueTx1, issueTx2, massTransfer, exchanges, bigBuyOrder) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer))),
          TestBlock.create(exchanges),
          fsOrderV3MassTransfer
        ) {
          case (blockDiff, _) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)

            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            val combinedPortfolio = Monoid
              .combineAll(
                exchanges.map(ex => getOrderFeePortfolio(bigBuyOrder, ex.buyMatcherFee))
              )

            val feeSumPaidByBuyer =
              bigBuyOrder.matcherFeeAssetId
                .fold(combinedPortfolio.balance)(combinedPortfolio.assets)

            (feeSumPaidByBuyer <= exchanges.head.buyOrder.matcherFee) shouldBe true
        }
    }
  }

  property("Validation fails when total matcher's fee (sum of matcher's fees in exchange transactions) is greater than order's matcher fee") {

    val preconditions =
      oneBuyFewSellsPreconditions(
        totalBuyMatcherFeeBoundaries = (bigBuyOrderMatcherFee: Long) => (bigBuyOrderMatcherFee + 1, bigBuyOrderMatcherFee + 100000L), // sum of buyMatcherFee in ex trs > specified in bigBuyOrder
        sellersTotalAmount = identity
      )

    forAll(preconditions) {
      case (genesises, issueTx1, issueTx2, massTransfer, exchanges, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer))),
          TestBlock.create(exchanges),
          fsOrderV3MassTransfer
        ) { blockDiffEi =>
          blockDiffEi should produce("Insufficient buy fee")
        }
    }
  }

  property("Validation fails when total sell amount overfills buy order amount") {

    val preconditions =
      oneBuyFewSellsPreconditions(
        totalBuyMatcherFeeBoundaries = (bigBuyOrderMatcherFee: Long) => (bigBuyOrderMatcherFee - 10000L, bigBuyOrderMatcherFee), // correct total buyMatcherFee in ex trs
        sellersTotalAmount = (bigBuyOrderAmount: Long) => bigBuyOrderAmount + 10000L // sell orders overfill buy order
      )

    forAll(preconditions) {
      case (genesises, issueTx1, issueTx2, massTransfer, exchanges, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer))),
          TestBlock.create(exchanges),
          fsOrderV3MassTransfer
        ) { blockDiffEi =>
          blockDiffEi should produce("Too much buy")
        }
    }
  }

  property("buy waves without enough money for fee") {
    val preconditions: Gen[(GenesisTransaction, GenesisTransaction, IssueTransactionV1, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, 1 * Constants.UnitsInWave, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransactionV1 <- issueGen(buyer)
      exchange <- Gen.oneOf(
        exchangeV1GeneratorP(buyer, seller, Waves, IssuedAsset(issue1.id()), fixedMatcherFee = Some(300000)),
        exchangeV2GeneratorP(buyer, seller, Waves, IssuedAsset(issue1.id()), fixedMatcherFee = Some(300000))
      )
    } yield {
      (gen1, gen2, issue1, exchange)
    }

    forAll(preconditions) {
      case (gen1, gen2, issue1, exchange) =>
        whenever(exchange.amount > 300000) {
          assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(exchange)), fsWithOrderV3Feature) {
            case (blockDiff, _) =>
              val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
              totalPortfolioDiff.balance shouldBe 0
              totalPortfolioDiff.effectiveBalance shouldBe 0
              totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

              blockDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
          }
        }
    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: KeyPair, ts: Long): Either[ValidationError, ExchangeTransaction] = {
    val mf     = buy.matcherFee
    val amount = math.min(buy.amount, sell.amount)
    ExchangeTransactionV1.create(
      matcher = matcher,
      buyOrder = buy.asInstanceOf[OrderV1],
      sellOrder = sell.asInstanceOf[OrderV1],
      amount = amount,
      price = price,
      buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
      sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
      fee = buy.matcherFee,
      timestamp = ts
    )
  }

  property("small fee cases") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(KeyPair, KeyPair, KeyPair, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(seller)
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(IssuedAsset(issue1.id()), Waves)
        val buy       = Order.buy(buyer, matcher, assetPair, 1000000L, price, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, 1L, price, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fs) {
          case (blockDiff, state) =>
            blockDiff.portfolios(tx.sender).balance shouldBe tx.buyMatcherFee + tx.sellMatcherFee - tx.fee
            state.balance(tx.sender) shouldBe 0L
        }
    }
  }

  property("Not enough balance") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(KeyPair, KeyPair, KeyPair, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(seller, fixedQuantity = Some(1000L))
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(IssuedAsset(issue1.id()), Waves)
        val buy       = Order.buy(buyer, matcher, assetPair, issue1.quantity + 1, price, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, issue1.quantity + 1, price, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fsWithOrderV3Feature) { totalDiffEi =>
          inside(totalDiffEi) {
            case Left(TransactionValidationError(AccountBalanceError(errs), _)) =>
              errs should contain key seller.toAddress
          }
        }
    }
  }

  property("Diff for ExchangeTransaction works as expected and doesn't use rounding inside") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(KeyPair, KeyPair, KeyPair, GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(buyer, fixedQuantity = Some(Long.MaxValue))
      } yield (buyer, seller, matcher, gen1, gen2, gen3, issue1)

    val (buyer, seller, matcher, gen1, gen2, gen3, issue1) = preconditions.sample.get
    val assetPair                                          = AssetPair(Waves, IssuedAsset(issue1.id()))

    val buy  = Order.buy(buyer, matcher, assetPair, 3100000000L, 238, Ts, Ts + 1, MatcherFee, version = 1: Byte).asInstanceOf[OrderV1]
    val sell = Order.sell(seller, matcher, assetPair, 425532L, 235, Ts, Ts + 1, MatcherFee, version = 1: Byte).asInstanceOf[OrderV1]
    val tx = ExchangeTransactionV1
      .create(matcher = matcher,
              buyOrder = buy,
              sellOrder = sell,
              amount = 425532,
              price = 238,
              buyMatcherFee = 41,
              sellMatcherFee = 300000,
              fee = buy.matcherFee,
              timestamp = Ts)
      .explicitGet()

    assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1))), TestBlock.create(Seq(tx))) { totalDiffEi =>
      inside(totalDiffEi) {
        case Right(diff) =>
          import diff.portfolios
          portfolios(buyer).balance shouldBe (-41L + 425532L)
          portfolios(seller).balance shouldBe (-300000L - 425532L)
          portfolios(matcher).balance shouldBe (+41L + 300000L - tx.fee)
      }
    }
  }

  val fsV2 = createSettings(
    BlockchainFeatures.SmartAccounts       -> 0,
    BlockchainFeatures.SmartAccountTrading -> 0,
    BlockchainFeatures.SmartAssets         -> 0,
    BlockchainFeatures.Ride4DApps          -> 0,
    BlockchainFeatures.FeeSponsorship      -> 0,
    BlockchainFeatures.FairPoS             -> 0
  )

  private def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): FunctionalitySettings =
    TestFunctionalitySettings.Enabled
      .copy(
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }(collection.breakOut),
        blocksForFeatureActivation = 1,
        featureCheckBlocksPeriod = 1
      )

  property(s"Exchange transaction with scripted matcher and orders needs extra fee ($ScriptExtraFee)") {
    val allValidP = smartTradePreconditions(
      scriptGen("Order", true),
      scriptGen("Order", true),
      scriptGen("ExchangeTransaction", true)
    )

    forAll(allValidP) {
      case (genesis, transfers, issueAndScripts, etx) =>
        val enoughFee = FeeValidation.ScriptExtraFee + FeeValidation.FeeConstants(ExchangeTransaction.typeId) * FeeValidation.FeeUnit
        val smallFee  = enoughFee - 1
        val exchangeWithSmallFee = ExchangeTransactionV2
          .create(MATCHER, etx.buyOrder, etx.sellOrder, 1000000, 1000000, 0, 0, smallFee, etx.timestamp)
          .explicitGet()

        val exchangeWithEnoughFee = ExchangeTransactionV2
          .create(MATCHER, etx.buyOrder, etx.sellOrder, 1000000, 1000000, 0, 0, enoughFee, etx.timestamp)
          .explicitGet()

        val preconBlocks = Seq(
          TestBlock.create(Seq(genesis)),
          TestBlock.create(transfers),
          TestBlock.create(issueAndScripts)
        )

        val blockWithSmallFeeETx  = TestBlock.create(Seq(exchangeWithSmallFee))
        val blockWithEnoughFeeETx = TestBlock.create(Seq(exchangeWithEnoughFee))

        assertLeft(preconBlocks, blockWithSmallFeeETx, fsV2)("does not exceed minimal value of")
        assertDiffEi(preconBlocks, blockWithEnoughFeeETx, fsV2)(_ shouldBe 'right)
    }
  }

  property("ExchangeTransactions valid if all scripts succeeds") {
    val allValidP = smartTradePreconditions(
      scriptGen("Order", true),
      scriptGen("Order", true),
      scriptGen("ExchangeTransaction", true)
    )

    forAll(allValidP) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(
          TestBlock.create(Seq(genesis)),
          TestBlock.create(transfers),
          TestBlock.create(issueAndScripts)
        )
        assertDiffEi(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2) { diff =>
          diff.isRight shouldBe true
        }
    }
  }

  property("ExchangeTransactions invalid if buyer scripts fails") {
    val failedOrderScript = smartTradePreconditions(
      scriptGen("Order", false),
      scriptGen("Order", true),
      scriptGen("ExchangeTransaction", true)
    )

    forAll(failedOrderScript) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
        assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransactions invalid if seller scripts fails") {
    val failedOrderScript = smartTradePreconditions(
      scriptGen("Order", true),
      scriptGen("Order", false),
      scriptGen("ExchangeTransaction", true)
    )

    forAll(failedOrderScript) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
        assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransactions invalid if matcher script fails") {
    val failedMatcherScript = smartTradePreconditions(
      scriptGen("Order", true),
      scriptGen("Order", true),
      scriptGen("ExchangeTransaction", false)
    )

    forAll(failedMatcherScript) {
      case (genesis, transfers, issueAndScripts, exchangeTx) =>
        val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
        assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), fsV2)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransaction invalid if order signature invalid") {
    val exchangeWithV2Tx =
      simpleTradePreconditions
        .filter(_._5.version == 2)

    forAll(exchangeWithV2Tx) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        val exchangeWithResignedOrder = exchange match {
          case e1 @ ExchangeTransactionV1(bo, so, _, _, _, _, _, _, _) =>
            val newSig = ByteStr(crypto.sign(PrivateKey(so.senderPublicKey), bo.bodyBytes()))
            e1.copy(buyOrder = bo.updateProofs(Proofs(Seq(newSig))).asInstanceOf[OrderV1])
          case e2 @ ExchangeTransactionV2(bo, so, _, _, _, _, _, _, _) =>
            val newSig = ByteStr(crypto.sign(PrivateKey(bo.senderPublicKey), so.bodyBytes()))
            e2.copy(sellOrder = so.updateProofs(Proofs(Seq(newSig))))
        }

        val preconBlocks = Seq(
          TestBlock.create(Seq(gen1, gen2)),
          TestBlock.create(Seq(issue1, issue2))
        )

        val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

        assertLeft(preconBlocks, blockWithExchange, fs)("Proof doesn't validate as signature")
    }
  }

  property("ExchangeTransaction invalid if order contains more than one proofs") {
    val exchangeWithV2Tx =
      simpleTradePreconditions
        .filter(_._5.version == 2)

    forAll(exchangeWithV2Tx) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        val newProofs = Proofs(
          Seq(
            ByteStr(crypto.sign(PrivateKey(exchange.sender), exchange.sellOrder.bodyBytes())),
            ByteStr(crypto.sign(PrivateKey(exchange.sellOrder.senderPublicKey), exchange.sellOrder.bodyBytes()))
          )
        )

        val exchangeWithResignedOrder = exchange match {
          case e1 @ ExchangeTransactionV1(_, so, _, _, _, _, _, _, _) =>
            e1.copy(buyOrder = so.updateProofs(newProofs).asInstanceOf[OrderV1])
          case e2 @ ExchangeTransactionV2(_, so, _, _, _, _, _, _, _) =>
            e2.copy(buyOrder = so.updateProofs(newProofs))
        }

        val preconBlocks = Seq(
          TestBlock.create(Seq(gen1, gen2)),
          TestBlock.create(Seq(issue1, issue2))
        )

        val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

        assertLeft(preconBlocks, blockWithExchange, fs)("Proof doesn't validate as signature")
    }
  }

  property("Disable use OrderV1 on SmartAccount") {
    val enoughFee        = 100000000
    val script           = "true"
    val txScriptCompiled = ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1

    val sellerScript = Some(ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1)
    val buyerScript  = Some(ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1)

    val chainId = AddressScheme.current.chainId

    forAll(for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      genesis = GenesisTransaction.create(MATCHER, Long.MaxValue, ts).explicitGet()
      tr1     = createWavesTransfer(MATCHER, buyer.toAddress, Long.MaxValue / 3, enoughFee, ts + 1).explicitGet()
      tr2     = createWavesTransfer(MATCHER, seller.toAddress, Long.MaxValue / 3, enoughFee, ts + 2).explicitGet()
      asset1 = IssueTransactionV2
        .selfSigned(chainId, buyer, "Asset#1".getBytes("UTF-8"), "".getBytes("UTF-8"), 1000000, 8, false, None, enoughFee, ts + 3)
        .explicitGet()
      asset2 = IssueTransactionV2
        .selfSigned(chainId, seller, "Asset#2".getBytes("UTF-8"), "".getBytes("UTF-8"), 1000000, 8, false, None, enoughFee, ts + 4)
        .explicitGet()
      setMatcherScript = SetScriptTransaction
        .selfSigned(MATCHER, Some(txScriptCompiled), enoughFee, ts + 5)
        .explicitGet()
      setSellerScript = SetScriptTransaction
        .selfSigned(seller, sellerScript, enoughFee, ts + 6)
        .explicitGet()
      setBuyerScript = SetScriptTransaction
        .selfSigned(buyer, buyerScript, enoughFee, ts + 7)
        .explicitGet()
      assetPair = AssetPair(IssuedAsset(asset1.id()), IssuedAsset(asset2.id()))
      o1 <- Gen.oneOf(
        OrderV1.buy(seller, MATCHER, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee),
        OrderV2.buy(seller, MATCHER, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee)
      )
      o2 <- Gen.oneOf(
        OrderV1.sell(buyer, MATCHER, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee),
        OrderV2.sell(buyer, MATCHER, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee)
      )
      exchangeTx = {
        ExchangeTransactionV2
          .create(MATCHER, o1, o2, 1000000, 1000000, enoughFee, enoughFee, enoughFee, ts + 10)
          .explicitGet()
      }
    } yield {
      val pretest = Seq(TestBlock.create(Seq(genesis)),
                        TestBlock.create(Seq(tr1, tr2)),
                        TestBlock.create(Seq(asset1, asset2, setMatcherScript, setSellerScript, setBuyerScript)))
      val test = TestBlock.create(Seq(exchangeTx))
      if (o1.isInstanceOf[OrderV2] && o2.isInstanceOf[OrderV2]) {
        assertDiffEi(pretest, test, fs) { diff =>
          diff.isRight shouldBe true
        }
      } else {
        assertLeft(pretest, test, fs)("Can't process order with signature from scripted account")
      }
    }) { _ =>
      ()
    }
  }

  def scriptGen(caseType: String, v: Boolean): Gen[String] = Gen.oneOf(true, false).map { full =>
    val expr =
      s"""
       |  match tx {
       |   case o: $caseType => $v
       |   case _ => ${!v}
       |  }
     """.stripMargin
    lazy val contract = s"""
       |
       |{-# STDLIB_VERSION 3 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |
       | @Verifier(tx)
       | func verify() = {
       | $expr
       |}
      """.stripMargin

    if (full) contract else expr
  }

  def changeOrderSignature(signWith: Array[Byte], o: Order): Order = {
    lazy val newProofs = Proofs(Seq(ByteStr(crypto.sign(PrivateKey(signWith), o.bodyBytes()))))

    o match {
      case o1 @ OrderV1(_, _, _, _, _, _, _, _, _, _) =>
        o1.copy(proofs = newProofs)
      case o2 @ OrderV2(_, _, _, _, _, _, _, _, _, _) =>
        o2.copy(proofs = newProofs)
    }
  }

  def changeTxSignature(signWith: Array[Byte], et: ExchangeTransaction): ExchangeTransaction = {
    lazy val newSignature = ByteStr(crypto.sign(PrivateKey(signWith), et.bodyBytes()))
    lazy val newProofs    = Proofs(Seq(newSignature))

    et match {
      case e1 @ ExchangeTransactionV1(_, _, _, _, _, _, _, _, _) =>
        e1.copy(signature = newSignature)

      case e2 @ ExchangeTransactionV2(_, _, _, _, _, _, _, _, _) =>
        e2.copy(proofs = newProofs)
    }
  }

  def smartTradePreconditions(buyerScriptSrc: Gen[String],
                              sellerScriptSrc: Gen[String],
                              txScript: Gen[String]): Gen[(GenesisTransaction, List[TransferTransaction], List[Transaction], ExchangeTransaction)] = {
    val enoughFee = 100000000

    val chainId = AddressScheme.current.chainId

    for {
      txScript <- txScript
      txScriptCompiled = ScriptCompiler(txScript, isAssetScript = false, estimator).explicitGet()._1
      sellerScriptSrc <- sellerScriptSrc
      sellerScript = Some(ScriptCompiler(sellerScriptSrc, isAssetScript = false, estimator).explicitGet()._1)
      buyerScriptSrc <- buyerScriptSrc
      buyerScript = Some(ScriptCompiler(buyerScriptSrc, isAssetScript = false, estimator).explicitGet()._1)

      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      genesis = GenesisTransaction.create(MATCHER, Long.MaxValue, ts).explicitGet()
      tr1     = createWavesTransfer(MATCHER, buyer.toAddress, Long.MaxValue / 3, enoughFee, ts + 1).explicitGet()
      tr2     = createWavesTransfer(MATCHER, seller.toAddress, Long.MaxValue / 3, enoughFee, ts + 2).explicitGet()
      asset1 = IssueTransactionV2
        .selfSigned(chainId, buyer, "Asset#1".getBytes("UTF-8"), "".getBytes("UTF-8"), 1000000, 8, false, None, enoughFee, ts + 3)
        .explicitGet()
      asset2 = IssueTransactionV2
        .selfSigned(chainId, seller, "Asset#2".getBytes("UTF-8"), "".getBytes("UTF-8"), 1000000, 8, false, None, enoughFee, ts + 4)
        .explicitGet()
      setMatcherScript = SetScriptTransaction
        .selfSigned(MATCHER, Some(txScriptCompiled), enoughFee, ts + 5)
        .explicitGet()
      setSellerScript = SetScriptTransaction
        .selfSigned(seller, sellerScript, enoughFee, ts + 6)
        .explicitGet()
      setBuyerScript = SetScriptTransaction
        .selfSigned(buyer, buyerScript, enoughFee, ts + 7)
        .explicitGet()
      assetPair = AssetPair(IssuedAsset(asset1.id()), IssuedAsset(asset2.id()))
      o1        = OrderV2.buy(seller, MATCHER, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee)
      o2        = OrderV2.sell(buyer, MATCHER, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee)
      exchangeTx = {
        ExchangeTransactionV2
          .create(MATCHER, o1, o2, 1000000, 1000000, enoughFee, enoughFee, enoughFee, ts + 10)
          .explicitGet()
      }
    } yield (genesis, List(tr1, tr2), List(asset1, asset2, setMatcherScript, setSellerScript, setBuyerScript), exchangeTx)
  }

  val simpleTradePreconditions: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
    buyer  <- accountGen
    seller <- accountGen
    ts     <- timestampGen
    gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
    gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
    maybeAsset1              <- Gen.option(issue1.id()) map Asset.fromCompatId
    maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1.compatId) map Asset.fromCompatId
    exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
  } yield (gen1, gen2, issue1, issue2, exchange)

  /**
    * Checks whether generated ExchangeTransactionV2 is valid.
    * In case of using orders of version 3 it is possible that matched amount of received asset is less than matcher's
    * fee in that asset. It leads to negative asset balance error
    */
  def transactionWithOrdersV3IsValid(ex: ExchangeTransaction): Boolean = {
    (ex.buyOrder, ex.sellOrder) match {
      case (_: OrderV3, _: Order) | (_: Order, _: OrderV3) =>
        val isBuyerReceiveAmountGreaterThanFee =
          if (ex.buyOrder.assetPair.amountAsset == ex.buyOrder.matcherFeeAssetId) {
            ex.buyOrder.getReceiveAmount(ex.amount, ex.price).right.get > ex.buyMatcherFee
          } else true

        val isSellerReceiveAmountGreaterThanFee =
          if (ex.sellOrder.assetPair.amountAsset == ex.sellOrder.matcherFeeAssetId) {
            ex.sellOrder.getReceiveAmount(ex.amount, ex.price).right.get > ex.sellMatcherFee
          } else true

        isBuyerReceiveAmountGreaterThanFee && isSellerReceiveAmountGreaterThanFee
      case _ => true
    }
  }

  /** Generates sequence of Longs with predefined sum and size */
  def getSeqWithPredefinedSum(sum: Long, count: Int): Seq[Long] = {

    val (rem, res) = (1 until count)
      .foldLeft((sum, List.empty[Long])) {
        case ((remainder, result), _) =>
          val next = java.util.concurrent.ThreadLocalRandom.current.nextLong(1, remainder)
          (remainder - next) -> (next :: result)
      }

    Random.shuffle(rem :: res)
  }

  /** Generates sequence of sell orders for one big buy order */
  def sellOrdersForBigBuyOrderGenerator(matcher: PublicKey,
                                        sellers: Seq[KeyPair],
                                        assetPair: AssetPair,
                                        price: Long,
                                        matcherFeeAssetId: Asset,
                                        totalAmount: Long,
                                        totalMatcherFee: Long): Gen[Seq[Order]] = {

    val randomAmountsAndFees =
      getSeqWithPredefinedSum(totalAmount, sellers.length) zip getSeqWithPredefinedSum(totalMatcherFee, sellers.length)

    val sellers2AmountsAndFees = sellers zip randomAmountsAndFees

    def timestampAndExpirationGenerator: Gen[(Long, Long)] = {
      for {
        timestamp  <- timestampGen
        expiration <- maxOrderTimeGen
      } yield (timestamp, expiration)
    }

    for { timestampsAndExpiration <- Gen.listOfN(sellers.length, timestampAndExpirationGenerator) } yield {

      (timestampsAndExpiration zip sellers2AmountsAndFees)
        .map {
          case ((timestamp, expiration), (seller, (amount, fee))) =>
            OrderV3(
              sender = seller,
              matcher = matcher,
              pair = assetPair,
              orderType = OrderType.SELL,
              amount = amount,
              price = price,
              timestamp = timestamp,
              expiration = expiration,
              matcherFee = fee,
              matcherFeeAssetId = matcherFeeAssetId
            )
        }
    }
  }

  /**
    * Returns preconditions for tests based on case when there is one big buy order and few small sell orders
    *
    * @param totalBuyMatcherFeeBoundaries function for manipulating of total matcher's fee paid by buyer in exchange transactions
    * @param sellersTotalAmount           function for manipulating of total sell orders amount
    */
  def oneBuyFewSellsPreconditions(totalBuyMatcherFeeBoundaries: Long => (Long, Long), sellersTotalAmount: Long => Long)
    : Gen[(List[GenesisTransaction], IssueTransaction, IssueTransaction, MassTransferTransaction, Seq[ExchangeTransactionV2], Order)] = {
    for {
      matcher                                                                                                        <- accountGen
      sellOrdersCount                                                                                                <- Gen.choose(1, 5)
      sellers                                                                                                        <- Gen.listOfN(sellOrdersCount, accountGen)
      (buyer, _, _, _, bigBuyOrderAmount, price, bigBuyOrderTimestamp, bigBuyOrderExpiration, bigBuyOrderMatcherFee) <- orderParamGen
      genesisTimestamp                                                                                               <- timestampGen
      issueTx1: IssueTransaction                                                                                     <- issueReissueBurnGeneratorP(Long.MaxValue - 1000L, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issueTx2: IssueTransaction                                                                                     <- issueReissueBurnGeneratorP(Long.MaxValue - 1000L, buyer).map(_._1).retryUntil(_.script.isEmpty)

      pair                                           = AssetPair(IssuedAsset(issueTx2.id()), IssuedAsset(issueTx1.id()))
      (minTotalBuyMatcherFee, maxTotalBuyMatcherFee) = totalBuyMatcherFeeBoundaries(bigBuyOrderMatcherFee)

      totalBuyMatcherFeeForExchangeTransactions <- Gen.choose(minTotalBuyMatcherFee, maxTotalBuyMatcherFee)

      bigBuyOrder = Order(
        sender = buyer,
        matcher = matcher,
        pair = pair,
        orderType = OrderType.BUY,
        amount = bigBuyOrderAmount,
        price = price,
        timestamp = bigBuyOrderTimestamp,
        expiration = bigBuyOrderExpiration,
        matcherFee = bigBuyOrderMatcherFee,
        version = 3: Byte,
        matcherFeeAssetId = IssuedAsset(issueTx1.id())
      )

      sellOrders <- sellOrdersForBigBuyOrderGenerator(
        matcher = matcher,
        assetPair = pair,
        price = price,
        matcherFeeAssetId = IssuedAsset(issueTx2.id()),
        sellers = sellers,
        totalAmount = sellersTotalAmount(bigBuyOrderAmount),
        totalMatcherFee = bigBuyOrderMatcherFee
      )
    } yield {

      val genesises = (matcher :: buyer :: sellers).map { recipient =>
        GenesisTransaction.create(recipient, ENOUGH_AMT, genesisTimestamp).explicitGet()
      }

      val massTransfer =
        MassTransferTransaction
          .selfSigned(
            assetId = IssuedAsset(issueTx2.id()),
            sender = buyer,
            transfers = sellers.map(seller => ParsedTransfer(seller, issueTx2.quantity / sellOrdersCount)),
            genesisTimestamp + 1000L,
            feeAmount = 1000L,
            Array.empty[Byte]
          )
          .explicitGet()

      val buyMatcherFees = getSeqWithPredefinedSum(totalBuyMatcherFeeForExchangeTransactions, sellOrdersCount)

      val exchanges = (sellOrders zip buyMatcherFees).map {
        case (sellOrder, buyMatcherFee) =>
          ExchangeTransactionV2
            .create(
              matcher = matcher,
              buyOrder = bigBuyOrder,
              sellOrder = sellOrder,
              amount = sellOrder.amount,
              price = bigBuyOrder.price,
              buyMatcherFee = buyMatcherFee,
              sellMatcherFee = sellOrder.matcherFee,
              fee = (bigBuyOrder.matcherFee + sellOrder.matcherFee) / 2,
              timestamp = Math.min(sellOrder.expiration, bigBuyOrder.expiration) - 10000
            )
            .explicitGet()
      }

      (genesises, issueTx1, issueTx2, massTransfer, exchanges, bigBuyOrder)
    }
  }

}
