package com.wavesplatform.state.diffs

import cats.{Order => _, _}
import com.wavesplatform.account.{Address, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.FunctionIds.THROW
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.ExchangeTransactionDiff.getOrderFeePortfolio
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AccountBalanceError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{Order, _}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.utils._
import com.wavesplatform.{NoShrink, TestValues, TransactionGen, crypto}
import org.scalacheck.Gen
import org.scalatest.{EitherValues, Inside, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Random

class ExchangeTransactionDiffTest
    extends PropSpec
    with PropertyChecks
    with TransactionGen
    with Inside
    with NoShrink
    with WithDomain
    with EitherValues {

  private def wavesPortfolio(amt: Long) = Portfolio.waves(amt)

  val MATCHER: KeyPair = TestValues.keyPair

  val fs: FunctionalitySettings = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id       -> 0,
      BlockchainFeatures.SmartAssets.id         -> 0,
      BlockchainFeatures.SmartAccountTrading.id -> 0,
      BlockchainFeatures.Ride4DApps.id          -> 0
    )
  )

  val fsWithOrderFeature: FunctionalitySettings =
    fs.copy(preActivatedFeatures = fs.preActivatedFeatures ++ Map(BlockchainFeatures.OrderV3.id -> 0))

  val fsOrderMassTransfer: FunctionalitySettings =
    fsWithOrderFeature.copy(preActivatedFeatures = fsWithOrderFeature.preActivatedFeatures + (BlockchainFeatures.MassTransfer.id -> 0))

  val fsWithBlockV5: FunctionalitySettings =
    fsWithOrderFeature.copy(
      preActivatedFeatures = fsWithOrderFeature.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
    )

  private val estimator = ScriptEstimatorV2

  property("Validation fails when Order feature is not activation yet") {

    val preconditionsAndExchange
        : Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher.toAddress, ENOUGH_AMT, ts).explicitGet()
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
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id()) map Asset.fromCompatId
      maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1.compatId) map Asset.fromCompatId
      exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
    } yield (gen1, gen2, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        assertDiffAndState(
          Seq(TestBlock.create(Seq(gen1, gen2, issue1, issue2))),
          TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
          fsWithOrderFeature
        ) {
          case (blockDiff, _) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            blockDiff.portfolios(exchange.sender.toAddress).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
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
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher.toAddress, ENOUGH_AMT, ts).explicitGet()
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
        assertDiffAndState(
          Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2))),
          TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
          fsWithOrderFeature
        ) {
          case (blockDiff, _) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            val matcherPortfolio =
              Monoid.combineAll(blockDiff.portfolios.view.filterKeys(_.stringRepr == exchange.sender.toAddress.stringRepr).values)

            val restoredMatcherPortfolio =
              Monoid.combineAll(
                Seq(
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.buyOrder, exchange.buyMatcherFee),
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.sellOrder, exchange.sellMatcherFee),
                  wavesPortfolio(-exchange.fee)
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
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      buyerIssuedAsset  = IssuedAsset(issue1.id())
      sellerIssuedAsset = IssuedAsset(issue2.id())
      exchange <- exchangeV2GeneratorP(
        buyer = buyer,
        seller = seller,
        amountAssetId = sellerIssuedAsset,        // buyer buys sellerIssuedAsset (received asset)
        priceAssetId = buyerIssuedAsset,          // buyer sells buyerIssuedAsset
        buyMatcherFeeAssetId = sellerIssuedAsset, // buyer pays fee in sellerIssuedAsset (received asset)
        sellMatcherFeeAssetId = buyerIssuedAsset,
        fixedMatcher = Some(matcher),
        orderVersions = Set(3)
      ).retryUntil(ex => !transactionWithOrdersV3IsValid(ex)) // fee in sellerIssuedAsset (received asset) is greater than amount of received sellerIssuedAsset
    } yield (gen1, gen2, gen3, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, gen3, issue1, issue2, exchange) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2))), TestBlock.create(Seq(exchange)), fsWithOrderFeature) {
          blockDiffEi =>
            blockDiffEi should produce("negative asset balance")
        }
    }
  }

  property("Preserves assets invariant (matcher's fee in separately issued asset), stores match info, rewards matcher (Orders V3 are used)") {

    val preconditionsAndExchange = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue3: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issue4: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id()).map(Asset.fromCompatId)
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
        assertDiffAndState(
          Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2, issue3, issue4))),
          TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
          fsWithOrderFeature
        ) {
          case (blockDiff, _) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            val matcherPortfolio =
              Monoid.combineAll(blockDiff.portfolios.view.filterKeys(_.stringRepr == exchange.sender.toAddress.stringRepr).values)

            val restoredMatcherPortfolio =
              Monoid.combineAll(
                Seq(
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.buyOrder, exchange.buyMatcherFee),
                  ExchangeTransactionDiff.getOrderFeePortfolio(exchange.sellOrder, exchange.sellMatcherFee),
                  wavesPortfolio(-exchange.fee)
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
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher.toAddress, ENOUGH_AMT, ts).explicitGet()
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
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2))), TestBlock.create(Seq(exchange)), fsWithOrderFeature) {
          blockDiffEi =>
            blockDiffEi should produce("AccountBalanceError")
        }
    }
  }

  property("Validation fails when balance of asset issued separately (asset is not in the pair) is less than fee in that asset (Orders V3 are used)") {

    val preconditionsAndExchange = for {
      buyer   <- accountGen
      seller  <- accountGen
      matcher <- accountGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      gen3: GenesisTransaction = GenesisTransaction.create(matcher.toAddress, ENOUGH_AMT, ts).explicitGet()
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
        assertDiffEi(
          Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1, issue2, issue3, issue4))),
          TestBlock.create(Seq(exchange)),
          fsWithOrderFeature
        ) { blockDiffEi =>
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
          Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer), Block.ProtoBlockVersion)),
          TestBlock.create(exchanges, Block.ProtoBlockVersion),
          fsOrderMassTransfer
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
          Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer), Block.ProtoBlockVersion)),
          TestBlock.create(exchanges, Block.ProtoBlockVersion),
          fsOrderMassTransfer
        ) { blockDiffEi =>
          blockDiffEi should produce("Insufficient buy fee")
        }
    }
  }

  property("Validation fails when total sell amount overfills buy order amount") {

    val preconditions =
      oneBuyFewSellsPreconditions(
        totalBuyMatcherFeeBoundaries = (bigBuyOrderMatcherFee: Long) => (bigBuyOrderMatcherFee - 10000L, bigBuyOrderMatcherFee), // correct total buyMatcherFee in ex trs
        sellersTotalAmount = (bigBuyOrderAmount: Long) => bigBuyOrderAmount + 10000L                                             // sell orders overfill buy order
      )

    forAll(preconditions) {
      case (genesises, issueTx1, issueTx2, massTransfer, exchanges, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer), Block.ProtoBlockVersion)),
          TestBlock.create(exchanges, Block.ProtoBlockVersion),
          fsOrderMassTransfer
        ) { blockDiffEi =>
          blockDiffEi should produce("Too much buy")
        }
    }
  }

  property("buy waves without enough money for fee") {
    val preconditions: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, 1 * Constants.UnitsInWave, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueGen(buyer)
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
          assertDiffAndState(
            Seq(TestBlock.create(Seq(gen1, gen2, issue1))),
            TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
            fsWithOrderFeature
          ) {
            case (blockDiff, _) =>
              val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
              totalPortfolioDiff.balance shouldBe 0
              totalPortfolioDiff.effectiveBalance shouldBe 0
              totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

              blockDiff.portfolios(exchange.sender.toAddress).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
          }

          assertDiffEi(
            Seq(TestBlock.create(Seq(gen1, gen2, issue1))),
            TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
            fsWithBlockV5
          ) { ei =>
            ei should produce("AccountBalanceError")
          }
        }
    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: KeyPair, ts: Long): Either[ValidationError, ExchangeTransaction] = {
    val mf     = buy.matcherFee
    val amount = math.min(buy.amount, sell.amount)
    ExchangeTransaction.signed(
      1.toByte,
      matcher = matcher.privateKey,
      order1 = buy.asInstanceOf[Order],
      order2 = sell.asInstanceOf[Order],
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

    val preconditions: Gen[(KeyPair, KeyPair, KeyPair, GenesisTransaction, GenesisTransaction, IssueTransaction)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransaction <- issueGen(seller)
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(IssuedAsset(issue1.id()), Waves)
        val buy = Order.buy(
          Order.V1,
          sender = buyer,
          matcher = matcher.publicKey,
          pair = assetPair,
          amount = 1000000L,
          price = price,
          timestamp = Ts,
          expiration = Ts + 1,
          matcherFee = MatcherFee
        )
        val sell = Order.sell(
          Order.V1,
          sender = seller,
          matcher = matcher.publicKey,
          pair = assetPair,
          amount = 1L,
          price = price,
          timestamp = Ts,
          expiration = Ts + 1,
          matcherFee = MatcherFee
        )
        val tx = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fs) {
          case (blockDiff, state) =>
            blockDiff.portfolios(tx.sender.toAddress).balance shouldBe tx.buyMatcherFee + tx.sellMatcherFee - tx.fee
            state.balance(tx.sender.toAddress) shouldBe 0L
        }
    }
  }

  property("Not enough balance") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(KeyPair, KeyPair, KeyPair, GenesisTransaction, GenesisTransaction, IssueTransaction)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransaction <- issueGen(seller, fixedQuantity = Some(1000L))
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(IssuedAsset(issue1.id()), Waves)
        val buy = Order.buy(
          Order.V1,
          sender = buyer,
          matcher = matcher.publicKey,
          pair = assetPair,
          amount = issue1.quantity + 1,
          price = price,
          timestamp = Ts,
          expiration = Ts + 1,
          matcherFee = MatcherFee
        )
        val sell = Order.sell(
          Order.V1,
          sender = seller,
          matcher = matcher.publicKey,
          pair = assetPair,
          amount = issue1.quantity + 1,
          price = price,
          timestamp = Ts,
          expiration = Ts + 1,
          matcherFee = MatcherFee
        )
        val tx = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fsWithOrderFeature) { totalDiffEi =>
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

    val preconditions: Gen[(KeyPair, KeyPair, KeyPair, GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransaction)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
        gen3: GenesisTransaction = GenesisTransaction.create(matcher.toAddress, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransaction <- issueGen(buyer, fixedQuantity = Some(Long.MaxValue))
      } yield (buyer, seller, matcher, gen1, gen2, gen3, issue1)

    val (buyer, seller, matcher, gen1, gen2, gen3, issue1) = preconditions.sample.get
    val assetPair                                          = AssetPair(Waves, IssuedAsset(issue1.id()))

    val buy  = Order.buy(version = 1: Byte, buyer, matcher.publicKey, assetPair, 3100000000L, 238, Ts, Ts + 1, MatcherFee)
    val sell = Order.sell(version = 1: Byte, seller, matcher.publicKey, assetPair, 425532L, 235, Ts, Ts + 1, MatcherFee)
    val tx = ExchangeTransaction
      .signed(
        1.toByte,
        matcher = matcher.privateKey,
        order1 = buy,
        order2 = sell,
        amount = 425532,
        price = 238,
        buyMatcherFee = 41,
        sellMatcherFee = 300000,
        fee = buy.matcherFee,
        timestamp = Ts
      )
      .explicitGet()

    assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1))), TestBlock.create(Seq(tx))) { totalDiffEi =>
      inside(totalDiffEi) {
        case Right(diff) =>
          import diff.portfolios
          portfolios(buyer.toAddress).balance shouldBe (-41L + 425532L)
          portfolios(seller.toAddress).balance shouldBe (-300000L - 425532L)
          portfolios(matcher.toAddress).balance shouldBe (+41L + 300000L - tx.fee)
      }
    }
  }

  private val fsV2 = createSettings(
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
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }.toMap,
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
        val exchangeWithSmallFee = ExchangeTransaction
          .signed(2.toByte, MATCHER.privateKey, etx.buyOrder, etx.sellOrder, 1000000, 1000000, 0, 0, smallFee, etx.timestamp)
          .explicitGet()

        val exchangeWithEnoughFee = ExchangeTransaction
          .signed(TxVersion.V2, MATCHER.privateKey, etx.buyOrder, etx.sellOrder, 1000000, 1000000, 0, 0, enoughFee, etx.timestamp)
          .explicitGet()

        val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))

        val blockWithSmallFeeETx  = TestBlock.create(Seq(exchangeWithSmallFee))
        val blockWithEnoughFeeETx = TestBlock.create(Seq(exchangeWithEnoughFee))

        assertLeft(preconBlocks, blockWithSmallFeeETx, fsV2)("does not exceed minimal value of")
        assertDiffEi(preconBlocks, blockWithEnoughFeeETx, fsV2)(_.explicitGet())
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
        val exchangeWithResignedOrder = (exchange: @unchecked) match {
          case e1 @ ExchangeTransaction(TxVersion.V1, bo, so, _, _, _, _, _, _, _, _) =>
            val newSig = crypto.sign(PrivateKey(so.senderPublicKey), bo.bodyBytes())
            e1.copy(order1 = bo.copy(proofs = Proofs(Seq(newSig))))
          case e2 @ ExchangeTransaction(TxVersion.V2, bo, so, _, _, _, _, _, _, _, _) =>
            val newSig = crypto.sign(PrivateKey(bo.senderPublicKey), so.bodyBytes())
            e2.copy(order2 = so.copy(proofs = Proofs(Seq(newSig))))
        }

        val preconBlocks = Seq(
          TestBlock.create(Seq(gen1, gen2)),
          TestBlock.create(Seq(issue1, issue2))
        )

        val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

        assertLeft(preconBlocks, blockWithExchange, fsWithOrderFeature)("Proof doesn't validate as signature")
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
            crypto.sign(PrivateKey(exchange.sender), exchange.sellOrder.bodyBytes()),
            crypto.sign(PrivateKey(exchange.sellOrder.senderPublicKey), exchange.sellOrder.bodyBytes())
          )
        )

        val exchangeWithResignedOrder = (exchange: @unchecked) match {
          case e1 @ ExchangeTransaction(TxVersion.V1, _, so, _, _, _, _, _, _, _, _) =>
            e1.copy(order1 = so.copy(proofs = newProofs))
          case e2 @ ExchangeTransaction(TxVersion.V2, _, so, _, _, _, _, _, _, _, _) =>
            e2.copy(order1 = so.copy(proofs = newProofs))
        }

        val preconBlocks = Seq(
          TestBlock.create(Seq(gen1, gen2)),
          TestBlock.create(Seq(issue1, issue2))
        )

        val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

        assertLeft(preconBlocks, blockWithExchange, fsWithOrderFeature)("Proof doesn't validate as signature")
    }
  }

  property("Disable use Order on SmartAccount") {
    val enoughFee        = 100000000
    val script           = "true"
    val txScriptCompiled = ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1

    val sellerScript = Some(ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1)
    val buyerScript  = Some(ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1)

    forAll(for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      genesis = GenesisTransaction.create(MATCHER.toAddress, Long.MaxValue, ts).explicitGet()
      tr1     = createWavesTransfer(MATCHER, buyer.toAddress, Long.MaxValue / 3, enoughFee, ts + 1).explicitGet()
      tr2     = createWavesTransfer(MATCHER, seller.toAddress, Long.MaxValue / 3, enoughFee, ts + 2).explicitGet()
      asset1 = IssueTransaction(TxVersion.V2, buyer.publicKey, "Asset#1".utf8Bytes, Array.emptyByteArray, 1000000, 8, false, None, enoughFee, ts + 3)
        .signWith(buyer.privateKey)
      asset2 = IssueTransaction(TxVersion.V2, seller.publicKey, "Asset#2".utf8Bytes, Array.emptyByteArray, 1000000, 8, false, None, enoughFee, ts + 4)
        .signWith(seller.privateKey)
      setMatcherScript = SetScriptTransaction
        .selfSigned(1.toByte, MATCHER, Some(txScriptCompiled), enoughFee, ts + 5)
        .explicitGet()
      setSellerScript = SetScriptTransaction
        .selfSigned(1.toByte, seller, sellerScript, enoughFee, ts + 6)
        .explicitGet()
      setBuyerScript = SetScriptTransaction
        .selfSigned(1.toByte, buyer, buyerScript, enoughFee, ts + 7)
        .explicitGet()
      assetPair = AssetPair(IssuedAsset(asset1.id()), IssuedAsset(asset2.id()))
      o1 <- Gen.oneOf(
        Order.buy(Order.V1, seller, MATCHER.publicKey, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee),
        Order.buy(Order.V2, seller, MATCHER.publicKey, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee)
      )
      o2 <- Gen.oneOf(
        Order.sell(Order.V1, buyer, MATCHER.publicKey, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee),
        Order.sell(Order.V2, buyer, MATCHER.publicKey, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee)
      )
      exchangeTx = {
        ExchangeTransaction
          .signed(2.toByte, MATCHER.privateKey, o1, o2, 1000000, 1000000, enoughFee, enoughFee, enoughFee, ts + 10)
          .explicitGet()
      }
    } yield {
      val pretest = Seq(
        TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq(tr1, tr2)),
        TestBlock.create(Seq(asset1, asset2, setMatcherScript, setSellerScript, setBuyerScript))
      )
      val test = TestBlock.create(Seq(exchangeTx))
      if (o1.version == 2 && o2.version == 2) {
        assertDiffEi(pretest, test, fs) { diff =>
          diff.explicitGet()
        }
      } else {
        assertLeft(pretest, test, fs)("Can't process order with signature from scripted account")
      }
    }) { _ =>
      ()
    }
  }

  property("ExchangeTransaction with Orders V4 uses asset decimals for price calculation") {
    val enoughFee = 100000000L
    val buyer     = accountGen.sample.get
    val seller    = accountGen.sample.get

    val (preconditions, usdn, tidex, liquid) = {

      val genesisTxs = Seq(
        GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(MATCHER.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet()
      )
      val usdnTx = IssueTransaction
        .selfSigned(TxVersion.V3, buyer, "USD-N", "USD-N", ENOUGH_AMT, 6.toByte, false, None, enoughFee, ntpTime.correctedTime())
        .explicitGet()
      val tidexTx = IssueTransaction
        .selfSigned(TxVersion.V3, seller, "Tidex", "Tidex", ENOUGH_AMT, 2.toByte, false, None, enoughFee, ntpTime.correctedTime())
        .explicitGet()
      val liquidTx = IssueTransaction
        .selfSigned(TxVersion.V3, seller, "Liquid", "Liquid", ENOUGH_AMT, 8.toByte, false, None, enoughFee, ntpTime.correctedTime())
        .explicitGet()

      val usdn   = IssuedAsset(usdnTx.assetId)
      val tidex  = IssuedAsset(tidexTx.assetId)
      val liquid = IssuedAsset(liquidTx.assetId)

      (Seq(TestBlock.create(genesisTxs), TestBlock.create(Seq(usdnTx, tidexTx, liquidTx), Block.ProtoBlockVersion)), usdn, tidex, liquid)
    }

    def mkExchange(txv: Byte, bov: Byte, sov: Byte, amount: Long, txPrice: Long, boPrice: Long, soPrice: Long, pair: AssetPair)
        : ExchangeTransaction = {
      val buyOrder =
        Order.buy(bov, buyer, MATCHER.publicKey, pair, amount, boPrice, ntpTime.correctedTime(), ntpTime.getTimestamp() + 1000, enoughFee)
      val sellOrder =
        Order.sell(sov, seller, MATCHER.publicKey, pair, amount, soPrice, ntpTime.correctedTime(), ntpTime.getTimestamp() + 1000, enoughFee)
      ExchangeTransaction
        .signed(txv, MATCHER.privateKey, buyOrder, sellOrder, amount, txPrice, enoughFee, enoughFee, enoughFee, ntpTime.correctedTime())
        .explicitGet()
    }

    val wavesUsdn   = AssetPair(Waves, usdn)
    val tidexWaves  = AssetPair(tidex, Waves)
    val liquidWaves = AssetPair(liquid, Waves)

    val scenarios = Table(
      ("transaction with orders v3", "transaction with orders v4", "transaction with orders v3 and v4", "transaction with orders v4 and v3"),
      (
        mkExchange(TxVersion.V2, Order.V3, Order.V3, 55768188998L, 592600L, 592600L, 592600L, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 55768188998L, 59260000L, 59260000L, 59260000L, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 55768188998L, 59260000L, 592600L, 59260000L, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 55768188998L, 59260000L, 59260000L, 592600L, wavesUsdn)
      ),
      (
        mkExchange(TxVersion.V2, Order.V3, Order.V3, 213L, 35016774000000L, 35016774000000L, 35016774000000L, tidexWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 213L, 35016774L, 35016774L, 35016774L, tidexWaves),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 213L, 35016774L, 35016774000000L, 35016774L, tidexWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 213L, 35016774L, 35016774L, 35016774000000L, tidexWaves)
      ),
      (
        mkExchange(TxVersion.V2, Order.V3, Order.V3, 2000000000L, 13898832L, 13898832L, 13898832L, liquidWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 2000000000L, 13898832L, 13898832L, 13898832L, liquidWaves),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 2000000000L, 13898832L, 13898832L, 13898832L, liquidWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 2000000000L, 13898832L, 13898832L, 13898832L, liquidWaves)
      )
    )

    forAll(scenarios) {
      case (txWithV3, txWithV4, txWithV3V4, txWithV4V3) =>
        val portfolios = collection.mutable.ListBuffer[Map[Address, Portfolio]]()

        assertDiffAndState(preconditions, TestBlock.create(Seq(txWithV4), Block.ProtoBlockVersion), fsWithBlockV5) {
          case (blockDiff, _) => portfolios += blockDiff.portfolios
        }

        assertDiffAndState(preconditions, TestBlock.create(Seq(txWithV3V4), Block.ProtoBlockVersion), fsWithBlockV5) {
          case (blockDiff, _) => portfolios += blockDiff.portfolios
        }

        assertDiffAndState(preconditions, TestBlock.create(Seq(txWithV4V3), Block.ProtoBlockVersion), fsWithBlockV5) {
          case (blockDiff, _) => portfolios += blockDiff.portfolios
        }

        assertDiffAndState(preconditions, TestBlock.create(Seq(txWithV3), Block.ProtoBlockVersion), fsWithBlockV5) {
          case (blockDiff, _) => portfolios += blockDiff.portfolios
        }

        // all portfolios built on the state and on the composite blockchain are equal
        portfolios.forall(_ == portfolios.head) shouldBe true
    }
  }

  property("ExchangeTransaction V3 can have SELL order as order1 after BlockV5 activation") {
    val scenario =
      for {
        buyer  <- accountGen
        seller <- accountGen
        gtx1 = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet()
        gtx2 = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet()
        gtx3 = GenesisTransaction.create(MATCHER.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet()
        fee  = 100000000L
        itx1 <- issueGen(MATCHER, Some(ENOUGH_AMT), fixedDecimals = Some(8.toByte))
        itx2 <- issueGen(MATCHER, Some(ENOUGH_AMT), fixedDecimals = Some(8.toByte))
        ttx1 = TransferTransaction
          .selfSigned(
            TxVersion.V3,
            MATCHER,
            seller.toAddress,
            IssuedAsset(itx1.assetId),
            ENOUGH_AMT / 2,
            Waves,
            fee,
            ByteStr.empty,
            itx1.timestamp + 1
          )
          .explicitGet()
        ttx2 = TransferTransaction
          .selfSigned(
            TxVersion.V3,
            MATCHER,
            buyer.toAddress,
            IssuedAsset(itx1.assetId),
            ENOUGH_AMT / 2,
            Waves,
            fee,
            ByteStr.empty,
            itx1.timestamp + 1
          )
          .explicitGet()
        ttx3 = TransferTransaction
          .selfSigned(
            TxVersion.V3,
            MATCHER,
            seller.toAddress,
            IssuedAsset(itx2.assetId),
            ENOUGH_AMT / 2,
            Waves,
            fee,
            ByteStr.empty,
            itx2.timestamp + 1
          )
          .explicitGet()
        ttx4 = TransferTransaction
          .selfSigned(
            TxVersion.V3,
            MATCHER,
            buyer.toAddress,
            IssuedAsset(itx2.assetId),
            ENOUGH_AMT / 2,
            Waves,
            fee,
            ByteStr.empty,
            itx2.timestamp + 1
          )
          .explicitGet()
        assets = Seq(IssuedAsset(itx1.assetId), IssuedAsset(itx2.assetId), Waves)
        amountAsset <- Gen.oneOf(assets)
        priceAsset  <- Gen.oneOf(assets).filter(_ != amountAsset)
        tx          <- exchangeGeneratorP(buyer, seller, amountAsset, priceAsset, fixedMatcher = Some(MATCHER))
        fixed = tx
          .copy(
            version = TxVersion.V3,
            buyMatcherFee = fee,
            sellMatcherFee = fee,
            fee = fee,
            order1 = tx.order1.copy(version = Order.V4, matcherFee = fee).signWith(buyer.privateKey),
            order2 = tx.order2.copy(version = Order.V4, matcherFee = fee).signWith(seller.privateKey)
          )
          .signWith(MATCHER.privateKey)
        reversed = fixed
          .copy(
            order1 = fixed.order2,
            order2 = fixed.order1
          )
          .signWith(MATCHER.privateKey)
      } yield (Seq(TestBlock.create(Seq(gtx1, gtx2, gtx3)), TestBlock.create(Seq(itx1, itx2, ttx1, ttx2, ttx3, ttx4))), fixed, reversed)
    forAll(scenario) {
      case (preconditions, fixed, reversed) =>
        val portfolios = collection.mutable.ListBuffer[Map[Address, Portfolio]]()

        assertDiffAndState(preconditions, TestBlock.create(Seq(fixed)), fsWithBlockV5) {
          case (diff, _) =>
            portfolios += diff.portfolios
        }

        assertDiffAndState(preconditions, TestBlock.create(Seq(reversed)), fsWithBlockV5) {
          case (diff, _) =>
            portfolios += diff.portfolios
        }

        portfolios.tail.forall(_ == portfolios.head) shouldBe true
    }
  }

  property(s"Accepts failed transactions after ${BlockchainFeatures.BlockV5} activation") {
    val scenario =
      for {
        buyer  <- accountGen
        seller <- accountGen
        gTx1           = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet()
        gTx2           = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet()
        gTx3           = GenesisTransaction.create(MATCHER.toAddress, ENOUGH_AMT, ntpTime.getTimestamp()).explicitGet()
        fee            = 100000000L
        throwingScript = ExprScript(FUNCTION_CALL(Native(THROW), Nil)).explicitGet()
        quantity <- matcherAmountGen
        iTx = IssueTransaction
          .selfSigned(TxVersion.V2, seller, "Asset", "", quantity, 8, reissuable = false, Some(throwingScript), fee, ntpTime.getTimestamp() + 1)
          .explicitGet()
        asset = IssuedAsset(iTx.assetId)
        eTx <- exchangeGeneratorP(buyer, seller, asset, Waves, fixedMatcher = Some(MATCHER))
          .flatMap { tx =>
            for {
              sellAmount <- Gen.choose(1, quantity)
              buyAmount  <- Gen.choose(1, quantity)
              amount     <- Gen.choose(Math.min(sellAmount, buyAmount) / 2000, Math.min(sellAmount, buyAmount) / 1000)
            } yield tx
              .copy(
                amount = amount,
                order1 = tx.buyOrder.copy(amount = sellAmount).signWith(buyer.privateKey),
                order2 = tx.sellOrder.copy(amount = buyAmount).signWith(seller.privateKey),
                buyMatcherFee = (BigInt(tx.fee) * amount / buyAmount).toLong,
                sellMatcherFee = (BigInt(tx.fee) * amount / sellAmount).toLong
              )
              .signWith(MATCHER.privateKey)
          }
        buyerBalance   = Map(Waves -> ENOUGH_AMT, asset         -> 0L)
        sellerBalance  = Map(Waves -> (ENOUGH_AMT - fee), asset -> iTx.quantity)
        matcherBalance = Map(Waves -> ENOUGH_AMT, asset         -> 0L)
      } yield (eTx, (buyerBalance, sellerBalance, matcherBalance), Seq(gTx1, gTx2, gTx3, iTx), throwingScript)

    forAll(scenario) {
      case (exchange, (buyerBalance, sellerBalance, matcherBalance), genesisTxs, throwingScript) =>
        assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(exchange), Block.ProtoBlockVersion), fsWithOrderFeature) { ei =>
          ei.left.value
        }
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(exchange), Block.ProtoBlockVersion), fsWithBlockV5) {
          case (diff, state) =>
            diff.scriptsRun shouldBe 0
            diff.portfolios(exchange.sender.toAddress).balance shouldBe -exchange.fee
            diff.portfolios.get(exchange.buyOrder.sender.toAddress) shouldBe None
            diff.portfolios.get(exchange.sellOrder.sender.toAddress) shouldBe None

            diff.scriptsComplexity shouldBe DiffsCommon.countVerifierComplexity(Some(throwingScript), state, isAsset = true).explicitGet().get._2

            buyerBalance.foreach {
              case (asset, balance) =>
                state.balance(exchange.buyOrder.sender.toAddress, asset) shouldBe balance
            }
            sellerBalance.foreach {
              case (asset, balance) =>
                state.balance(exchange.sellOrder.sender.toAddress, asset) shouldBe balance
            }

            state.balance(exchange.sender.toAddress, Waves) shouldBe matcherBalance(Waves) - exchange.fee
            matcherBalance.collect { case b @ (IssuedAsset(_), _) => b }.foreach {
              case (asset, balance) =>
                diff.portfolios(exchange.sender.toAddress).balanceOf(asset) shouldBe 0L
                state.balance(exchange.sender.toAddress, asset) shouldBe balance
            }

            state.transactionInfo(exchange.id()).map(r => r._2 -> r._3) shouldBe Some((exchange, false))
        }
    }
  }

  property("Counts complexity correctly for failed transactions") {
    def test(
        priceAssetIssue: IssueTransaction,
        amountAssetIssue: IssueTransaction,
        order1FeeAssetIssue: IssueTransaction,
        order2FeeAssetIssue: IssueTransaction,
        complexity: Long
    ): Unit = {
      val order1 = TxHelpers.orderV3(
        OrderType.BUY,
        IssuedAsset(amountAssetIssue.assetId),
        IssuedAsset(priceAssetIssue.assetId),
        IssuedAsset(order1FeeAssetIssue.assetId)
      )
      val order2 = TxHelpers.orderV3(
        OrderType.SELL,
        IssuedAsset(amountAssetIssue.assetId),
        IssuedAsset(priceAssetIssue.assetId),
        IssuedAsset(order2FeeAssetIssue.assetId)
      )
      val exchange = TxHelpers.exchange(order1, order2)

      withDomain(
        domainSettingsWithFS(
          TestFunctionalitySettings.withFeatures(
            BlockchainFeatures.SmartAssets,
            BlockchainFeatures.SmartAccountTrading,
            BlockchainFeatures.OrderV3,
            BlockchainFeatures.BlockV5
          )
        )
      ) { d =>
        d.appendBlock(Seq(amountAssetIssue, priceAssetIssue, order1FeeAssetIssue, order2FeeAssetIssue).distinct: _*)
        val newBlock = d.createBlock(2.toByte, Seq(exchange))
        val diff     = BlockDiffer.fromBlock(d.blockchainUpdater, Some(d.lastBlock), newBlock, MiningConstraint.Unlimited, newBlock.header.generationSignature).explicitGet()
        diff.diff.scriptsComplexity shouldBe complexity
      }
    }

    withClue("price asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = TestValues.rejectAssetScript)
      val amountAssetIssue    = TxHelpers.issue(script = TestValues.assetScript)
      val order1FeeAssetIssue = TxHelpers.issue(script = TestValues.assetScript)
      val order2FeeAssetIssue = TxHelpers.issue(script = TestValues.assetScript)

      test(priceAssetIssue, amountAssetIssue, order1FeeAssetIssue, order2FeeAssetIssue, TestValues.rejectAssetScriptComplexity)

    }

    withClue("amount asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = TestValues.assetScript)
      val amountAssetIssue    = TxHelpers.issue(script = TestValues.rejectAssetScript)
      val order1FeeAssetIssue = TxHelpers.issue(script = TestValues.assetScript)
      val order2FeeAssetIssue = TxHelpers.issue(script = TestValues.assetScript)

      test(
        priceAssetIssue,
        amountAssetIssue,
        order1FeeAssetIssue,
        order2FeeAssetIssue,
        TestValues.assetScriptComplexity + TestValues.rejectAssetScriptComplexity
      )
    }

    withClue("order1 matcher fee asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = TestValues.assetScript)
      val amountAssetIssue    = TxHelpers.issue(script = TestValues.assetScript)
      val order1FeeAssetIssue = TxHelpers.issue(script = TestValues.rejectAssetScript)
      val order2FeeAssetIssue = TxHelpers.issue(script = TestValues.assetScript)

      test(
        priceAssetIssue,
        amountAssetIssue,
        order1FeeAssetIssue,
        order2FeeAssetIssue,
        2 * TestValues.assetScriptComplexity
      )
    }

    withClue("order2 matcher fee asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = TestValues.assetScript)
      val amountAssetIssue    = TxHelpers.issue(script = TestValues.assetScript)
      val order1FeeAssetIssue = TxHelpers.issue(script = TestValues.assetScript)
      val order2FeeAssetIssue = TxHelpers.issue(script = TestValues.rejectAssetScript)

      test(
        priceAssetIssue,
        amountAssetIssue,
        order1FeeAssetIssue,
        order2FeeAssetIssue,
        2 * TestValues.assetScriptComplexity
      )
    }
  }

  property("Counts exchange fee asset complexity") {
    def test(tradeableAssetIssue: IssueTransaction, feeAssetIssue: IssueTransaction, complexity: Long): Unit = {
      val order1   = TxHelpers.orderV3(OrderType.BUY, IssuedAsset(tradeableAssetIssue.assetId), IssuedAsset(feeAssetIssue.assetId))
      val order2   = TxHelpers.orderV3(OrderType.SELL, IssuedAsset(tradeableAssetIssue.assetId), IssuedAsset(feeAssetIssue.assetId))
      val exchange = TxHelpers.exchange(order1, order2)

      withDomain(
        domainSettingsWithFS(
          TestFunctionalitySettings.withFeatures(BlockchainFeatures.SmartAssets, BlockchainFeatures.SmartAccountTrading, BlockchainFeatures.OrderV3)
        )
      ) { d =>
        d.appendBlock(Seq(tradeableAssetIssue, feeAssetIssue).distinct: _*)
        val newBlock = d.createBlock(2.toByte, Seq(exchange))
        val diff     = BlockDiffer.fromBlock(d.blockchainUpdater, Some(d.lastBlock), newBlock, MiningConstraint.Unlimited, newBlock.header.generationSignature).explicitGet()
        diff.diff.scriptsComplexity shouldBe complexity

        val feeUnits = FeeValidation.getMinFee(d.blockchainUpdater, exchange).explicitGet().minFeeInWaves / FeeValidation.FeeUnit
        if (complexity > 0) feeUnits shouldBe 7
        else feeUnits shouldBe 3
      }
    }

    withClue("fee") {
      val tradeableAssetIssue = TxHelpers.issue()
      val feeAssetIssue       = TxHelpers.issue(script = TestValues.assetScript)
      test(tradeableAssetIssue, feeAssetIssue, 0)
    }

    withClue("asset") {
      val tradeableAssetIssue = TxHelpers.issue(script = TestValues.assetScript)
      val feeAssetIssue       = TxHelpers.issue()
      test(tradeableAssetIssue, feeAssetIssue, 1)
    }

    withClue("fee and asset") {
      val tradeableAssetIssue = TxHelpers.issue(script = TestValues.assetScript)
      val feeAssetIssue       = TxHelpers.issue(script = TestValues.assetScript)
      test(tradeableAssetIssue, feeAssetIssue, 1)
    }

    withClue("fee and asset (same asset)") {
      val tradeableAssetIssue = TxHelpers.issue(script = TestValues.assetScript)
      test(tradeableAssetIssue, tradeableAssetIssue, 1)
    }
  }

  def scriptGen(caseType: String, v: Boolean): Gen[String] = Gen.oneOf(true, false).map { full =>
    val expr =
      s"""
       |  match tx {
       |   case _: $caseType => $v
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

  def smartTradePreconditions(
      buyerScriptSrc: Gen[String],
      sellerScriptSrc: Gen[String],
      txScript: Gen[String]
  ): Gen[(GenesisTransaction, List[TransferTransaction], List[Transaction], ExchangeTransaction)] = {
    val enoughFee = 100000000

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
      genesis = GenesisTransaction.create(MATCHER.toAddress, Long.MaxValue, ts).explicitGet()
      tr1     = createWavesTransfer(MATCHER, buyer.toAddress, Long.MaxValue / 3, enoughFee, ts + 1).explicitGet()
      tr2     = createWavesTransfer(MATCHER, seller.toAddress, Long.MaxValue / 3, enoughFee, ts + 2).explicitGet()
      asset1 = IssueTransaction(TxVersion.V2, buyer.publicKey, "Asset#1".utf8Bytes, Array.emptyByteArray, 1000000, 8, false, None, enoughFee, ts + 3)
        .signWith(buyer.privateKey)
      asset2 = IssueTransaction(TxVersion.V2, seller.publicKey, "Asset#2".utf8Bytes, Array.emptyByteArray, 1000000, 8, false, None, enoughFee, ts + 4)
        .signWith(seller.privateKey)
      setMatcherScript = SetScriptTransaction
        .selfSigned(1.toByte, MATCHER, Some(txScriptCompiled), enoughFee, ts + 5)
        .explicitGet()
      setSellerScript = SetScriptTransaction
        .selfSigned(1.toByte, seller, sellerScript, enoughFee, ts + 6)
        .explicitGet()
      setBuyerScript = SetScriptTransaction
        .selfSigned(1.toByte, buyer, buyerScript, enoughFee, ts + 7)
        .explicitGet()
      assetPair = AssetPair(IssuedAsset(asset1.id()), IssuedAsset(asset2.id()))
      o1        = Order.buy(Order.V2, seller, MATCHER.publicKey, assetPair, 1000000, 1000000, ts + 8, ts + 10000, enoughFee)
      o2        = Order.sell(Order.V2, buyer, MATCHER.publicKey, assetPair, 1000000, 1000000, ts + 9, ts + 10000, enoughFee)
      exchangeTx = {
        ExchangeTransaction
          .signed(TxVersion.V2, MATCHER.privateKey, o1, o2, 1000000, 1000000, enoughFee, enoughFee, enoughFee, ts + 10)
          .explicitGet()
      }
    } yield (genesis, List(tr1, tr2), List(asset1, asset2, setMatcherScript, setSellerScript, setBuyerScript), exchangeTx)
  }

  val simpleTradePreconditions: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
    buyer  <- accountGen
    seller <- accountGen
    ts     <- timestampGen
    gen1: GenesisTransaction = GenesisTransaction.create(buyer.toAddress, ENOUGH_AMT, ts).explicitGet()
    gen2: GenesisTransaction = GenesisTransaction.create(seller.toAddress, ENOUGH_AMT, ts).explicitGet()
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
      case (_: Order, _: Order) | (_: Order, _: Order) =>
        val isBuyerReceiveAmountGreaterThanFee =
          if (ex.buyOrder.assetPair.amountAsset == ex.buyOrder.matcherFeeAssetId) {
            ExchangeTransactionDiff.getReceiveAmount(ex.buyOrder, 8, 8, ex.amount, ex.price).explicitGet() > ex.buyMatcherFee
          } else true

        val isSellerReceiveAmountGreaterThanFee =
          if (ex.sellOrder.assetPair.amountAsset == ex.sellOrder.matcherFeeAssetId) {
            ExchangeTransactionDiff.getReceiveAmount(ex.sellOrder, 8, 8, ex.amount, ex.price).explicitGet() > ex.sellMatcherFee
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
  def sellOrdersForBigBuyOrderGenerator(
      matcher: PublicKey,
      sellers: Seq[KeyPair],
      assetPair: AssetPair,
      price: Long,
      matcherFeeAssetId: Asset,
      totalAmount: Long,
      totalMatcherFee: Long
  ): Gen[Seq[Order]] = {

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
            Order.selfSigned(
              Order.V3,
              sender = seller,
              matcher = matcher,
              assetPair = assetPair,
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
  def oneBuyFewSellsPreconditions(
      totalBuyMatcherFeeBoundaries: Long => (Long, Long),
      sellersTotalAmount: Long => Long
  ): Gen[(List[GenesisTransaction], IssueTransaction, IssueTransaction, MassTransferTransaction, Seq[ExchangeTransaction], Order)] = {
    for {
      matcher         <- accountGen
      sellOrdersCount <- Gen.choose(1, 5)
      sellers         <- Gen.listOfN(sellOrdersCount, accountGen)

      (buyer, _, _, _, bigBuyOrderAmount, price, bigBuyOrderTimestamp, bigBuyOrderExpiration, bigBuyOrderMatcherFee) <- orderParamGen

      genesisTimestamp           <- timestampGen
      issueTx1: IssueTransaction <- issueReissueBurnGeneratorP(Long.MaxValue - 1000L, buyer).map(_._1).retryUntil(_.script.isEmpty)
      issueTx2: IssueTransaction <- issueReissueBurnGeneratorP(Long.MaxValue - 1000L, buyer).map(_._1).retryUntil(_.script.isEmpty)

      pair                                           = AssetPair(IssuedAsset(issueTx2.id()), IssuedAsset(issueTx1.id()))
      (minTotalBuyMatcherFee, maxTotalBuyMatcherFee) = totalBuyMatcherFeeBoundaries(bigBuyOrderMatcherFee)

      totalBuyMatcherFeeForExchangeTransactions <- Gen.choose(minTotalBuyMatcherFee, maxTotalBuyMatcherFee)

      bigBuyOrder = Order.selfSigned(
        version = 3: Byte,
        sender = buyer,
        matcher = matcher.publicKey,
        assetPair = pair,
        orderType = OrderType.BUY,
        amount = bigBuyOrderAmount,
        price = price,
        timestamp = bigBuyOrderTimestamp,
        expiration = bigBuyOrderExpiration,
        matcherFee = bigBuyOrderMatcherFee,
        matcherFeeAssetId = IssuedAsset(issueTx1.id())
      )

      sellOrders <- sellOrdersForBigBuyOrderGenerator(
        matcher = matcher.publicKey,
        assetPair = pair,
        price = price,
        matcherFeeAssetId = IssuedAsset(issueTx2.id()),
        sellers = sellers,
        totalAmount = sellersTotalAmount(bigBuyOrderAmount),
        totalMatcherFee = bigBuyOrderMatcherFee
      )
    } yield {

      val genesises = (matcher :: buyer :: sellers).map { recipient =>
        GenesisTransaction.create(recipient.toAddress, ENOUGH_AMT, genesisTimestamp).explicitGet()
      }

      val massTransfer =
        MassTransferTransaction
          .selfSigned(
            1.toByte,
            sender = buyer,
            assetId = IssuedAsset(issueTx2.id()),
            transfers = sellers.map(seller => ParsedTransfer(seller.toAddress, issueTx2.quantity / sellOrdersCount)),
            fee = 1000L,
            genesisTimestamp + 1000L,
            ByteStr.empty
          )
          .explicitGet()

      val buyMatcherFees = getSeqWithPredefinedSum(totalBuyMatcherFeeForExchangeTransactions, sellOrdersCount)

      val exchanges = (sellOrders zip buyMatcherFees).map {
        case (sellOrder, buyMatcherFee) =>
          ExchangeTransaction
            .signed(
              2.toByte,
              matcher = matcher.privateKey,
              order1 = bigBuyOrder,
              order2 = sellOrder,
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
