package com.wavesplatform.matcher.model

import java.util.concurrent.ConcurrentHashMap

import com.google.common.base.Charsets
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.market.OrderBookActor.MarketStatus
import com.wavesplatform.matcher.model.OrderValidator.Result
import com.wavesplatform.settings.OrderFeeSettings.{FixedSettings, FixedWavesSettings, OrderFeeSettings, PercentSettings}
import com.wavesplatform.settings.{AssetType, Constants, DeviationsSettings}
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{AssetDescription, Blockchain, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.smart.script.{Script, ScriptCompiler}
import com.wavesplatform.transaction.{Asset, Proofs}
import com.wavesplatform.utils.randomBytes
import com.wavesplatform.{NoShrink, TestTime, WithDB}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OrderValidatorSpecification
    extends WordSpec
    with WithDB
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val wbtc          = mkAssetId("WBTC")
  private val pairWavesBtc  = AssetPair(Waves, wbtc)
  private val accountScript = ExprScript(V2, Terms.TRUE, checkSize = false).explicitGet()

  private val defaultPortfolio = Portfolio(0, LeaseBalance.empty, Map(wbtc -> 10 * Constants.UnitsInWave))

  "OrderValidator" should {
    "allow buying WAVES for BTC without balance for order fee" in asa() { v =>
      v shouldBe 'right
    }

    "reject new order" when {
      "this order had already been accepted" in asa(orderStatus = _ => true) { v =>
        v should produce("OrderDuplicate")
      }

      "sender's address is blacklisted" in {
        val blacklistedAccount = PrivateKeyAccount("3irbW78fffj5XDzAMjaEeo3kn8V".getBytes(Charsets.UTF_8))
        val o                  = newBuyOrder(blacklistedAccount)

        val v = msa(Set(blacklistedAccount.toAddress), o)
        v(o) should produce("AddressIsBlacklisted")
      }

      "v1 order from a scripted account" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.TRUE).explicitGet()))
          (bc.height _).when().returns(50).once()

          ov(newBuyOrder(scripted)) should produce("ScriptedAccountTradingUnsupported")
        }
      }

      "sender's address has a script, but trading from smart accounts hasn't been activated" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.TRUE).explicitGet()))
          (bc.height _).when().returns(50).anyNumberOfTimes()

          ov(newBuyOrder(scripted)) should produce("ScriptedAccountTradingUnsupported")
        }
      }

      "sender's address has a script returning FALSE" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.FALSE).explicitGet()))
          (bc.height _).when().returns(150).anyNumberOfTimes()

          ov(newBuyOrder(scripted, version = 2)) should produce("AccountScriptDeniedOrder")
        }
      }

      "order expires too soon" in forAll(Gen.choose[Long](1, OrderValidator.MinExpiration), accountGen) { (offset, pk) =>
        val tt       = new TestTime
        val unsigned = newBuyOrder
        val signed   = Order.sign(unsigned.updateExpiration(tt.getTimestamp() + offset).updateSender(pk), pk)

        OrderValidator.timeAware(tt)(signed) should produce("WrongExpiration")
      }

      "amount is invalid" in {
        val pk = PrivateKeyAccount(randomBytes())
        val unsigned = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(amount = 0L)
          case x: OrderV2 => x.copy(amount = 0L)
        }
        val signed = Order.sign(unsigned, pk)
        OrderValidator.timeAware(ntpTime)(signed) should produce("amount should be > 0")
      }

      "order signature is invalid" in portfolioTest(defaultPortfolio) { (ov, bc) =>
        val pk = PrivateKeyAccount(randomBytes())
        (bc.accountScript _).when(pk.toAddress).returns(None)
        val order = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
          case x: OrderV2 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
        }
        ov(order) should produce("InvalidSignature")
      }

      "order exists" in {
        val pk = PrivateKeyAccount(randomBytes())
        val ov = OrderValidator.accountStateAware(pk, defaultPortfolio.balanceOf, 1, _ => true)(_)
        ov(newBuyOrder(pk, 1000)) should produce("OrderDuplicate")
      }

      "order price has invalid non-zero trailing decimals" in forAll(assetIdGen(1), accountGen, Gen.choose(1, 7)) {
        case (amountAsset, sender, amountDecimals) =>
          portfolioTest(Portfolio(11 * Constants.UnitsInWave, LeaseBalance.empty, Map.empty)) { (ov, bc) =>
            (bc.hasScript _).when(sender.toAddress).returns(false)
            (bc.assetDescription _).when(amountAsset).returns(mkAssetDescription(amountDecimals))

            val price = BigDecimal(10).pow(-amountDecimals - 1)
            ov(
              buy(
                AssetPair(amountAsset, Waves),
                10 * Constants.UnitsInWave,
                price,
                matcherFee = Some((0.003 * Constants.UnitsInWave).toLong)
              )) should produce("Invalid price")
          }
      }

      "matcherFeeAssetId is blacklisted" in {
        val preconditions = for {
          matcherFeeAsset <- arbitraryAssetIdGen map (asset => IssuedAsset(asset.compatId.get))
          (_, order)      <- orderV3WithPredefinedFeeAssetGenerator(Some(matcherFeeAsset))
        } yield order -> matcherFeeAsset

        forAll(preconditions) {
          case (order, matcherFeeAssetId) =>
            validateByMatcherSettings(matcherSettings.orderFee, Set(matcherFeeAssetId))(order) should produce("FeeAssetBlacklisted")
        }
      }

      "matcherFeeAssetId doesn't meet matcher's settings requirements (percent mode and arbitrary asset)" in {
        val preconditions = for {
          (_, order)      <- orderV3WithPredefinedFeeAssetGenerator()
          percentSettings <- percentSettingsGenerator
        } yield order -> percentSettings

        // in percent mode it's not allowed to pay fee in arbitrary asset (only in one of the assets of the pair)

        forAll(preconditions) {
          case (order, percentFeeSettings) => validateByMatcherSettings(percentFeeSettings)(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFeeAssetId doesn't meet matcher's settings requirements (fixed mode and incorrect asset)" in {
        val preconditions =
          for {
            order            <- orderV3Generator
            fixedFeeAsset    <- arbitraryAssetIdGen
            fixedFeeSettings <- fixedSettingsGenerator(fixedFeeAsset)
          } yield (order, fixedFeeSettings)

        forAll(preconditions) {
          case (order, fixedFeeSettings) => validateByMatcherSettings(fixedFeeSettings)(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFeeAssetId doesn't meet matcher's settings requirements (waves mode and incorrect asset)" in {
        forAll(orderV3WithPredefinedFeeAssetGenerator()) {
          case (_, order) => validateByMatcherSettings(FixedWavesSettings(order.matcherFee))(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcherFee is too small (percent mode)" in {
        def setFeeLessThanMinBy(percentSettings: PercentSettings)(order: Order): Order = {
          order
            .updateFee(OrderValidator.getMinValidFeeForSettings(order, percentSettings, order.price) - 1)
            .updateMatcherFeeAssetId(OrderValidator.getValidFeeAssetForSettings(order, percentSettings))
        }

        val preconditions =
          for {
            percentFeeSettings <- percentSettingsGenerator
            order              <- orderV3Generator map setFeeLessThanMinBy(percentFeeSettings) // incorrect fee (less than minimal admissible by 1) but correct asset
          } yield (order, percentFeeSettings)

        forAll(preconditions) {
          case (order, percentFeeSettings) => validateByMatcherSettings(percentFeeSettings)(order) should produce("FeeNotEnough")
        }
      }

      "matcherFee is too small (fixed mode)" in forAll(orderV3Generator) { order =>
        validateByMatcherSettings(FixedSettings(order.matcherFeeAssetId, order.matcherFee + 1))(order) should produce("FeeNotEnough")
      }

      "matcherFee is too small (waves mode)" in forAll(orderV3WithPredefinedFeeAssetGenerator(Some(Waves))) {
        case (_, order) => validateByMatcherSettings(FixedWavesSettings(order.matcherFee + 1))(order) should produce("FeeNotEnough")
      }

      "matcherFee is less than calculated by ExchangeTransactionCreator one" in forAll(orderWithFeeSettingsGenerator) {
        case (order, sender, orderFeeSettings) =>
          val baseFee = orderFeeSettings match {
            case FixedWavesSettings(fee) => fee
            case _                       => OrderValidator.exchangeTransactionCreationFee
          }

          val orderValidator = setScriptsAndValidate(orderFeeSettings)(None, None, None, None) _ // assets and accounts don't have any scripts
          val minFee         = ExchangeTransactionCreator.minFee(stub[Blockchain], MatcherAccount, order.assetPair, baseFee)
          val correctedOrder = Order.sign(order.updateFee(minFee - 1), sender)

          orderFeeSettings match {
            case _: FixedWavesSettings => orderValidator(correctedOrder) should produce("FeeNotEnough")
            case _                     => orderValidator(correctedOrder) shouldBe 'right
          }
      }

      "matcherFee is insufficient in case of scripted account or asset" in forAll(orderWithoutWavesInPairAndWithFeeSettingsGenerator) {
        case (order, _, orderFeeSettings) =>
          val trueScript = ExprScript(Terms.TRUE).explicitGet()

          def setAssetsAndMatcherAccountScriptsAndValidate(amountAssetScript: Option[Script],
                                                           priceAssetScript: Option[Script],
                                                           matcherAccountScript: Option[Script]): Result[Order] =
            setScriptsAndValidate(orderFeeSettings)(amountAssetScript, priceAssetScript, None, matcherAccountScript)(order)

          orderFeeSettings match {
            case _: FixedWavesSettings =>
              setAssetsAndMatcherAccountScriptsAndValidate(Some(trueScript), None, None) should produce("FeeNotEnough")
              setAssetsAndMatcherAccountScriptsAndValidate(None, Some(trueScript), None) should produce("FeeNotEnough")
              setAssetsAndMatcherAccountScriptsAndValidate(None, None, Some(trueScript)) should produce("FeeNotEnough")

              setAssetsAndMatcherAccountScriptsAndValidate(None, None, None) shouldBe 'right

            case _ =>
              setAssetsAndMatcherAccountScriptsAndValidate(Some(trueScript), None, None) shouldBe 'right
              setAssetsAndMatcherAccountScriptsAndValidate(None, Some(trueScript), None) shouldBe 'right
              setAssetsAndMatcherAccountScriptsAndValidate(None, None, Some(trueScript)) shouldBe 'right

              setAssetsAndMatcherAccountScriptsAndValidate(None, None, None) shouldBe 'right
          }
      }

      "buy order's price is too high" in {
        val preconditions =
          for {
            bestAmount <- maxWavesAmountGen
            bestPrice  <- Gen.choose(1, (Long.MaxValue / bestAmount) - 100)

            bestAsk                = LevelAgg(bestAmount, bestPrice)
            deviationSettings      = DeviationsSettings(true, 50, 70, 50)
            tooHighPriceInBuyOrder = (bestAsk.price * (1 + (deviationSettings.maxPriceLoss / 100))).toLong + 50L

            (order, orderFeeSettings) <- orderWithFeeSettingsGenerator(OrderType.BUY, tooHighPriceInBuyOrder)
          } yield {
            val assetPair2MarketStatus = new ConcurrentHashMap[AssetPair, MarketStatus]
            assetPair2MarketStatus.put(order.assetPair, MarketStatus(None, None, Some(bestAsk)))
            (order, orderFeeSettings, deviationSettings, Option(assetPair2MarketStatus.get(order.assetPair)))
          }

        forAll(preconditions) {
          case (order, orderFeeSettings, deviationSettings, nonEmptyMarketStatus) =>
            OrderValidator.marketAware(orderFeeSettings, deviationSettings, nonEmptyMarketStatus)(order) should produce("DeviantOrderPrice")
        }
      }

      "sell order's price is out of deviation bounds" in {
        val fixedWavesFeeSettings = FixedWavesSettings(300000L)

        // seller cannot sell with price which:
        //   1. less than 50% of best bid (sell order price must be >= 2000)
        //   2. higher than 170% of best ask (sell order price must be <= 8500)

        val deviationSettings = DeviationsSettings(true, maxPriceProfit = 70, maxPriceLoss = 50, 50)
        val bestBid           = LevelAgg(1000L, 4000L)
        val bestAsk           = LevelAgg(1000L, 5000L)

        val tooLowPrice      = 1999L // = 50% of best bid (4000) - 1, hence invalid
        val lowButValidPrice = 2000L // = 50% of best bid (4000)

        val tooHighPrice      = 8501L // = 170% of best ask (5000) + 1, hence invalid
        val highButValidPrice = 8500L // = 170% of best ask (5000)

        val assetPair2MarketStatus = new ConcurrentHashMap[AssetPair, MarketStatus]
        assetPair2MarketStatus.put(pairWavesBtc, MarketStatus(None, Some(bestBid), Some(bestAsk)))
        val nonEmptyMarketStatus = assetPair2MarketStatus.get(pairWavesBtc)

        val tooLowPriceOrder =
          Order(
            sender = PrivateKeyAccount("seed".getBytes),
            matcher = MatcherAccount,
            pair = pairWavesBtc,
            orderType = OrderType.SELL,
            amount = 1000,
            price = tooLowPrice,
            timestamp = System.currentTimeMillis() - 10000L,
            expiration = System.currentTimeMillis() + 10000L,
            matcherFee = 1000L,
            version = 3: Byte,
            matcherFeeAssetId = Waves
          )

        val lowButValidPriceOrder  = tooLowPriceOrder.updatePrice(lowButValidPrice)
        val tooHighPriceOrder      = tooLowPriceOrder.updatePrice(tooHighPrice)
        val highButValidPriceOrder = tooLowPriceOrder.updatePrice(highButValidPrice)

        val orderValidator = OrderValidator.marketAware(fixedWavesFeeSettings, deviationSettings, Option(nonEmptyMarketStatus)) _

        orderValidator(tooLowPriceOrder) should produce("DeviantOrderPrice")
        orderValidator(lowButValidPriceOrder) shouldBe 'right

        orderValidator(tooHighPriceOrder) should produce("DeviantOrderPrice")
        orderValidator(highButValidPriceOrder) shouldBe 'right
      }

      "order's fee is out of deviation bounds" in {
        val percentSettings   = PercentSettings(AssetType.PRICE, 10)
        val deviationSettings = DeviationsSettings(true, 100, 100, maxPriceFee = 10)

        val bestAsk = LevelAgg(1000L, 4000L)

        val assetPair2MarketStatus = new ConcurrentHashMap[AssetPair, MarketStatus]
        assetPair2MarketStatus.put(pairWavesBtc, MarketStatus(None, None, Some(bestAsk)))
        val nonEmptyMarketStatus = assetPair2MarketStatus.get(pairWavesBtc)

        val order =
          Order(
            sender = PrivateKeyAccount("seed".getBytes),
            matcher = MatcherAccount,
            pair = pairWavesBtc,
            orderType = OrderType.BUY,
            amount = 1000,
            price = 1000,
            timestamp = System.currentTimeMillis() - 10000L,
            expiration = System.currentTimeMillis() + 10000L,
            matcherFee = 1000L,
            version = 3: Byte,
            matcherFeeAssetId = wbtc
          )

        val validFee     = OrderValidator.getMinValidFeeForSettings(order, percentSettings, bestAsk.price, 1 - (deviationSettings.maxPriceFee / 100))
        val validOrder   = order.updateFee(validFee)
        val invalidOrder = order.updateFee(validFee - 1L)

        val orderValidator = OrderValidator.marketAware(percentSettings, deviationSettings, Option(nonEmptyMarketStatus)) _

        orderValidator(invalidOrder) should produce("DeviantOrderMatcherFee")
        orderValidator(validOrder) shouldBe 'right
      }

      "assetPair is not in whitelist" in {
        val preconditions = for {
          (order, _, orderFeeSettings) <- orderWithFeeSettingsGenerator
          amountAsset                  <- arbitraryAssetIdGen
          priceAsset                   <- arbitraryAssetIdGen
        } yield (order, orderFeeSettings, AssetPair(amountAsset, priceAsset))

        forAll(preconditions) {
          case (order, orderFeeSettings, assetPair) =>
            validateByMatcherSettings(orderFeeSettings, allowedAssetPairs = Set(assetPair))(order) should produce("AssetPairIsNotAllowed")
            validateByMatcherSettings(orderFeeSettings, allowedAssetPairs = Set(order.assetPair))(order) shouldBe 'right
            validateByMatcherSettings(orderFeeSettings, allowedAssetPairs = Set.empty[AssetPair])(order) shouldBe 'right // empty allowedAssetPairs set means that all pairs are allowed
        }
      }

      "it's version = 3 and matcher disallows that" in forAll(orderWithFeeSettingsGenerator) {
        case (order, _, orderFeeSettings) =>
          if (order.version == 3) {
            validateByMatcherSettings(orderFeeSettings, allowOrderV3 = false)(order) should produce("OrderV3IsNotAllowed")
            validateByMatcherSettings(orderFeeSettings, allowOrderV3 = true)(order) shouldBe 'right
          } else {
            validateByMatcherSettings(orderFeeSettings, allowOrderV3 = false)(order) shouldBe 'right
            validateByMatcherSettings(orderFeeSettings, allowOrderV3 = true)(order) shouldBe 'right
          }
      }
    }

    "verify script of matcherFeeAssetId" in {
      forAll(orderV3WithFeeSettingsGenerator) {
        case (order, orderFeeSettings) =>
          def setFeeAssetScriptAndValidate(matcherFeeAssetScript: Option[Script]): Result[Order] =
            setScriptsAndValidate(orderFeeSettings)(None, None, matcherFeeAssetScript, None)(order)

          val (invalidScript, _) = ScriptCompiler.compile("(5 / 0) == 2").explicitGet()
          val falseScript        = ExprScript(Terms.FALSE).explicitGet()

          orderFeeSettings match {
            case _: FixedSettings =>
              setFeeAssetScriptAndValidate(Some(invalidScript)) should produce("AssetScriptReturnedError")
              setFeeAssetScriptAndValidate(Some(falseScript)) should produce("AssetScriptDeniedOrder")
              setFeeAssetScriptAndValidate(None) shouldBe 'right
            case _ =>
              // case _: FixedWavesSettings => it's impossible to set script for Waves
              // case _: PercentSettings    => matcherFeeAssetId script won't be validated since matcherFeeAssetId equals to one of the asset of the pair
              //                               (in that case additional validation of matcherFeeAssetId's script is not required)

              setFeeAssetScriptAndValidate(Some(invalidScript)) shouldBe 'right
              setFeeAssetScriptAndValidate(Some(falseScript)) shouldBe 'right
              setFeeAssetScriptAndValidate(None) shouldBe 'right
          }
      }
    }

    "validate order with any number of signatures from a scripted account" in forAll(Gen.choose(0, 5)) { proofsNumber =>
      validateOrderProofsTest((1 to proofsNumber).map(x => ByteStr(Array(x.toByte))))
    }

    "meaningful error for undefined functions in matcher" in portfolioTest(defaultPortfolio) { (ov, bc) =>
      activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)

      val pk     = PrivateKeyAccount(randomBytes())
      val o      = newBuyOrder(pk, version = 2)
      val script = ScriptCompiler("true && (height > 0)", isAssetScript = false).explicitGet()._1
      (bc.accountScript _).when(pk.toAddress).returns(Some(script))
      ov(o) should produce("height is inaccessible when running script on matcher")
    }

    "validate order with smart token" when {
      val asset1 = mkAssetId("asset1")
      val asset2 = mkAssetId("asset2")
      val pair   = AssetPair(asset1, asset2)
      val portfolio = Portfolio(10 * Constants.UnitsInWave,
                                LeaseBalance.empty,
                                Map(
                                  asset1 -> 10 * Constants.UnitsInWave,
                                  asset2 -> 10 * Constants.UnitsInWave
                                ))

      val permitScript = ExprScript(Terms.TRUE).explicitGet()
      val denyScript   = ExprScript(Terms.FALSE).explicitGet()

      "two assets are smart and they permit an order" when test { (ov, bc, o) =>
        (bc.assetScript _).when(asset1).returns(Some(permitScript))
        (bc.assetScript _).when(asset2).returns(Some(permitScript))

        ov(o) shouldBe 'right
      }

      "first asset is smart and it deny an order" when test { (ov, bc, o) =>
        (bc.assetScript _).when(asset1).returns(Some(denyScript))
        (bc.assetScript _).when(asset2).returns(None)

        ov(o) should produce("AssetScriptDeniedOrder")
      }

      "second asset is smart and it deny an order" when test { (ov, bc, o) =>
        (bc.assetScript _).when(asset1).returns(None)
        (bc.assetScript _).when(asset2).returns(Some(denyScript))

        ov(o) should produce("AssetScriptDeniedOrder")
      }

      def test(f: (Order => OrderValidator.Result[Order], Blockchain, Order) => Any): Unit = (1 to 2).foreach { version =>
        s"v$version" in portfolioTest(portfolio) { (ov, bc) =>
          val features = Seq(BlockchainFeatures.SmartAssets -> 0) ++ {
            if (version == 1) Seq.empty
            else Seq(BlockchainFeatures.SmartAccountTrading -> 0)
          }
          activate(bc, features: _*)
          (bc.assetDescription _).when(asset1).returns(mkAssetDescription(8))
          (bc.assetDescription _).when(asset2).returns(mkAssetDescription(8))

          val pk = PrivateKeyAccount(randomBytes())
          val o = buy(
            pair = pair,
            amount = 100 * Constants.UnitsInWave,
            price = 0.0022,
            sender = Some(pk),
            matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
            ts = Some(System.currentTimeMillis()),
            version = version.toByte
          )
          (bc.accountScript _).when(o.sender.toAddress).returns(None)
          f(ov, bc, o)
        }
      }
    }

    "deny OrderV2 if SmartAccountTrading hasn't been activated yet" in forAll(accountGen) { account =>
      portfolioTest(defaultPortfolio) { (ov, bc) =>
        activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
        (bc.accountScript _).when(account.toAddress).returns(Some(accountScript)).anyNumberOfTimes()
        (bc.height _).when().returns(0).anyNumberOfTimes()

        ov(newBuyOrder(account, version = 2)) should produce("OrderVersionUnsupported")
      }
    }

    "deny blockchain functions in account script" in forAll(accountGen) { account =>
      portfolioTest(defaultPortfolio) { (ov, bc) =>
        activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)
        (bc.height _).when().returns(0).anyNumberOfTimes()

        val scriptText =
          """match tx {
            |  case o: Order => height >= 0
            |  case _ => true
            |}""".stripMargin
        val script = ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
        (bc.accountScript _).when(account.toAddress).returns(Some(script)).anyNumberOfTimes()

        ov(newBuyOrder(account, version = 2)) should produce("height is inaccessible when running script on matcher")
      }
    }
  }

  "sunny day test when order meets matcher's settings requirements" in forAll(orderWithFeeSettingsGenerator) {
    case (order, _, orderFeeSettings) => validateByMatcherSettings(orderFeeSettings)(order) shouldBe 'right
  }

  private def portfolioTest(p: Portfolio)(f: (Order => OrderValidator.Result[Order], Blockchain) => Any): Unit = {
    val bc = stub[Blockchain]
    (bc.assetScript _).when(wbtc).returns(None)
    (bc.assetDescription _).when(wbtc).returns(mkAssetDescription(8)).anyNumberOfTimes()
    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)
    f(ov, bc)
  }

  private def validateOrderProofsTest(proofs: Seq[ByteStr]): Unit = {
    val bc = stub[Blockchain]
    val pk = PrivateKeyAccount(randomBytes())

    activate(bc, BlockchainFeatures.SmartAccountTrading -> 0)
    (bc.accountScript _).when(pk.toAddress).returns(Some(accountScript)).anyNumberOfTimes()
    (bc.height _).when().returns(1).anyNumberOfTimes()
    (bc.assetScript _).when(wbtc).returns(None)
    (bc.assetDescription _).when(wbtc).returns(mkAssetDescription(8)).anyNumberOfTimes()

    val order = OrderV2(
      senderPublicKey = pk,
      matcherPublicKey = MatcherAccount,
      assetPair = pairWavesBtc,
      amount = 100 * Constants.UnitsInWave,
      price = (0.0022 * Order.PriceConstant).toLong,
      timestamp = System.currentTimeMillis(),
      expiration = System.currentTimeMillis() + 60 * 60 * 1000L,
      matcherFee = (0.003 * Constants.UnitsInWave).toLong,
      orderType = OrderType.BUY,
      proofs = Proofs.empty
    )

    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)
    ov(order) shouldBe 'right
  }

  private def mkAssetDescription(decimals: Int): Option[AssetDescription] =
    Some(AssetDescription(MatcherAccount, Array.emptyByteArray, Array.emptyByteArray, decimals, reissuable = false, BigInt(0), None, 0))

  private def newBuyOrder: Order =
    buy(pair = pairWavesBtc, amount = 100 * Constants.UnitsInWave, price = 0.0022, matcherFee = Some((0.003 * Constants.UnitsInWave).toLong))

  private def newBuyOrder(pk: PrivateKeyAccount, ts: Long = 0, version: Byte = 1) =
    buy(
      pair = pairWavesBtc,
      amount = 100 * Constants.UnitsInWave,
      price = 0.0022,
      sender = Some(pk),
      matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
      ts = Some(ts),
      version = version
    )

  private def activate(bc: Blockchain, features: (BlockchainFeature, Int)*): Unit = {
    (bc.activatedFeatures _).when().returns(features.map(x => x._1.id -> x._2).toMap).anyNumberOfTimes()
  }

  private def mkOrderValidator(bc: Blockchain, tc: ExchangeTransactionCreator) =
    OrderValidator.blockchainAware(bc, tc.createTransaction, MatcherAccount, ntpTime, matcherSettings.orderFee)(_)

  private def tradableBalance(p: Portfolio)(assetId: Asset): Long = assetId.fold(p.spendableBalance)(p.assets.getOrElse(_, 0L))

  private def exchangeTransactionCreator(blockchain: Blockchain) =
    new ExchangeTransactionCreator(blockchain, MatcherAccount, matcherSettings.orderFee)

  private def asa[A](
      p: Portfolio = defaultPortfolio,
      orderStatus: ByteStr => Boolean = _ => false,
      o: Order = newBuyOrder
  )(f: OrderValidator.Result[Order] => A): A =
    f(OrderValidator.accountStateAware(o.sender, tradableBalance(p), 0, orderStatus)(o))

  private def msa(ba: Set[Address], o: Order) =
    OrderValidator.matcherSettingsAware(o.matcherPublicKey, ba, Set.empty, matcherSettings) _

  private def validateByMatcherSettings(orderFeeSettings: OrderFeeSettings,
                                        blacklistedAssets: Set[IssuedAsset] = Set.empty[IssuedAsset],
                                        allowedAssetPairs: Set[AssetPair] = Set.empty[AssetPair],
                                        allowOrderV3: Boolean = true): Order => Result[Order] =
    order =>
      OrderValidator
        .matcherSettingsAware(
          MatcherAccount,
          Set.empty,
          blacklistedAssets,
          matcherSettings.copy(orderFee = orderFeeSettings, allowedAssetPairs = allowedAssetPairs, allowOrderV3 = allowOrderV3)
        )(order)

  private def setScriptsAndValidate(orderFeeSettings: OrderFeeSettings)(
      amountAssetScript: Option[Script],
      priceAssetScript: Option[Script],
      matcherFeeAssetScript: Option[Script],
      matcherAccountScript: Option[Script])(order: Order): OrderValidator.Result[Order] = {

    val blockchain = stub[Blockchain]

    activate(blockchain, BlockchainFeatures.SmartAccountTrading -> 0, BlockchainFeatures.OrderV3 -> 0, BlockchainFeatures.SmartAssets -> 0)

    def prepareAssets(assetsAndScripts: (Asset, Option[Script])*): Unit = assetsAndScripts foreach {
      case (asset: IssuedAsset, scriptOption) =>
        (blockchain.assetDescription _).when(asset).returns(mkAssetDescription(8))
        (blockchain.assetScript _).when(asset).returns(scriptOption)
        (blockchain.hasAssetScript _).when(asset).returns(scriptOption.isDefined)
      case _ =>
    }

    prepareAssets(order.assetPair.amountAsset -> amountAssetScript,
                  order.assetPair.priceAsset  -> priceAssetScript,
                  order.matcherFeeAssetId     -> matcherFeeAssetScript)

    (blockchain.accountScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript)
    (blockchain.hasScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript.isDefined)

    (blockchain.accountScript _).when(order.sender.toAddress).returns(None)
    (blockchain.hasScript _).when(order.sender.toAddress).returns(false)

    val transactionCreator = exchangeTransactionCreator(blockchain).createTransaction _

    OrderValidator.blockchainAware(blockchain, transactionCreator, MatcherAccount.toAddress, ntpTime, orderFeeSettings)(order)
  }
}
