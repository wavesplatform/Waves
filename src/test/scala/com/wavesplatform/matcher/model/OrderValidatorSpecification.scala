package com.wavesplatform.matcher.model

import com.google.common.base.Charsets
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.settings.Constants
import com.wavesplatform.settings.fee.OrderFeeSettings.{FixedWavesSettings, OrderFeeSettings, PercentSettings}
import com.wavesplatform.state.diffs.{CommonValidation, produce}
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

      "matcher's fee asset in order is blacklisted" in forAll(orderV3WithArbitraryFeeAssetGenerator) { order =>
        val orderValidator =
          OrderValidator
            .matcherSettingsAware(order.matcherPublicKey,
                                  Set.empty,
                                  order.matcherFeeAssetId.fold(Set.empty[IssuedAsset])(Set[IssuedAsset](_)),
                                  matcherSettings.orderFee) _

        orderValidator(order) should produce("FeeAssetBlacklisted")
      }

      "matcher's fee asset in order doesn't meet matcher's settings requirements (percent mode and arbitrary asset)" in forAll(
        orderV3WithArbitraryFeeAssetGenerator, // in percent mode it's not allowed to pay fee in arbitrary asset (only in one of the assets of the pair)
        percentSettingsGenerator
      ) {

        case (order, percentFeeSettings) =>
          val orderValidator =
            OrderValidator
              .matcherSettingsAware(order.matcherPublicKey, Set.empty, Set.empty, percentFeeSettings) _

          orderValidator(order) should produce("UnexpectedFeeAsset")
      }

      "matcher's fee asset in order doesn't meet matcher's settings requirements (fixed mode and incorrect asset)" in {

        val preconditions =
          for {
            order            <- orderV3Generator
            fixedFeeAsset    <- assetIdGen(1)
            fixedFeeSettings <- fixedSettingsGenerator(fixedFeeAsset)
          } yield (order, fixedFeeSettings)

        forAll(preconditions) {

          case (order, fixedFeeSettings) =>
            val orderValidator =
              OrderValidator
                .matcherSettingsAware(order.matcherPublicKey, Set.empty, Set.empty, fixedFeeSettings) _

            orderValidator(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcher's fee asset in order doesn't meet matcher's settings requirements (fixed-waves mode and incorrect asset)" in {
        val preconditions = for {
          order              <- orderV3Generator.filter(_.matcherFeeAssetId != Waves)
          fixedWavesSettings <- Gen.const(FixedWavesSettings(order.matcherFee - 1000L))
        } yield order -> fixedWavesSettings

        forAll(preconditions) {
          case (order, fixedWavesSettings) =>
            val orderValidator =
              OrderValidator
                .matcherSettingsAware(order.matcherPublicKey, Set.empty, Set.empty, fixedWavesSettings) _

            orderValidator(order) should produce("UnexpectedFeeAsset")
        }
      }

      "matcher's fee in order is too small (percent mode)" in {

        def updateOrder(order: Order, percentSettings: PercentSettings, offset: Long): Order = {
          order
            .updateFee(OrderValidator.getMinValidFee(order, percentSettings) + offset) // incorrect fee (less than minimal admissible by offset)
            .updateMatcherFeeAssetId(OrderValidator.getValidFeeAsset(order, percentSettings.assetType)) // but correct asset
        }

        val preconditions =
          for {
            percentFeeSettings <- percentSettingsGenerator
            order              <- orderV3Generator map (ord => updateOrder(ord, percentFeeSettings, -100L))
          } yield (order, percentFeeSettings)

        forAll(preconditions) {
          case (order, percentFeeSettings) =>
            val orderValidator =
              OrderValidator
                .matcherSettingsAware(order.matcherPublicKey, Set.empty, Set.empty, percentFeeSettings) _

            orderValidator(order) should produce("FeeNotEnough")
        }
      }

      "matcher's fee in order is too small (fixed mode)" in {

        val preconditions =
          for {
            order <- orderV3Generator
            fixedFeeSettings <- fixedSettingsGenerator(
              defaultAsset = order.matcherFeeAssetId,
              lowerMinFeeBound = order.matcherFee + 1, // fee in matcher's settings is greater than in order
              upperMinFeeBound = Math.min(order.matcherFee * 2, Long.MaxValue - 1)
            )
          } yield (order, fixedFeeSettings)

        forAll(preconditions) {

          case (order, fixedFeeSettings) =>
            val orderValidator =
              OrderValidator
                .matcherSettingsAware(order.matcherPublicKey, Set.empty, Set.empty, fixedFeeSettings) _

            orderValidator(order) should produce("FeeNotEnough")
        }
      }

      "matcher's fee in order is too small (fixed-waves mode)" in {

        val preconditions =
          for {
            order                 <- orderV3Generator filter (_.matcherFeeAssetId == Waves)
            fixedWavesFeeSettings <- Gen.const(FixedWavesSettings(order.matcherFee + 1000L))
          } yield (order, fixedWavesFeeSettings)

        forAll(preconditions) {

          case (order, fixedWavesFeeSettings) =>
            val orderValidator =
              OrderValidator
                .matcherSettingsAware(order.matcherPublicKey, Set.empty, Set.empty, fixedWavesFeeSettings) _

            orderValidator(order) should produce("FeeNotEnough")
        }
      }

      "matcher's fee is less than calculated by ExchangeTransactionCreator one" in {

        forAll(orderWithMatcherSettingsGenerator) {
          case (order, sender, orderFeeSettings) =>
            val blockchain = stub[Blockchain]

            activate(blockchain, BlockchainFeatures.SmartAccountTrading -> 0, BlockchainFeatures.OrderV3 -> 0)

            val transactionCreator = exchangeTransactionCreator(blockchain).createTransaction _

            val orderValidator =
              OrderValidator.blockchainAware(blockchain, transactionCreator, order.matcherPublicKey, ntpTime, orderFeeSettings) _

            val minFee         = ExchangeTransactionCreator.minFee(blockchain, order.matcherPublicKey, order.assetPair)
            val correctedOrder = Order.sign(order.updateFee(minFee - 1000L), sender)

            def setAssetsDescriptionAndEmptyScript(assets: Asset*): Unit = {
              assets.foreach {
                case asset @ IssuedAsset(_) =>
                  (blockchain.assetDescription _).when(asset).returns(mkAssetDescription(8))
                  (blockchain.assetScript _).when(asset).returns(None)
                case _ =>
              }
            }

            setAssetsDescriptionAndEmptyScript(order.assetPair.amountAsset, order.assetPair.priceAsset, order.matcherFeeAssetId)
            (blockchain.accountScript _).when(sender.toAddress).returns(None)

            orderFeeSettings match {
              case _: FixedWavesSettings => orderValidator(correctedOrder) should produce("FeeNotEnough")
              case _                     => orderValidator(correctedOrder) shouldBe 'right
            }
        }
      }

      "matcher's fee in order in insufficient in case of scripted account or asset" in {

        val preconditions =
          for {
            (order, sender, orderFeeSettings) <- orderWithMatcherSettingsGenerator.filter {
              case (order, _, _) => (order.assetPair.amountAsset != Waves) && (order.assetPair.priceAsset != Waves)
            }
          } yield {

            val minFee = orderFeeSettings match {
              case _: FixedWavesSettings => CommonValidation.FeeConstants(ExchangeTransaction.typeId) * CommonValidation.FeeUnit
              case _                     => order.matcherFee
            }

            val correctedOrder = Order.sign(order.updateFee(minFee), sender)

            correctedOrder -> orderFeeSettings
          }

        forAll(preconditions) {
          case (order, orderFeeSettings) =>
            val trueScript                       = ExprScript(Terms.TRUE).explicitGet()
            val setScriptAndValidateWithSettings = setScriptsAndValidate(orderFeeSettings) _

            orderFeeSettings match {
              case _: FixedWavesSettings =>
                setScriptAndValidateWithSettings(Some(trueScript), None, None)(order) should produce("FeeNotEnough")
                setScriptAndValidateWithSettings(None, Some(trueScript), None)(order) should produce("FeeNotEnough")
                setScriptAndValidateWithSettings(None, None, Some(trueScript))(order) should produce("FeeNotEnough")

                setScriptAndValidateWithSettings(None, None, None)(order) shouldBe 'right

              case _ =>
                setScriptAndValidateWithSettings(Some(trueScript), None, None)(order) shouldBe 'right
                setScriptAndValidateWithSettings(None, Some(trueScript), None)(order) shouldBe 'right
                setScriptAndValidateWithSettings(None, None, Some(trueScript))(order) shouldBe 'right

                setScriptAndValidateWithSettings(None, None, None)(order) shouldBe 'right
            }
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

  "sunny day test when order meets matcher's settings requirements" in forAll(orderWithMatcherSettingsGenerator) {
    case (order, _, orderFeeSettings) =>
      val orderValidator =
        OrderValidator
          .matcherSettingsAware(order.matcherPublicKey, Set.empty, Set.empty, orderFeeSettings) _

      orderValidator(order) shouldBe 'right
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

  private def msa(ba: Set[Address], o: Order) = OrderValidator.matcherSettingsAware(o.matcherPublicKey, ba, Set.empty, matcherSettings.orderFee) _

  private def setScriptsAndValidate(orderFeeSettings: OrderFeeSettings)(
      scriptForAmountAsset: Option[Script],
      scriptForPriceAsset: Option[Script],
      scriptForAccount: Option[Script])(order: Order): OrderValidator.Result[Order] = {
    val blockchain = stub[Blockchain]

    activate(blockchain, BlockchainFeatures.SmartAccountTrading -> 0, BlockchainFeatures.OrderV3 -> 0, BlockchainFeatures.SmartAssets -> 0)

    def prepareAssets(assetsAndScripts: (Asset, Option[Script])*): Unit = {
      assetsAndScripts.foreach {
        case (assetOption, scriptOption) =>
          assetOption match {
            case asset @ IssuedAsset(_) =>
              (blockchain.assetDescription _).when(asset).returns(mkAssetDescription(8))
              (blockchain.assetScript _).when(asset).returns(scriptOption)
              (blockchain.hasAssetScript _).when(asset).returns(scriptOption.isDefined)
            case _ =>
          }
      }
    }

    prepareAssets(order.assetPair.amountAsset -> scriptForAmountAsset,
                  order.assetPair.priceAsset  -> scriptForPriceAsset,
                  order.matcherFeeAssetId     -> None)

    (blockchain.accountScript _).when(MatcherAccount.toAddress).returns(scriptForAccount)
    (blockchain.hasScript _).when(MatcherAccount.toAddress).returns(scriptForAccount.isDefined)

    (blockchain.accountScript _).when(order.sender.toAddress).returns(None)
    (blockchain.hasScript _).when(order.sender.toAddress).returns(false)

    val transactionCreator = exchangeTransactionCreator(blockchain).createTransaction _

    OrderValidator.blockchainAware(blockchain, transactionCreator, MatcherAccount.toAddress, ntpTime, orderFeeSettings)(order)
  }

}
