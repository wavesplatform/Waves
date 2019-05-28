package com.wavesplatform.matcher.market

import com.google.common.base.Charsets
import com.wavesplatform.OrderOps._
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.OrderValidator.ValidationResult
import com.wavesplatform.matcher.model._
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.{CommonValidation, produce}
import com.wavesplatform.state.{AssetDescription, Blockchain, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.smart.script.{Script, ScriptCompiler}
import com.wavesplatform.transaction.{AssetId, Proofs}
import com.wavesplatform.utils.randomBytes
import com.wavesplatform.{NoShrink, TestTime, WithDB}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class OrderValidatorSpecification
    extends WordSpec
    with WithDB
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val wbtc         = mkAssetId("WBTC").get
  private val pairWavesBtc = AssetPair(None, Some(wbtc))

  private val defaultPortfolio = Portfolio(0, LeaseBalance.empty, Map(wbtc -> 10 * Constants.UnitsInWave))

  "OrderValidator" should {
    "allow buying WAVES for BTC without balance for order fee" in asa() { v =>
      v shouldBe 'right
    }

    "reject new order" when {
      "this order had already been accepted" in asa(orderStatus = _ => true) { v =>
        v shouldBe Left("Order has already been placed")
      }

      "sender's address is blacklisted" in {
        val blacklistedAccount = PrivateKeyAccount("3irbW78fffj5XDzAMjaEeo3kn8V".getBytes(Charsets.UTF_8))
        val o                  = newBuyOrder(blacklistedAccount)

        val v = msa(Set(blacklistedAccount.toAddress), o)
        v(o) shouldBe Left("Invalid address")
      }

      "v1 order from a scripted account" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.TRUE).explicitGet()))
          (bc.height _).when().returns(50).once()

          ov(newBuyOrder(scripted)) should produce("Trading on scripted account isn't allowed yet")
        }
      }

      "sender's address has a script, but trading from smart accounts hasn't been activated" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.TRUE).explicitGet()))
          (bc.height _).when().returns(50).anyNumberOfTimes()

          ov(newBuyOrder(scripted)) should produce("Trading on scripted account isn't allowed yet")
        }
      }

      "sender's address has a script returning FALSE" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          activate(bc, BlockchainFeatures.SmartAccountTrading -> 100)
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ExprScript(Terms.FALSE).explicitGet()))
          (bc.height _).when().returns(150).anyNumberOfTimes()

          ov(newBuyOrder(scripted, version = 2)) should produce("Order rejected by script")
        }
      }

      "order expires too soon" in forAll(Gen.choose[Long](1, OrderValidator.MinExpiration), accountGen) { (offset, pk) =>
        val tt       = new TestTime
        val unsigned = newBuyOrder
        val signed   = Order.sign(unsigned.updateExpiration(tt.getTimestamp() + offset).updateSender(pk), pk)

        OrderValidator.timeAware(tt)(signed) shouldBe Left("Order expiration should be > 1 min")
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
        ov(order) should produce("Script doesn't exist and proof doesn't validate as signature")
      }

      "order exists" in {
        val pk = PrivateKeyAccount(randomBytes())
        val ov = OrderValidator.accountStateAware(pk, defaultPortfolio.balanceOf, 1, _ => true)(_)
        ov(newBuyOrder(pk, 1000)) should produce("Order has already been placed")
      }

      "order price has invalid non-zero trailing decimals" in forAll(assetIdGen(1), accountGen, Gen.choose(1, 7)) {
        case (Some(amountAsset), sender, amountDecimals) =>
          portfolioTest(Portfolio(11 * Constants.UnitsInWave, LeaseBalance.empty, Map.empty)) { (ov, bc) =>
            (bc.hasScript _).when(sender.toAddress).returns(false)
            (bc.assetDescription _).when(amountAsset).returns(mkAssetDescription(amountDecimals))

            val price = BigDecimal(10).pow(-amountDecimals - 1)
            ov(
              buy(
                AssetPair(Some(amountAsset), None),
                10 * Constants.UnitsInWave,
                price,
                matcherFee = Some((0.003 * Constants.UnitsInWave).toLong)
              )) should produce("Invalid price")
          }
      }

      "whiteListOnly is enabled" in {
        def validate(allowedAssetPairs: Set[AssetPair]): Order => OrderValidator.ValidationResult =
          OrderValidator.matcherSettingsAware(MatcherAccount, Set.empty, whiteListOnly = true, Set.empty, allowedAssetPairs)

        forAll(orderGenerator) {
          case (order, _) =>
            validate(Set.empty[AssetPair])(order) should produce(s"Trading is not allowed for the pair: ${order.assetPair}")

            validate(Set(order.assetPair))(order) shouldBe 'right

            validate(Set(AssetPair.createAssetPair("A1", "A2").get))(order) should produce(
              s"Trading is not allowed for the pair: ${order.assetPair}"
            )

            OrderValidator.matcherSettingsAware(
              MatcherAccount,
              blacklistedAddresses = Set(order.senderPublicKey.toAddress),
              whiteListOnly = true,
              blacklistedAssets = Set.empty,
              allowedAssetPairs = Set(order.assetPair)
            )(order) should produce("Invalid address")

            OrderValidator.matcherSettingsAware(
              MatcherAccount,
              blacklistedAddresses = Set.empty,
              whiteListOnly = true,
              blacklistedAssets = Set(order.assetPair.amountAsset, order.assetPair.priceAsset),
              allowedAssetPairs = Set(order.assetPair)
            )(order) shouldBe 'right
        }
      }

      "whiteListOnly is disabled" in {
        def validate(allowedAssetPairs: Set[AssetPair]): Order => OrderValidator.ValidationResult =
          OrderValidator.matcherSettingsAware(MatcherAccount, Set.empty, whiteListOnly = false, Set.empty, allowedAssetPairs)

        forAll(orderGenerator) {
          case (order, _) =>
            validate(Set.empty[AssetPair])(order) shouldBe 'right
            validate(Set(order.assetPair))(order) shouldBe 'right

            validate(Set(AssetPair.createAssetPair("A1", "A2").get))(order) shouldBe 'right

            OrderValidator.matcherSettingsAware(
              MatcherAccount,
              blacklistedAddresses = Set(order.senderPublicKey.toAddress),
              whiteListOnly = false,
              blacklistedAssets = Set.empty,
              allowedAssetPairs = Set(order.assetPair)
            )(order) should produce("Invalid address")

            OrderValidator.matcherSettingsAware(
              MatcherAccount,
              blacklistedAddresses = Set.empty,
              whiteListOnly = false,
              blacklistedAssets = Set(order.assetPair.amountAsset, order.assetPair.priceAsset),
              allowedAssetPairs = Set(order.assetPair)
            )(order) shouldBe 'right

            OrderValidator.matcherSettingsAware(
              MatcherAccount,
              blacklistedAddresses = Set.empty,
              whiteListOnly = false,
              blacklistedAssets = Set(order.assetPair.amountAsset),
              allowedAssetPairs = Set.empty
            )(order) should produce("Invalid amount asset")
        }
      }

      "matcherFee is not enough" in {
        def setScriptsAndValidate(amountAssetScript: Option[Script], priceAssetScript: Option[Script], matcherAccountScript: Option[Script])(
            disableExtraFeeForScript: Boolean)(order: Order): OrderValidator.ValidationResult = {

          val blockchain = stub[Blockchain]

          activate(blockchain, BlockchainFeatures.SmartAccountTrading -> 0, BlockchainFeatures.SmartAssets -> 0)

          def prepareAssets(assetsAndScripts: (Option[AssetId], Option[Script])*): Unit = assetsAndScripts foreach {
            case (asset: Some[AssetId], scriptOption) =>
              (blockchain.assetDescription _).when(asset.get).returns(mkAssetDescription(8))
              (blockchain.assetScript _).when(asset.get).returns(scriptOption)
              (blockchain.hasAssetScript _).when(asset.get).returns(scriptOption.isDefined)
            case _ =>
          }

          prepareAssets(order.assetPair.amountAsset -> amountAssetScript, order.assetPair.priceAsset -> priceAssetScript)

          (blockchain.accountScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript)
          (blockchain.hasScript _).when(MatcherAccount.toAddress).returns(matcherAccountScript.isDefined)

          (blockchain.accountScript _).when(order.sender.toAddress).returns(None)
          (blockchain.hasScript _).when(order.sender.toAddress).returns(false)

          val transactionCreator = exchangeTransactionCreator(blockchain).createTransaction _

          OrderValidator.blockchainAware(blockchain,
                                         transactionCreator,
                                         matcherSettings.minOrderFee,
                                         MatcherAccount.toAddress,
                                         ntpTime,
                                         disableExtraFeeForScript)(order)
        }

        val trueScript             = Some(ExprScript(Terms.TRUE).explicitGet())
        val minOrderWithScriptsFee = matcherSettings.minOrderFee + CommonValidation.ScriptExtraFee * 3

        val preconditions = for {
          assetPair <- distinctPairGen
          sender    <- accountGen
          order     <- orderGenerator(sender, assetPair)
        } yield Order.sign(order.updateFee(matcherSettings.minOrderFee), sender)

        forAll(preconditions) { order =>
          def validateOrderWithAllScripts(disableExtraFeeForScript: Boolean): ValidationResult =
            setScriptsAndValidate(trueScript, trueScript, trueScript)(disableExtraFeeForScript)(order)

          validateOrderWithAllScripts(disableExtraFeeForScript = true) shouldBe 'right
          validateOrderWithAllScripts(disableExtraFeeForScript = false) should produce(s"Order matcherFee should be >= $minOrderWithScriptsFee")
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
      val asset1 = mkAssetId("asset1").get
      val asset2 = mkAssetId("asset2").get
      val pair   = AssetPair(Some(asset1), Some(asset2))
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

        ov(o) should produce("Order rejected by script of asset")
      }

      "second asset is smart and it deny an order" when test { (ov, bc, o) =>
        (bc.assetScript _).when(asset1).returns(None)
        (bc.assetScript _).when(asset2).returns(Some(denyScript))

        ov(o) should produce("Order rejected by script of asset")
      }

      def test(f: (Order => OrderValidator.ValidationResult, Blockchain, Order) => Any): Unit = (1 to 2).foreach { version =>
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
        (bc.height _).when().returns(0).anyNumberOfTimes()

        ov(newBuyOrder(account, version = 2)) should produce("Orders of version 1 are only accepted")
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

  private def portfolioTest(p: Portfolio)(f: (Order => OrderValidator.ValidationResult, Blockchain) => Any): Unit = {
    val bc = stub[Blockchain]
    (bc.assetScript _).when(wbtc).returns(None)
    (bc.assetDescription _).when(wbtc).returns(mkAssetDescription(8)).anyNumberOfTimes()
    val tc = exchangeTransactionCreator(bc)
    val ov = mkOrderValidator(bc, tc)
    f(ov, bc)
  }

  private def validateOrderProofsTest(proofs: Seq[ByteStr]): Unit = {
    val bc            = stub[Blockchain]
    val pk            = PrivateKeyAccount(randomBytes())
    val accountScript = ExprScript(V2, Terms.TRUE, checkSize = false).explicitGet()

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
    OrderValidator.blockchainAware(bc,
                                   tc.createTransaction,
                                   (0.003 * Constants.UnitsInWave).toLong,
                                   MatcherAccount,
                                   ntpTime,
                                   matcherSettings.disableExtraFeeForScript)(_)

  private def tradableBalance(p: Portfolio)(assetId: Option[AssetId]): Long = assetId.fold(p.spendableBalance)(p.assets.getOrElse(_, 0L))

  private def exchangeTransactionCreator(blockchain: Blockchain) =
    new ExchangeTransactionCreator(blockchain, MatcherAccount, matcherSettings)

  private def asa[A](
      p: Portfolio = defaultPortfolio,
      orderStatus: ByteStr => Boolean = _ => false,
      o: Order = newBuyOrder
  )(f: Either[String, Order] => A): A =
    f(OrderValidator.accountStateAware(o.sender, tradableBalance(p), 0, orderStatus)(o))

  private def msa(ba: Set[Address], o: Order) =
    OrderValidator.matcherSettingsAware(o.matcherPublicKey, ba, whiteListOnly = false, Set.empty, matcherSettings.allowedAssetPairs) _
}
