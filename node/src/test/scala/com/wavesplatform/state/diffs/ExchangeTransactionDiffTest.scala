package com.wavesplatform.state.diffs

import cats.Order as _
import com.wavesplatform.account.{Address, AddressScheme, KeyPair, PrivateKey}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.history.{Domain, defaultSigner}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.FunctionIds.THROW
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.ExchangeTransactionDiff.getOrderFeePortfolio
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{AccountBalanceError, GenericError}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.assets.exchange.OrderPriceMode.{AssetDecimals, FixedDecimals, Default as DefaultPriceMode}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.utils.{EthEncoding, EthHelpers}
import com.wavesplatform.{TestValues, TestWallet, crypto}
import org.scalatest.{EitherValues, Inside}
import org.web3j.crypto.Bip32ECKeyPair

import scala.util.{Random, Try}

class ExchangeTransactionDiffTest extends PropSpec with Inside with WithDomain with EitherValues with TestWallet with EthHelpers {

  private def wavesPortfolio(amt: Long) = Portfolio.waves(amt)

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
    fsWithOrderFeature.copy(preActivatedFeatures = fsWithOrderFeature.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))

  val fsWithRideV6: FunctionalitySettings =
    fsWithBlockV5.copy(preActivatedFeatures = fsWithBlockV5.preActivatedFeatures + (BlockchainFeatures.RideV6.id -> 0))

  private val estimator = ScriptEstimatorV2

  property("Not enough order sender balance and failed script") {
    val buyer  = TxHelpers.signer(10)
    val seller = TxHelpers.signer(11)

    val issue = TxHelpers.issue()

    def withReadyDomain(f: (Domain, Boolean) => Unit): Unit = {
      val simpleScript = TxHelpers.script("""{-# STDLIB_VERSION 5 #-}
                                            |{-# CONTENT_TYPE DAPP #-}
                                            |{-# SCRIPT_TYPE ACCOUNT #-}
                                            |
                                            |@Verifier(tx)
                                            |func verify () = match(tx) {
                                            |    case _ => if (sigVerify(base58'', base58'', base58'')) then true else true
                                            |}""".stripMargin)
      val complexScript = TxHelpers.script(s"""{-# STDLIB_VERSION 5 #-}
                                              |{-# CONTENT_TYPE DAPP #-}
                                              |{-# SCRIPT_TYPE ACCOUNT #-}
                                              |
                                              |@Verifier(tx)
                                              |func verify () = match(tx) {
                                              |    case _ =>
                                              |      if (
                                              |        ${(1 to 9).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || \n")}
                                              |      ) then true else true
                                              |}""".stripMargin)

      for {
        (accName, account) <- ("matcher" -> TxHelpers.defaultSigner) :: ("buyer" -> buyer) :: Nil

        (scriptType, script) <- ("simple" -> simpleScript) :: ("complex" -> complexScript) :: Nil
        setScript = TxHelpers.setScript(account, script)
      } withClue(s"$scriptType script on $accName") {
        val isScriptSimple = scriptType == "simple"

        withClue("without fix")(withDomain(DomainPresets.RideV5) { d =>
          d.appendAndAssertSucceed(TxHelpers.genesis(TxHelpers.defaultAddress))
          d.appendAndAssertSucceed(issue, TxHelpers.transfer(TxHelpers.defaultSigner, setScript.sender.toAddress, TestValues.fee), setScript)
          f(d, isScriptSimple)
        })

        withClue("with fix")(withDomain(DomainPresets.RideV5.configure(_.copy(estimatorSumOverflowFixHeight = 4))) { d =>
          d.appendAndAssertSucceed(TxHelpers.genesis(TxHelpers.defaultAddress))
          d.appendAndAssertSucceed(issue, TxHelpers.transfer(TxHelpers.defaultSigner, setScript.sender.toAddress, TestValues.fee), setScript)
          f(d, true)
        })
      }
    }

    def doExchangeTest(d: Domain, shouldThrowBalanceError: Boolean)(exchange: ExchangeTransaction): Unit = {
      if (shouldThrowBalanceError) intercept[RuntimeException](d.appendBlock(exchange)).toString should include("AccountBalanceError")
      else Try(d.appendAndAssertFailed(exchange)) shouldBe Symbol("success")
    }

    withClue("insufficient balance of buyer")(withReadyDomain { case (d, shouldThrowBalanceError) =>
      d.appendAndAssertSucceed(
        TxHelpers.transfer(TxHelpers.defaultSigner, buyer.toAddress, 1, Waves),
        TxHelpers.transfer(TxHelpers.defaultSigner, seller.toAddress, 1, Waves),
        TxHelpers.transfer(TxHelpers.defaultSigner, seller.toAddress, 1, issue.asset)
      )

      val exchange = TxHelpers.exchangeFromOrders(
        TxHelpers.order(OrderType.BUY, issue.asset, Waves, Waves, 1L, 1_0000_0000L, sender = buyer),
        TxHelpers.order(OrderType.SELL, issue.asset, Waves, Waves, 1L, 1_0000_0000L, sender = seller)
      )
      doExchangeTest(d, shouldThrowBalanceError)(exchange)
    })

    withClue("insufficient balance of seller")(withReadyDomain { case (d, shouldThrowBalanceError) =>
      d.appendAndAssertSucceed(
        TxHelpers.transfer(TxHelpers.defaultSigner, buyer.toAddress, 2, Waves),
        TxHelpers.transfer(TxHelpers.defaultSigner, seller.toAddress, 1, Waves)
        // TxHelpers.transfer(TxHelpers.defaultSigner, seller.toAddress, 1, issue.asset)
      )

      val exchange = TxHelpers.exchangeFromOrders(
        TxHelpers.order(OrderType.BUY, issue.asset, Waves, Waves, 1L, 1_0000_0000L, sender = buyer),
        TxHelpers.order(OrderType.SELL, issue.asset, Waves, Waves, 1L, 1_0000_0000L, sender = seller)
      )
      doExchangeTest(d, shouldThrowBalanceError)(exchange)
    })

    withClue("insufficient balance of matcher")(withReadyDomain { case (d, _) =>
      d.appendAndAssertSucceed(
        TxHelpers.transfer(TxHelpers.defaultSigner, buyer.toAddress, 2, Waves),
        TxHelpers.transfer(TxHelpers.defaultSigner, seller.toAddress, 1, Waves),
        TxHelpers.transfer(TxHelpers.defaultSigner, seller.toAddress, 1, issue.asset)
      )

      val exchange = TxHelpers.exchangeFromOrders(
        TxHelpers.order(OrderType.BUY, issue.asset, Waves, Waves, 1L, 1_0000_0000L, sender = buyer, matcher = TxHelpers.secondSigner),
        TxHelpers.order(OrderType.SELL, issue.asset, Waves, Waves, 1L, 1_0000_0000L, sender = seller, matcher = TxHelpers.secondSigner),
        TxHelpers.secondSigner
      )
      doExchangeTest(d, shouldThrowBalanceError = true)(exchange)
    })

    withClue("insufficient balance for two orders")(withReadyDomain { case (d, shouldThrowBalanceError) =>
      val buyerAndSeller = buyer

      d.appendAndAssertSucceed(
        TxHelpers.transfer(TxHelpers.defaultSigner, buyerAndSeller.toAddress, 1, Waves) // fee for a single order
      )

      val exchange = TxHelpers.exchangeFromOrders(
        TxHelpers.order(OrderType.BUY, issue.asset, Waves, Waves, 2L, 1_0000_0000L, sender = buyerAndSeller),
        TxHelpers.order(OrderType.SELL, issue.asset, Waves, Waves, 2L, 1_0000_0000L, sender = buyerAndSeller)
      )
      doExchangeTest(d, shouldThrowBalanceError)(exchange)
    })
  }

  property("Validation fails when Order feature is not activation yet") {

    val preconditionsAndExchange: Seq[(Seq[GenesisTransaction], IssueTransaction, IssueTransaction, ExchangeTransaction)] = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue1  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      val issue2  = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)

      for {
        maybeAsset1 <- Seq(Some(issue1.id()), None)
        maybeAsset2 <- Seq(Some(issue2.id()), None) if maybeAsset1 != maybeAsset2
      } yield {
        val exchange = TxHelpers.exchangeFromOrders(
          TxHelpers.order(
            OrderType.BUY,
            Asset.fromCompatId(maybeAsset2),
            Asset.fromCompatId(maybeAsset1),
            sender = buyer,
            matcher = matcher,
            version = Order.V3
          ),
          TxHelpers.order(
            OrderType.SELL,
            Asset.fromCompatId(maybeAsset2),
            Asset.fromCompatId(maybeAsset1),
            sender = seller,
            matcher = matcher,
            version = Order.V3
          ),
          matcher,
          version = TxVersion.V2
        )

        (genesis, issue1, issue2, exchange)
      }
    }

    preconditionsAndExchange.foreach { case (genesis, issue1, issue2, exchange) =>
      assertDiffEi(Seq(TestBlock.create(genesis :+ issue1 :+ issue2)), TestBlock.create(Seq(exchange)), fs) { blockDiffEi =>
        blockDiffEi should produce("Order Version 3 feature has not been activated yet")
      }
    }
  }

  property("Preserves waves invariant, stores match info, rewards matcher") {

    val preconditionsAndExchange: Seq[(Seq[GenesisTransaction], IssueTransaction, IssueTransaction, ExchangeTransaction)] = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue1  = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      val issue2  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)

      for {
        maybeAsset1 <- Seq(Some(issue1.id()), None)
        maybeAsset2 <- Seq(Some(issue2.id()), None) if maybeAsset1 != maybeAsset2
        exchange <- Seq(
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(
              OrderType.BUY,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = buyer,
              matcher = matcher,
              version = Order.V1
            ),
            TxHelpers.order(
              OrderType.SELL,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = seller,
              matcher = matcher,
              version = Order.V1
            ),
            matcher,
            version = TxVersion.V1
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(
              OrderType.BUY,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = buyer,
              matcher = matcher,
              version = Order.V1
            ),
            TxHelpers.order(
              OrderType.SELL,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = seller,
              matcher = matcher,
              version = Order.V1
            ),
            matcher,
            version = TxVersion.V2
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(
              OrderType.BUY,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = buyer,
              matcher = matcher,
              version = Order.V2
            ),
            TxHelpers.order(
              OrderType.SELL,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = seller,
              matcher = matcher,
              version = Order.V2
            ),
            matcher,
            version = TxVersion.V2
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(
              OrderType.BUY,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = buyer,
              matcher = matcher,
              version = Order.V3
            ),
            TxHelpers.order(
              OrderType.SELL,
              Asset.fromCompatId(maybeAsset1),
              Asset.fromCompatId(maybeAsset2),
              sender = seller,
              matcher = matcher,
              version = Order.V3
            ),
            matcher,
            version = TxVersion.V2
          )
        )
      } yield (genesis, issue1, issue2, exchange)
    }

    preconditionsAndExchange.foreach { case (genesis, issue1, issue2, exchange) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis :+ issue1 :+ issue2)),
        TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
        fsWithOrderFeature
      ) { case (blockDiff, _) =>
        val totalPortfolioDiff: Portfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
        totalPortfolioDiff.assets.values.toSet should (be(Set()) or be(Set(0)))

        blockDiff.portfolios(exchange.sender.toAddress).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee.value
      }
    }
  }

  property("Preserves assets invariant (matcher's fee in one of the assets of the pair or in Waves), stores match info, rewards matcher") {

    val preconditionsAndExchange: Seq[(Seq[GenesisTransaction], IssueTransaction, IssueTransaction, ExchangeTransaction)] = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue1  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      val issue2  = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)

      (for {
        maybeAsset1           <- Seq(Some(issue1.id()), None).map(Asset.fromCompatId)
        maybeAsset2           <- Seq(Some(issue2.id()), None).map(Asset.fromCompatId) if maybeAsset1.compatId != maybeAsset2.compatId
        buyMatcherFeeAssetId  <- Seq(maybeAsset1, maybeAsset2)
        sellMatcherFeeAssetId <- Seq(maybeAsset1, maybeAsset2)
        exchange <- Seq(
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(OrderType.BUY, maybeAsset2, maybeAsset1, amount = 100000000, sender = buyer, matcher = matcher, version = Order.V1),
            TxHelpers.order(OrderType.SELL, maybeAsset2, maybeAsset1, amount = 100000000, sender = seller, matcher = matcher, version = Order.V1),
            matcher,
            version = TxVersion.V2
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(OrderType.BUY, maybeAsset2, maybeAsset1, amount = 100000000, sender = buyer, matcher = matcher, version = Order.V2),
            TxHelpers.order(OrderType.SELL, maybeAsset2, maybeAsset1, amount = 100000000, sender = seller, matcher = matcher, version = Order.V2),
            matcher,
            version = TxVersion.V2
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(
              OrderType.BUY,
              maybeAsset2,
              maybeAsset1,
              feeAsset = buyMatcherFeeAssetId,
              amount = 100000000,
              sender = buyer,
              matcher = matcher,
              version = Order.V3
            ),
            TxHelpers.order(
              OrderType.SELL,
              maybeAsset2,
              maybeAsset1,
              feeAsset = sellMatcherFeeAssetId,
              amount = 100000000,
              sender = seller,
              matcher = matcher,
              version = Order.V3
            ),
            matcher,
            version = TxVersion.V2
          )
        )
      } yield (genesis, issue1, issue2, exchange)).distinct
    }

    preconditionsAndExchange.foreach { case (genesis, issue1, issue2, exchange) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis :+ issue1 :+ issue2)),
        TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
        fsWithOrderFeature
      ) { case (blockDiff, _) =>
        val totalPortfolioDiff: Portfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
        totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

        val matcherPortfolio =
          blockDiff.portfolios.view
            .filterKeys(_.toString == exchange.sender.toAddress.toString)
            .values
            .fold(Portfolio())(_.combine(_).explicitGet())

        val restoredMatcherPortfolio =
          Seq(
            ExchangeTransactionDiff.getOrderFeePortfolio(exchange.buyOrder, exchange.buyMatcherFee),
            ExchangeTransactionDiff.getOrderFeePortfolio(exchange.sellOrder, exchange.sellMatcherFee),
            wavesPortfolio(-exchange.fee.value)
          ).fold(Portfolio())(_.combine(_).explicitGet())

        matcherPortfolio shouldBe restoredMatcherPortfolio
      }
    }
  }

  property("Validation fails when received amount of asset is less than fee in that asset (Orders V3 are used)") {
    val preconditionsAndExchange: (Seq[GenesisTransaction], IssueTransaction, IssueTransaction, ExchangeTransaction) = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis           = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue1            = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      val issue2            = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)
      val buyerIssuedAsset  = issue1.asset
      val sellerIssuedAsset = issue2.asset

      val exchange = TxHelpers.exchangeFromOrders(
        TxHelpers.order(
          OrderType.BUY,
          sellerIssuedAsset,
          buyerIssuedAsset,
          feeAsset = sellerIssuedAsset,
          fee = 10,
          sender = buyer,
          matcher = matcher,
          version = Order.V3
        ),
        TxHelpers.order(
          OrderType.SELL,
          sellerIssuedAsset,
          buyerIssuedAsset,
          feeAsset = buyerIssuedAsset,
          sender = seller,
          matcher = matcher,
          version = Order.V3
        ),
        matcher,
        version = TxVersion.V2
      )

      (genesis, issue1, issue2, exchange)
    }

    val (genesis, issue1, issue2, exchange) = preconditionsAndExchange
    assertDiffEi(Seq(TestBlock.create(genesis :+ issue1 :+ issue2)), TestBlock.create(Seq(exchange)), fsWithOrderFeature) { blockDiffEi =>
      blockDiffEi should produce("negative asset balance")
    }
  }

  property("Preserves assets invariant (matcher's fee in separately issued asset), stores match info, rewards matcher (Orders V3 are used)") {

    val preconditionsAndExchange = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue1  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      val issue2  = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)
      val issue3  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset3", version = TxVersion.V1)
      val issue4  = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset4", version = TxVersion.V1)

      for {
        maybeAsset1 <- Seq(Some(issue1.id()), None).map(Asset.fromCompatId)
        maybeAsset2 <- Seq(Some(issue2.id()), None).map(Asset.fromCompatId) if maybeAsset1.compatId != maybeAsset2.compatId
      } yield {
        val buyMatcherFeeAssetId  = issue3.asset
        val sellMatcherFeeAssetId = issue4.asset

        val exchange = TxHelpers.exchangeFromOrders(
          TxHelpers
            .order(OrderType.BUY, maybeAsset2, maybeAsset1, feeAsset = buyMatcherFeeAssetId, sender = buyer, matcher = matcher, version = Order.V3),
          TxHelpers.order(
            OrderType.SELL,
            maybeAsset2,
            maybeAsset1,
            feeAsset = sellMatcherFeeAssetId,
            sender = seller,
            matcher = matcher,
            version = Order.V3
          ),
          matcher,
          version = TxVersion.V2
        )

        (genesis, issue1, issue2, issue3, issue4, exchange)
      }
    }

    preconditionsAndExchange.foreach { case (genesis, issue1, issue2, issue3, issue4, exchange) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis :+ issue1 :+ issue2 :+ issue3 :+ issue4)),
        TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
        fsWithOrderFeature
      ) { case (blockDiff, _) =>
        val totalPortfolioDiff: Portfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
        totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

        val matcherPortfolio =
          blockDiff.portfolios.view
            .filterKeys(_.toString == exchange.sender.toAddress.toString)
            .values
            .fold(Portfolio())(_.combine(_).explicitGet())

        val restoredMatcherPortfolio =
          Seq(
            ExchangeTransactionDiff.getOrderFeePortfolio(exchange.buyOrder, exchange.buyMatcherFee),
            ExchangeTransactionDiff.getOrderFeePortfolio(exchange.sellOrder, exchange.sellMatcherFee),
            wavesPortfolio(-exchange.fee.value)
          ).fold(Portfolio())(_.combine(_).explicitGet())
        matcherPortfolio shouldBe restoredMatcherPortfolio
      }
    }
  }

  property("Validation fails in case of attempt to pay fee in unissued asset (Orders V3 are used)") {

    val preconditionsAndExchange: Seq[(Seq[GenesisTransaction], IssueTransaction, IssueTransaction, ExchangeTransaction)] = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue1  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      val issue2  = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)

      for {
        maybeAsset1 <- Seq(Some(issue1.id()), None).map(Asset.fromCompatId)
        maybeAsset2 <- Seq(Some(issue2.id()), None).map(Asset.fromCompatId) if maybeAsset1.compatId != maybeAsset2.compatId
      } yield {
        val matcherFeeAssetId = IssuedAsset(ByteStr.fill(32)(1))

        val exchange = TxHelpers.exchangeFromOrders(
          TxHelpers
            .order(OrderType.BUY, maybeAsset2, maybeAsset1, feeAsset = matcherFeeAssetId, sender = buyer, matcher = matcher, version = Order.V3),
          TxHelpers
            .order(OrderType.SELL, maybeAsset2, maybeAsset1, feeAsset = matcherFeeAssetId, sender = seller, matcher = matcher, version = Order.V3),
          matcher,
          version = TxVersion.V2
        )

        (genesis, issue1, issue2, exchange)
      }
    }

    preconditionsAndExchange.foreach { case (genesis, issue1, issue2, exchange) =>
      assertDiffEi(Seq(TestBlock.create(genesis :+ issue1 :+ issue2)), TestBlock.create(Seq(exchange)), fsWithOrderFeature) { blockDiffEi =>
        blockDiffEi should produce("AccountBalanceError")
      }
    }
  }

  property(
    "Validation fails when balance of asset issued separately (asset is not in the pair) is less than fee in that asset (Orders V3 are used)"
  ) {

    val preconditionsAndExchange = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis               = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue1                = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset1", version = TxVersion.V1)
      val issue2                = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset2", version = TxVersion.V1)
      val issue3                = TxHelpers.issue(buyer, ENOUGH_AMT / 1000000, name = "asset3", version = TxVersion.V1)
      val issue4                = TxHelpers.issue(seller, ENOUGH_AMT / 1000000, name = "asset4", version = TxVersion.V1)
      val buyerIssuedAsset      = issue1.asset
      val sellerIssuedAsset     = issue2.asset
      val buyMatcherFeeAssetId  = issue3.asset
      val sellMatcherFeeAssetId = issue4.asset
      val exchange = TxHelpers.exchangeFromOrders(
        TxHelpers.order(
          OrderType.BUY,
          sellerIssuedAsset,
          buyerIssuedAsset,
          feeAsset = buyMatcherFeeAssetId,
          fee = ENOUGH_AMT / 10,
          sender = buyer,
          matcher = matcher,
          version = Order.V3
        ),
        TxHelpers.order(
          OrderType.SELL,
          sellerIssuedAsset,
          buyerIssuedAsset,
          feeAsset = sellMatcherFeeAssetId,
          fee = ENOUGH_AMT / 10,
          sender = seller,
          matcher = matcher,
          version = Order.V3
        ),
        matcher,
        version = TxVersion.V2
      )

      (genesis, issue1, issue2, issue3, issue4, exchange)
    }

    val (genesis, issue1, issue2, issue3, issue4, exchange) = preconditionsAndExchange
    assertDiffEi(
      Seq(TestBlock.create(genesis :+ issue1 :+ issue2 :+ issue3 :+ issue4)),
      TestBlock.create(Seq(exchange)),
      fsWithOrderFeature
    ) { blockDiffEi =>
      blockDiffEi should produce("negative asset balance")
    }
  }

  property("Total matcher's fee (sum of matcher's fees in exchange transactions) is less than or equal to order's matcher fee") {

    val preconditions =
      oneBuyFewSellsPreconditions(
        totalBuyMatcherFeeBoundaries =
          (bigBuyOrderMatcherFee: Long) =>
            (bigBuyOrderMatcherFee - 1000L, bigBuyOrderMatcherFee), // sum of buyMatcherFee in ex trs <= specified in bigBuyOrder
        sellersTotalAmount = identity
      )

    val (genesises, issueTx1, issueTx2, massTransfer, exchanges, bigBuyOrder) = preconditions
    assertDiffAndState(
      Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer), Block.ProtoBlockVersion)),
      TestBlock.create(exchanges, Block.ProtoBlockVersion),
      fsOrderMassTransfer
    ) { case (blockDiff, _) =>
      val totalPortfolioDiff: Portfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())

      totalPortfolioDiff.balance shouldBe 0
      totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
      totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

      val combinedPortfolio =
        exchanges.map(ex => getOrderFeePortfolio(bigBuyOrder, ex.buyMatcherFee)).fold(Portfolio())(_.combine(_).explicitGet())

      val feeSumPaidByBuyer =
        bigBuyOrder.matcherFeeAssetId
          .fold(combinedPortfolio.balance)(combinedPortfolio.assets)

      (feeSumPaidByBuyer <= exchanges.head.buyOrder.matcherFee.value) shouldBe true
    }
  }

  property("Validation fails when total matcher's fee (sum of matcher's fees in exchange transactions) is greater than order's matcher fee") {

    val preconditions =
      oneBuyFewSellsPreconditions(
        totalBuyMatcherFeeBoundaries =
          (bigBuyOrderMatcherFee: Long) =>
            (bigBuyOrderMatcherFee + 1, bigBuyOrderMatcherFee + 100000L), // sum of buyMatcherFee in ex trs > specified in bigBuyOrder
        sellersTotalAmount = identity
      )

    val (genesises, issueTx1, issueTx2, massTransfer, exchanges, _) = preconditions
    assertDiffEi(
      Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer), Block.ProtoBlockVersion)),
      TestBlock.create(exchanges, Block.ProtoBlockVersion),
      fsOrderMassTransfer
    ) { blockDiffEi =>
      blockDiffEi should produce("Insufficient buy fee")
    }
  }

  property("Validation fails when total sell amount overfills buy order amount") {

    val preconditions =
      oneBuyFewSellsPreconditions(
        totalBuyMatcherFeeBoundaries =
          (bigBuyOrderMatcherFee: Long) => (bigBuyOrderMatcherFee - 10000L, bigBuyOrderMatcherFee), // correct total buyMatcherFee in ex trs
        sellersTotalAmount = (bigBuyOrderAmount: Long) => bigBuyOrderAmount + 10000L                // sell orders overfill buy order
      )

    val (genesises, issueTx1, issueTx2, massTransfer, exchanges, _) = preconditions
    assertDiffEi(
      Seq(TestBlock.create(genesises), TestBlock.create(Seq(issueTx1, issueTx2, massTransfer), Block.ProtoBlockVersion)),
      TestBlock.create(exchanges, Block.ProtoBlockVersion),
      fsOrderMassTransfer
    ) { blockDiffEi =>
      blockDiffEi should produce("Too much buy")
    }
  }

  property("buy waves without enough money for fee") {
    val preconditions: Seq[(Seq[GenesisTransaction], IssueTransaction, ExchangeTransaction)] = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(
        TxHelpers.genesis(buyer.toAddress, 1 * Constants.UnitsInWave),
        TxHelpers.genesis(seller.toAddress)
      )
      val issue = TxHelpers.issue(buyer)

      Seq(
        TxHelpers.exchangeFromOrders(
          TxHelpers
            .order(OrderType.BUY, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = buyer, matcher = matcher, version = Order.V1),
          TxHelpers
            .order(OrderType.SELL, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = seller, matcher = matcher, version = Order.V1),
          matcher,
          fee = 300000,
          version = TxVersion.V1
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers
            .order(OrderType.BUY, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = buyer, matcher = matcher, version = Order.V1),
          TxHelpers
            .order(OrderType.SELL, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = seller, matcher = matcher, version = Order.V1),
          matcher,
          fee = 300000,
          version = TxVersion.V2
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers
            .order(OrderType.BUY, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = buyer, matcher = matcher, version = Order.V2),
          TxHelpers
            .order(OrderType.SELL, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = seller, matcher = matcher, version = Order.V2),
          matcher,
          fee = 300000,
          version = TxVersion.V2
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers
            .order(OrderType.BUY, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = buyer, matcher = matcher, version = Order.V3),
          TxHelpers
            .order(OrderType.SELL, Waves, issue.asset, amount = 100000000L, fee = 300000, sender = seller, matcher = matcher, version = Order.V3),
          matcher,
          fee = 300000,
          version = TxVersion.V2
        )
      ).map { exchange =>
        (genesis, issue, exchange)
      }
    }

    preconditions.foreach { case (genesis, issue, exchange) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis :+ issue)),
        TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
        fsWithOrderFeature
      ) { case (blockDiff, _) =>
        val totalPortfolioDiff: Portfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
        totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

        blockDiff.portfolios(exchange.sender.toAddress).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee.value
      }

      assertDiffEi(
        Seq(TestBlock.create(genesis :+ issue)),
        TestBlock.create(Seq(exchange), Block.ProtoBlockVersion),
        fsWithBlockV5
      ) { ei =>
        ei should produce("AccountBalanceError")
      }
    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: KeyPair): ExchangeTransaction = {
    val mf     = buy.matcherFee.value
    val amount = math.min(buy.amount.value, sell.amount.value)
    TxHelpers.exchange(
      order1 = buy,
      order2 = sell,
      matcher = matcher,
      amount = amount,
      price = price,
      buyMatcherFee = (BigInt(mf) * amount / buy.amount.value).toLong,
      sellMatcherFee = (BigInt(mf) * amount / sell.amount.value).toLong,
      fee = buy.matcherFee.value
    )
  }

  property("small fee cases") {
    val MatcherFee = 1000000L

    val preconditions: (KeyPair, KeyPair, KeyPair, Seq[GenesisTransaction], IssueTransaction) = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue   = TxHelpers.issue(seller)

      (buyer, seller, matcher, genesis, issue)
    }

    val (buyer, seller, matcher, genesis, issue) = preconditions
    val buy =
      TxHelpers.order(OrderType.BUY, issue.asset, Waves, amount = 1000000L, fee = MatcherFee, sender = buyer, matcher = matcher, version = Order.V1)
    val sell = TxHelpers.order(OrderType.SELL, issue.asset, Waves, fee = MatcherFee, sender = seller, matcher = matcher, version = Order.V1)
    val tx   = createExTx(buy, sell, buy.price.value, matcher)
    assertDiffAndState(Seq(TestBlock.create(genesis :+ issue)), TestBlock.create(Seq(tx)), fs) { case (blockDiff, state) =>
      blockDiff.portfolios(tx.sender.toAddress).balance shouldBe tx.buyMatcherFee + tx.sellMatcherFee - tx.fee.value
      state.balance(tx.sender.toAddress) shouldBe 1L
    }
  }

  property("Not enough balance") {
    val MatcherFee = 300000L

    val preconditions: (KeyPair, KeyPair, KeyPair, Seq[GenesisTransaction], IssueTransaction) = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue   = TxHelpers.issue(seller, 1000L)

      (buyer, seller, matcher, genesis, issue)
    }

    val (buyer, seller, matcher, genesis, issue) = preconditions
    val buy = TxHelpers.order(
      OrderType.BUY,
      issue.asset,
      Waves,
      amount = issue.quantity.value + 1,
      fee = MatcherFee,
      sender = buyer,
      matcher = matcher,
      version = Order.V1
    )
    val sell = TxHelpers.order(
      OrderType.SELL,
      issue.asset,
      Waves,
      amount = issue.quantity.value + 1,
      fee = MatcherFee,
      sender = seller,
      matcher = matcher,
      version = Order.V1
    )
    val tx = createExTx(buy, sell, buy.price.value, matcher)
    assertDiffEi(Seq(TestBlock.create(genesis :+ issue)), TestBlock.create(Seq(tx)), fsWithOrderFeature) { totalDiffEi =>
      inside(totalDiffEi) { case Left(TransactionValidationError(AccountBalanceError(errs), _)) =>
        errs should contain key seller.toAddress
      }
    }
  }

  property("Diff for ExchangeTransaction works as expected and doesn't use rounding inside") {
    val MatcherFee = 300000L

    val preconditions: (KeyPair, KeyPair, KeyPair, Seq[GenesisTransaction], IssueTransaction) = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val issue   = TxHelpers.issue(buyer, Long.MaxValue)

      (buyer, seller, matcher, genesis, issue)
    }

    val (buyer, seller, matcher, genesis, issue) = preconditions

    val buy = TxHelpers.order(
      OrderType.BUY,
      Waves,
      issue.asset,
      amount = 3100000000L,
      price = 238,
      fee = MatcherFee,
      sender = buyer,
      matcher = matcher,
      version = Order.V1
    )
    val sell = TxHelpers.order(
      OrderType.SELL,
      Waves,
      issue.asset,
      amount = 425532L,
      price = 235,
      fee = MatcherFee,
      sender = seller,
      matcher = matcher,
      version = Order.V1
    )
    val tx = TxHelpers.exchange(
      order1 = buy,
      order2 = sell,
      matcher = matcher,
      amount = 425532,
      price = 238,
      buyMatcherFee = 41,
      sellMatcherFee = 300000,
      fee = buy.matcherFee.value,
      version = TxVersion.V1
    )

    assertDiffEi(Seq(TestBlock.create(genesis :+ issue)), TestBlock.create(Seq(tx))) { totalDiffEi =>
      inside(totalDiffEi) { case Right(diff) =>
        import diff.portfolios
        portfolios(buyer.toAddress).balance shouldBe (-41L + 425532L)
        portfolios(seller.toAddress).balance shouldBe (-300000L - 425532L)
        portfolios(matcher.toAddress).balance shouldBe (+41L + 300000L - tx.fee.value)
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

  private val RideV6 = DomainPresets.RideV6.blockchainSettings.functionalitySettings

  private def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): FunctionalitySettings =
    TestFunctionalitySettings.Enabled
      .copy(
        featureCheckBlocksPeriod = 1,
        blocksForFeatureActivation = 1,
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }.toMap
      )

  property(s"Exchange transaction with scripted matcher and orders needs extra fee ($ScriptExtraFee)") {
    for {
      buyerScriptSrc  <- script("Order", true)
      sellerScriptSrc <- script("Order", true)
      txScript        <- script("ExchangeTransaction", true)
    } yield {

      val (genesis, transfers, issueAndScripts, etx, matcher) = smartTradePreconditions(buyerScriptSrc, sellerScriptSrc, txScript)
      val enoughFee            = FeeValidation.ScriptExtraFee + FeeValidation.FeeConstants(TransactionType.Exchange) * FeeValidation.FeeUnit
      val smallFee             = enoughFee - 1
      val exchangeWithSmallFee = TxHelpers.exchange(etx.buyOrder, etx.sellOrder, matcher, 1000000, 1000000, 1, 1, fee = smallFee)

      val exchangeWithEnoughFee = TxHelpers.exchange(etx.buyOrder, etx.sellOrder, matcher, 1000000, 1000000, 1, 1, fee = enoughFee)

      val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))

      val blockWithSmallFeeETx  = TestBlock.create(Seq(exchangeWithSmallFee))
      val blockWithEnoughFeeETx = TestBlock.create(Seq(exchangeWithEnoughFee))

      assertLeft(preconBlocks, blockWithSmallFeeETx, fsV2)("does not exceed minimal value of")
      assertDiffEi(preconBlocks, blockWithEnoughFeeETx, fsV2)(_.explicitGet())
    }
  }

  property("ExchangeTransactions valid if all scripts succeeds") {
    for {
      buyerScriptSrc  <- script("Order", true)
      sellerScriptSrc <- script("Order", true)
      txScript        <- script("ExchangeTransaction", true)
    } yield {
      val (genesis, transfers, issueAndScripts, exchangeTx, _) = smartTradePreconditions(buyerScriptSrc, sellerScriptSrc, txScript)
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
    for {
      buyerScriptSrc  <- script("Order", false, complex = true)
      sellerScriptSrc <- script("Order", true)
      txScript        <- script("ExchangeTransaction", true)
    } yield {
      val (genesis, transfers, issueAndScripts, exchangeTx, _) = smartTradePreconditions(buyerScriptSrc, sellerScriptSrc, txScript)
      val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
      assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), RideV6)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransactions invalid if seller scripts fails") {
    for {
      buyerScriptSrc  <- script("Order", true)
      sellerScriptSrc <- script("Order", false, complex = true)
      txScript        <- script("ExchangeTransaction", true)
    } yield {
      val (genesis, transfers, issueAndScripts, exchangeTx, _) = smartTradePreconditions(buyerScriptSrc, sellerScriptSrc, txScript)
      val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
      assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), RideV6)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransactions invalid if matcher script fails") {
    for {
      buyerScriptSrc  <- script("Order", true)
      sellerScriptSrc <- script("Order", true)
      txScript        <- script("ExchangeTransaction", false, complex = true)
    } yield {
      val (genesis, transfers, issueAndScripts, exchangeTx, _) = smartTradePreconditions(buyerScriptSrc, sellerScriptSrc, txScript)
      val preconBlocks = Seq(TestBlock.create(Seq(genesis)), TestBlock.create(transfers), TestBlock.create(issueAndScripts))
      assertLeft(preconBlocks, TestBlock.create(Seq(exchangeTx)), RideV6)("TransactionNotAllowedByScript")
    }
  }

  property("ExchangeTransaction invalid if order signature invalid") {
    simpleTradePreconditions.foreach { case (genesis, issue1, issue2, exchange) =>
      val exchangeWithResignedOrder = (exchange: @unchecked) match {
        case e1 @ ExchangeTransaction(TxVersion.V1, bo, so, _, _, _, _, _, _, _, _) =>
          val newSig = crypto.sign(PrivateKey(so.senderPublicKey), bo.bodyBytes())
          e1.copy(order1 = bo.withProofs(Proofs(Seq(newSig))))
        case e2 @ ExchangeTransaction(TxVersion.V2, bo, so, _, _, _, _, _, _, _, _) =>
          val newSig = crypto.sign(PrivateKey(bo.senderPublicKey), so.bodyBytes())
          e2.copy(order2 = so.withProofs(Proofs(Seq(newSig))))
      }

      val preconBlocks = Seq(
        TestBlock.create(genesis),
        TestBlock.create(Seq(issue1, issue2))
      )

      val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

      assertLeft(preconBlocks, blockWithExchange, fsWithOrderFeature)("Proof doesn't validate as signature")
    }
  }

  property("ExchangeTransaction invalid if exchange.price > buyOrder.price or exchange.price < sellOrder.price") {
    val buyer   = TxHelpers.signer(1)
    val seller  = TxHelpers.signer(2)
    val matcher = TxHelpers.signer(3)

    val genesis            = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
    val amountAssetIssueTx = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset2", decimals = 8, version = TxVersion.V1)
    val priceAssetIssueTx  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset1", decimals = 6, version = TxVersion.V1)
    val baseBlocks         = Seq(TestBlock.create(genesis :+ priceAssetIssueTx :+ amountAssetIssueTx))

    val amountAsset = Asset.IssuedAsset(amountAssetIssueTx.assetId)
    val priceAsset  = Asset.IssuedAsset(priceAssetIssueTx.assetId)

    def mkTestOrder(tpe: OrderType, price: Long, version: Int, priceMode: OrderPriceMode) = TxHelpers.order(
      tpe,
      amountAsset,
      priceAsset,
      price = price,
      sender = buyer,
      matcher = matcher,
      version = version.toByte,
      priceMode = priceMode
    )

    // decimalPrice = 12.5
    val cases = {
      val case1OldTxs = for {
        txVersion <- 1 to 2
        // See ExchangeTxValidator
        ordersVersions = txVersion match {
          case 1 => 1 to 1
          case 2 => 1 to 3
        }

        buyOrderVersion  <- ordersVersions
        sellOrderVersion <- ordersVersions

        r <- Seq(
          // normalizedPrice = decimalPrice * 10^(8 + priceAssetDecimals - amountAssetDecimals) = 12_500_000
          (
            txVersion,
            12_500_000L,
            mkTestOrder(OrderType.BUY, 12_400_000L, buyOrderVersion, OrderPriceMode.Default),
            mkTestOrder(OrderType.SELL, 12_500_000L, sellOrderVersion, OrderPriceMode.Default),
            "exchange.price = 12_500_000 should be <= buyOrder.price = 12_400_000"
          ),
          (
            txVersion,
            12_500_000L,
            mkTestOrder(OrderType.BUY, 12_500_000L, buyOrderVersion, OrderPriceMode.Default),
            mkTestOrder(OrderType.SELL, 12_600_000L, sellOrderVersion, OrderPriceMode.Default),
            "exchange.price = 12_500_000 should be >= sellOrder.price = 12_600_000"
          )
        )
      } yield r

      val case1NewTxs = Seq(
        // normalizedPrice = decimalPrice * 10^(priceAssetDecimals - amountAssetDecimals) = 15
        (
          3,
          1_250_000_000L,
          mkTestOrder(OrderType.BUY, 12_400_000L, 4, OrderPriceMode.AssetDecimals),
          mkTestOrder(OrderType.SELL, 12_500_000L, 4, OrderPriceMode.AssetDecimals),
          "exchange.price = 1_250_000_000 should be <= buyOrder.price = 1_240_000_000 (assetDecimals price = 12_400_000)"
        ),
        (
          3,
          1_250_000_000L,
          mkTestOrder(OrderType.BUY, 12_500_000L, 4, OrderPriceMode.AssetDecimals),
          mkTestOrder(OrderType.SELL, 12_600_000L, 4, OrderPriceMode.AssetDecimals),
          "exchange.price = 1_250_000_000 should be >= sellOrder.price = 1_260_000_000 (assetDecimals price = 12_600_000)"
        )
      )

      val case2 = Seq(
        // normalizedPrice = decimalPrice * 10^8 = 1_250_000_000
        (
          3,
          1_250_000_000L,
          mkTestOrder(OrderType.BUY, 1_240_000_000L, 4, OrderPriceMode.FixedDecimals),
          mkTestOrder(OrderType.SELL, 1_250_000_000L, 4, OrderPriceMode.FixedDecimals),
          "exchange.price = 1_250_000_000 should be <= buyOrder.price = 1_240_000_000"
        ),
        (
          3,
          1_250_000_000L,
          mkTestOrder(OrderType.BUY, 1_250_000_000L, 4, OrderPriceMode.FixedDecimals),
          mkTestOrder(OrderType.SELL, 1_260_000_000L, 4, OrderPriceMode.FixedDecimals),
          "exchange.price = 1_250_000_000 should be >= sellOrder.price = 1_260_000_000"
        )
      )

      val mixedCase = Seq(
        (
          3,
          1_250_000_000L,
          mkTestOrder(OrderType.BUY, 12_400_000L, 2, OrderPriceMode.Default),
          mkTestOrder(OrderType.SELL, 1_250_000_000L, 4, OrderPriceMode.FixedDecimals),
          "exchange.price = 1_250_000_000 should be <= buyOrder.price = 1_240_000_000 (assetDecimals price = 12_400_000)"
        ),
        (
          3,
          1_250_000_000L,
          mkTestOrder(OrderType.BUY, 1_250_000_000L, 4, OrderPriceMode.FixedDecimals),
          mkTestOrder(OrderType.SELL, 12_600_000L, 4, OrderPriceMode.AssetDecimals),
          "exchange.price = 1_250_000_000 should be >= sellOrder.price = 1_260_000_000 (assetDecimals price = 12_600_000)"
        )
      )

      Table(
        ("txVersion", "txPrice", "buyOrder", "sellOrder", "expectedError"),
        (case1OldTxs ++ case2 ++ case1NewTxs ++ mixedCase)*
      )
    }

    forAll(cases) { case (txVersion, txPrice, buyOrder, sellOrder, expectedError) =>
      val exchange = TxHelpers.exchangeFromOrders(
        order1 = buyOrder,
        order2 = sellOrder,
        price = txPrice,
        matcher = matcher,
        version = txVersion.toByte,
        fee = TestValues.fee,
        chainId = AddressScheme.current.chainId
      )

      assertDiffEi(baseBlocks, TestBlock.create(Seq(exchange)), fsWithRideV6) { blockDiffEi =>
        blockDiffEi should produce(expectedError)
      }
    }
  }

  property("ExchangeTransaction invalid if order contains more than one proofs") {
    simpleTradePreconditions.foreach { case (genesis, issue1, issue2, exchange) =>
      val newProofs = Proofs(
        Seq(
          crypto.sign(PrivateKey(exchange.sender), exchange.sellOrder.bodyBytes()),
          crypto.sign(PrivateKey(exchange.sellOrder.senderPublicKey), exchange.sellOrder.bodyBytes())
        )
      )

      val exchangeWithResignedOrder = (exchange: @unchecked) match {
        case e1 @ ExchangeTransaction(TxVersion.V1, _, so, _, _, _, _, _, _, _, _) =>
          e1.copy(order1 = so.withProofs(newProofs))
        case e2 @ ExchangeTransaction(TxVersion.V2, _, so, _, _, _, _, _, _, _, _) =>
          e2.copy(order1 = so.withProofs(newProofs))
      }

      val preconBlocks = Seq(
        TestBlock.create(genesis),
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

    val sellerScript = ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1
    val buyerScript  = ScriptCompiler(script, isAssetScript = false, estimator).explicitGet()._1

    val buyer   = TxHelpers.signer(1)
    val seller  = TxHelpers.signer(2)
    val matcher = TxHelpers.signer(3)

    val genesis          = TxHelpers.genesis(matcher.toAddress, Long.MaxValue)
    val tr1              = TxHelpers.transfer(matcher, buyer.toAddress, Long.MaxValue / 3)
    val tr2              = TxHelpers.transfer(matcher, seller.toAddress, Long.MaxValue / 3)
    val issue1           = TxHelpers.issue(buyer, amount = 1000000, decimals = 8, reissuable = false, name = "asset1")
    val issue2           = TxHelpers.issue(seller, amount = 1000000, decimals = 8, reissuable = false, name = "asset2")
    val setMatcherScript = TxHelpers.setScript(matcher, txScriptCompiled)
    val setSellerScript  = TxHelpers.setScript(seller, sellerScript)
    val setBuyerScript   = TxHelpers.setScript(buyer, buyerScript)

    for {
      o1 <- Seq(
        TxHelpers.order(
          OrderType.BUY,
          issue1.asset,
          issue2.asset,
          amount = 1000000,
          price = 1000000,
          fee = enoughFee,
          sender = seller,
          matcher = matcher,
          version = Order.V1
        ),
        TxHelpers.order(
          OrderType.BUY,
          issue1.asset,
          issue2.asset,
          amount = 1000000,
          price = 1000000,
          fee = enoughFee,
          sender = seller,
          matcher = matcher,
          version = Order.V2
        )
      )
      o2 <- Seq(
        TxHelpers.order(
          OrderType.SELL,
          issue1.asset,
          issue2.asset,
          amount = 1000000,
          price = 1000000,
          fee = enoughFee,
          sender = buyer,
          matcher = matcher,
          version = Order.V1
        ),
        TxHelpers.order(
          OrderType.SELL,
          issue1.asset,
          issue2.asset,
          amount = 1000000,
          price = 1000000,
          fee = enoughFee,
          sender = buyer,
          matcher = matcher,
          version = Order.V2
        )
      )
    } yield {
      val exchangeTx = TxHelpers.exchange(
        order1 = o1,
        order2 = o2,
        matcher = matcher,
        amount = 1000000,
        price = 1000000,
        buyMatcherFee = enoughFee,
        sellMatcherFee = enoughFee,
        fee = enoughFee,
        version = TxVersion.V2
      )

      val pretest = Seq(
        TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq(tr1, tr2)),
        TestBlock.create(Seq(issue1, issue2, setMatcherScript, setSellerScript, setBuyerScript))
      )
      val test = TestBlock.create(Seq(exchangeTx))
      if (o1.version == 2 && o2.version == 2) {
        assertDiffEi(pretest, test, fs) { diff =>
          diff.explicitGet()
        }
      } else {
        assertLeft(pretest, test, fs)("Can't process order with signature from scripted account")
      }
    }
  }

  property("Legacy price mode is only allowed in Order V4 after RideV6") {
    val matcher = TxHelpers.secondSigner
    val issue   = TxHelpers.issue()
    val asset   = issue.asset

    def generateTx(version: TxVersion, mode: OrderPriceMode): ExchangeTransaction = {
      val pair = AssetPair(asset, Waves)

      val buyOrder =
        Order
          .buy(
            version,
            TxHelpers.secondSigner,
            matcher.publicKey,
            pair,
            1L,
            1_0000_0000L,
            ntpTime.correctedTime(),
            ntpTime.getTimestamp() + 1000,
            TestValues.fee,
            priceMode = mode
          )
          .explicitGet()
      val sellOrder =
        Order
          .sell(
            version,
            TxHelpers.defaultSigner,
            matcher.publicKey,
            pair,
            1L,
            1L,
            ntpTime.correctedTime(),
            ntpTime.getTimestamp() + 1000,
            TestValues.fee,
            priceMode = mode
          )
          .explicitGet()

      ExchangeTransaction
        .signed(
          TxVersion.V3,
          matcher.privateKey,
          buyOrder,
          sellOrder,
          1L,
          1L,
          TestValues.fee,
          TestValues.fee,
          TestValues.fee,
          ntpTime.correctedTime()
        )
        .explicitGet()
    }

    def generateAndAppendTx(orderVersion: TxVersion, mode: OrderPriceMode, settings: WavesSettings = DomainPresets.RideV5): Unit = {
      withDomain(settings) { d =>
        d.helpers.creditWavesToDefaultSigner()
        d.helpers.creditWavesFromDefaultSigner(TxHelpers.secondAddress)
        d.appendAndAssertSucceed(issue)
        d.appendAndAssertSucceed(generateTx(orderVersion, mode))
      }
    }

    intercept[RuntimeException](generateAndAppendTx(Order.V1, FixedDecimals)).getMessage should include("price mode should be default")
    intercept[RuntimeException](generateAndAppendTx(Order.V2, FixedDecimals)).getMessage should include("price mode should be default")
    intercept[RuntimeException](generateAndAppendTx(Order.V3, FixedDecimals)).getMessage should include("price mode should be default")
    intercept[RuntimeException](generateAndAppendTx(Order.V4, AssetDecimals)).getMessage should include(
      "Legacy price mode is only available after RideV6 activation"
    )
    generateAndAppendTx(Order.V4, FixedDecimals, DomainPresets.RideV6)
    generateAndAppendTx(Order.V4, AssetDecimals, DomainPresets.RideV6)
  }

  property("ExchangeTransaction with Orders V4 uses asset decimals for price calculation") {
    val enoughFee = 100000000L
    val buyer     = TxHelpers.signer(1)
    val seller    = TxHelpers.signer(2)
    val matcher   = TxHelpers.signer(3)

    val (preconditions, usdn, tidex, liquid) = {

      val genesisTxs = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val usdnTx =
        TxHelpers.issue(buyer, amount = ENOUGH_AMT, decimals = 6, reissuable = false, name = "USD-N", description = "USD-N", version = TxVersion.V3)
      val tidexTx =
        TxHelpers.issue(seller, amount = ENOUGH_AMT, decimals = 2, reissuable = false, name = "Tidex", description = "Tidex", version = TxVersion.V3)
      val liquidTx = TxHelpers.issue(
        seller,
        amount = ENOUGH_AMT,
        decimals = 8,
        reissuable = false,
        name = "Liquid",
        description = "Liquid",
        version = TxVersion.V3
      )

      val usdn   = IssuedAsset(usdnTx.assetId)
      val tidex  = IssuedAsset(tidexTx.assetId)
      val liquid = IssuedAsset(liquidTx.assetId)

      (Seq(TestBlock.create(genesisTxs), TestBlock.create(Seq(usdnTx, tidexTx, liquidTx), Block.ProtoBlockVersion)), usdn, tidex, liquid)
    }

    def mkExchange(
        txv: Byte,
        bov: Byte,
        sov: Byte,
        amount: Long,
        txPrice: Long,
        boPrice: Long,
        boMode: OrderPriceMode,
        soPrice: Long,
        soMode: OrderPriceMode,
        pair: AssetPair
    ): ExchangeTransaction = {
      val buyOrder = TxHelpers.order(
        OrderType.BUY,
        pair.amountAsset,
        pair.priceAsset,
        amount = amount,
        price = boPrice,
        priceMode = boMode,
        sender = buyer,
        matcher = matcher,
        fee = enoughFee,
        version = bov
      )

      val sellOrder = TxHelpers.order(
        OrderType.SELL,
        pair.amountAsset,
        pair.priceAsset,
        amount = amount,
        price = soPrice,
        priceMode = soMode,
        sender = seller,
        matcher = matcher,
        fee = enoughFee,
        version = sov
      )

      TxHelpers.exchange(buyOrder, sellOrder, matcher, amount, txPrice, enoughFee, enoughFee, enoughFee, version = txv)
    }

    val wavesUsdn   = AssetPair(Waves, usdn)
    val tidexWaves  = AssetPair(tidex, Waves)
    val liquidWaves = AssetPair(liquid, Waves)

    val scenarios = Table(
      (
        "transaction with orders v3",
        "transaction with orders v4 (v3 mode)",
        "transaction with orders v4",
        "transaction with orders v3 and v4 (v3 mode)",
        "transaction with orders v3 and v4",
        "transaction with orders v4 (v3 mode) and v3",
        "transaction with orders v4 and v3"
      ),
      (
        mkExchange(TxVersion.V2, Order.V3, Order.V3, 55768188998L, 592600L, 592600L, DefaultPriceMode, 592600L, DefaultPriceMode, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 55768188998L, 59260000L, 592600L, AssetDecimals, 592600L, AssetDecimals, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 55768188998L, 59260000L, 59260000L, DefaultPriceMode, 59260000L, DefaultPriceMode, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 55768188998L, 59260000L, 592600L, DefaultPriceMode, 592600L, AssetDecimals, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 55768188998L, 59260000L, 592600L, DefaultPriceMode, 59260000L, FixedDecimals, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 55768188998L, 59260000L, 592600L, AssetDecimals, 592600L, DefaultPriceMode, wavesUsdn),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 55768188998L, 59260000L, 59260000L, FixedDecimals, 592600L, DefaultPriceMode, wavesUsdn)
      ),
      (
        mkExchange(
          TxVersion.V2,
          Order.V3,
          Order.V3,
          213L,
          35016774000000L,
          35016774000000L,
          DefaultPriceMode,
          35016774000000L,
          DefaultPriceMode,
          tidexWaves
        ),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 213L, 35016774L, 35016774000000L, AssetDecimals, 35016774000000L, AssetDecimals, tidexWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 213L, 35016774L, 35016774, FixedDecimals, 35016774L, FixedDecimals, tidexWaves),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 213L, 35016774L, 35016774000000L, DefaultPriceMode, 35016774000000L, AssetDecimals, tidexWaves),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 213L, 35016774L, 35016774000000L, DefaultPriceMode, 35016774L, FixedDecimals, tidexWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 213L, 35016774L, 35016774000000L, AssetDecimals, 35016774000000L, DefaultPriceMode, tidexWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 213L, 35016774L, 35016774L, FixedDecimals, 35016774000000L, DefaultPriceMode, tidexWaves)
      ),
      (
        mkExchange(TxVersion.V2, Order.V3, Order.V3, 2000000000L, 13898832L, 13898832L, DefaultPriceMode, 13898832L, DefaultPriceMode, liquidWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 2000000000L, 13898832L, 13898832L, AssetDecimals, 13898832L, AssetDecimals, liquidWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V4, 2000000000L, 13898832L, 13898832L, FixedDecimals, 13898832L, FixedDecimals, liquidWaves),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 2000000000L, 13898832L, 13898832L, DefaultPriceMode, 13898832L, AssetDecimals, liquidWaves),
        mkExchange(TxVersion.V3, Order.V3, Order.V4, 2000000000L, 13898832L, 13898832L, DefaultPriceMode, 13898832L, FixedDecimals, liquidWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 2000000000L, 13898832L, 13898832L, AssetDecimals, 13898832L, DefaultPriceMode, liquidWaves),
        mkExchange(TxVersion.V3, Order.V4, Order.V3, 2000000000L, 13898832L, 13898832L, FixedDecimals, 13898832L, DefaultPriceMode, liquidWaves)
      )
    )

    forAll(scenarios) { case (txWithV3, txWithV4AsV3, txWithV4, txWithV3V4AsV3, txWithV3V4, txWithV4AsV3V3, txWithV4V3) =>
      val portfolios = collection.mutable.ListBuffer[Map[Address, Portfolio]]()

      Seq(txWithV3, txWithV4AsV3, txWithV4, txWithV3V4AsV3, txWithV3V4, txWithV4AsV3V3, txWithV4V3)
        .foreach { tx =>
          assertDiffAndState(
            preconditions,
            TestBlock.create(Seq(tx), Block.ProtoBlockVersion),
            DomainPresets.RideV6.blockchainSettings.functionalitySettings
          ) { case (blockDiff, _) =>
            portfolios += blockDiff.portfolios
          }
        }

      // all portfolios built on the state and on the composite blockchain are equal
      portfolios.distinct.size shouldBe 1
    }
  }

  property("ExchangeTransaction V3 can have SELL order as order1 after BlockV5 activation") {
    val scenario = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val genesis = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val itx1    = TxHelpers.issue(matcher, ENOUGH_AMT, decimals = 8, name = "asset1")
      val itx2    = TxHelpers.issue(matcher, ENOUGH_AMT, decimals = 8, name = "asset2")
      val ttx1    = TxHelpers.transfer(matcher, seller.toAddress, ENOUGH_AMT / 2, itx1.asset, version = TxVersion.V3)
      val ttx2    = TxHelpers.transfer(matcher, buyer.toAddress, ENOUGH_AMT / 2, itx1.asset, version = TxVersion.V3)
      val ttx3    = TxHelpers.transfer(matcher, seller.toAddress, ENOUGH_AMT / 2, itx2.asset, version = TxVersion.V3)
      val ttx4    = TxHelpers.transfer(matcher, buyer.toAddress, ENOUGH_AMT / 2, itx2.asset, version = TxVersion.V3)

      val assets = Seq(itx1.asset, itx2.asset)

      for {
        amountAsset <- assets
        priceAsset  <- assets if priceAsset != amountAsset
        tx <- Seq(
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(OrderType.BUY, amountAsset, priceAsset, sender = buyer, matcher = matcher, version = Order.V1),
            TxHelpers.order(OrderType.SELL, amountAsset, priceAsset, sender = seller, matcher = matcher, version = Order.V1),
            matcher,
            version = TxVersion.V1
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(OrderType.BUY, amountAsset, priceAsset, sender = buyer, matcher = matcher, version = Order.V1),
            TxHelpers.order(OrderType.SELL, amountAsset, priceAsset, sender = seller, matcher = matcher, version = Order.V1),
            matcher,
            version = TxVersion.V2
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(OrderType.BUY, amountAsset, priceAsset, sender = buyer, matcher = matcher, version = Order.V2),
            TxHelpers.order(OrderType.SELL, amountAsset, priceAsset, sender = seller, matcher = matcher, version = Order.V2),
            matcher,
            version = TxVersion.V2
          ),
          TxHelpers.exchangeFromOrders(
            TxHelpers.order(OrderType.BUY, amountAsset, priceAsset, sender = buyer, matcher = matcher, version = Order.V3),
            TxHelpers.order(OrderType.SELL, amountAsset, priceAsset, sender = seller, matcher = matcher, version = Order.V3),
            matcher,
            version = TxVersion.V2
          )
        )
      } yield {
        val fee = 100000000L
        val fixed = tx
          .copy(
            version = TxVersion.V3,
            buyMatcherFee = fee,
            sellMatcherFee = fee,
            fee = TxPositiveAmount.unsafeFrom(fee),
            order1 = tx.order1.copy(version = Order.V4, matcherFee = TxMatcherFee.unsafeFrom(fee)).signWith(buyer.privateKey),
            order2 = tx.order2.copy(version = Order.V4, matcherFee = TxMatcherFee.unsafeFrom(fee)).signWith(seller.privateKey)
          )
          .signWith(matcher.privateKey)
        val reversed = fixed
          .copy(
            order1 = fixed.order2,
            order2 = fixed.order1
          )
          .signWith(matcher.privateKey)

        (Seq(TestBlock.create(genesis), TestBlock.create(Seq(itx1, itx2, ttx1, ttx2, ttx3, ttx4))), fixed, reversed)
      }
    }

    scenario.foreach { case (preconditions, fixed, reversed) =>
      val portfolios = collection.mutable.ListBuffer[Map[Address, Portfolio]]()

      assertDiffAndState(preconditions, TestBlock.create(Seq(fixed)), fsWithBlockV5) { case (diff, _) =>
        portfolios += diff.portfolios
      }

      assertDiffAndState(preconditions, TestBlock.create(Seq(reversed)), fsWithBlockV5) { case (diff, _) =>
        portfolios += diff.portfolios
      }

      portfolios.tail.forall(_ == portfolios.head) shouldBe true
    }
  }

  property(s"Accepts failed transactions after ${BlockchainFeatures.BlockV5} activation") {
    val scenario = {
      val buyer   = TxHelpers.signer(1)
      val seller  = TxHelpers.signer(2)
      val matcher = TxHelpers.signer(3)

      val fee      = 100000000L
      val quantity = 3 * 100000L * 100000000L

      val genesis        = Seq(buyer, seller, matcher).map(acc => TxHelpers.genesis(acc.toAddress))
      val throwingScript = ExprScript(FUNCTION_CALL(Native(THROW), Nil)).explicitGet()
      val issue          = TxHelpers.issue(seller, quantity, decimals = 8, reissuable = false, script = Some(throwingScript), fee = fee)
      Seq(
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, issue.asset, Waves, sender = buyer, matcher = matcher, fee = fee, version = Order.V1),
          TxHelpers.order(OrderType.SELL, issue.asset, Waves, sender = seller, matcher = matcher, fee = fee, version = Order.V1),
          matcher,
          version = TxVersion.V1
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, issue.asset, Waves, sender = buyer, matcher = matcher, fee = fee, version = Order.V1),
          TxHelpers.order(OrderType.SELL, issue.asset, Waves, sender = seller, matcher = matcher, fee = fee, version = Order.V1),
          matcher,
          version = TxVersion.V2
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, issue.asset, Waves, sender = buyer, matcher = matcher, fee = fee, version = Order.V2),
          TxHelpers.order(OrderType.SELL, issue.asset, Waves, sender = seller, matcher = matcher, fee = fee, version = Order.V2),
          matcher,
          version = TxVersion.V2
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, issue.asset, Waves, sender = buyer, matcher = matcher, fee = fee, version = Order.V3),
          TxHelpers.order(OrderType.SELL, issue.asset, Waves, sender = seller, matcher = matcher, fee = fee, version = Order.V3),
          matcher,
          version = TxVersion.V2
        )
      ).map { tx =>
        val sellAmount = quantity
        val buyAmount  = quantity
        val amount     = Math.min(sellAmount, buyAmount) / 1000
        val exchange = tx
          .copy(
            amount = TxExchangeAmount.unsafeFrom(amount),
            order1 = tx.buyOrder.copy(amount = TxExchangeAmount.unsafeFrom(sellAmount)).signWith(buyer.privateKey),
            order2 = tx.sellOrder.copy(amount = TxExchangeAmount.unsafeFrom(buyAmount)).signWith(seller.privateKey),
            buyMatcherFee = (BigInt(tx.fee.value) * amount / buyAmount).toLong,
            sellMatcherFee = (BigInt(tx.fee.value) * amount / sellAmount).toLong
          )
          .signWith(matcher.privateKey)

        val buyerBalance   = Map(Waves -> ENOUGH_AMT, issue.asset -> 0L)
        val sellerBalance  = Map(Waves -> (ENOUGH_AMT - fee), issue.asset -> issue.quantity.value)
        val matcherBalance = Map(Waves -> ENOUGH_AMT, issue.asset -> 0L)

        (exchange, (buyerBalance, sellerBalance, matcherBalance), genesis :+ issue, throwingScript)
      }
    }

    scenario.foreach { case (exchange, (buyerBalance, sellerBalance, matcherBalance), genesisTxs, throwingScript) =>
      assertDiffEi(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(exchange), Block.ProtoBlockVersion), fsWithOrderFeature) { ei =>
        ei.left.value
      }
      assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(exchange), Block.ProtoBlockVersion), fsWithBlockV5) {
        case (diff, state) =>
          diff.scriptsComplexity shouldBe 1 // throw()
          diff.portfolios(exchange.sender.toAddress).balance shouldBe -exchange.fee.value
          diff.portfolios.get(exchange.buyOrder.sender.toAddress) shouldBe None
          diff.portfolios.get(exchange.sellOrder.sender.toAddress) shouldBe None

          diff.scriptsComplexity shouldBe DiffsCommon
            .countVerifierComplexity(Some(throwingScript), state, isAsset = true)
            .explicitGet()
            .get
            ._2

          buyerBalance.foreach { case (asset, balance) =>
            state.balance(exchange.buyOrder.sender.toAddress, asset) shouldBe balance
          }
          sellerBalance.foreach { case (asset, balance) =>
            state.balance(exchange.sellOrder.sender.toAddress, asset) shouldBe balance
          }

          state.balance(exchange.sender.toAddress, Waves) shouldBe matcherBalance(Waves) - exchange.fee.value
          matcherBalance.collect { case b @ (IssuedAsset(_), _) => b }.foreach { case (asset, balance) =>
            diff.portfolios(exchange.sender.toAddress).balanceOf(asset) shouldBe 0L
            state.balance(exchange.sender.toAddress, asset) shouldBe balance
          }

          state.transactionInfo(exchange.id()).map(r => r._2 -> (r._1.status == Status.Succeeded)) shouldBe Some((exchange, false))
      }
    }
  }

  property("Counts complexity correctly for exchanges failed by assets") {
    def test(
        priceAssetIssue: IssueTransaction,
        amountAssetIssue: IssueTransaction,
        order1FeeAssetIssue: IssueTransaction,
        order2FeeAssetIssue: IssueTransaction,
        complexity: Long
    ): Unit = {
      val order1 = TxHelpers.order(
        OrderType.BUY,
        IssuedAsset(amountAssetIssue.assetId),
        IssuedAsset(priceAssetIssue.assetId),
        IssuedAsset(order1FeeAssetIssue.assetId)
      )
      val order2 = TxHelpers.order(
        OrderType.SELL,
        IssuedAsset(amountAssetIssue.assetId),
        IssuedAsset(priceAssetIssue.assetId),
        IssuedAsset(order2FeeAssetIssue.assetId)
      )
      val fee      = FeeConstants(TransactionType.Exchange) * FeeUnit + 2 * ScriptExtraFee
      val exchange = TxHelpers.exchangeFromOrders(order1, order2, fee = fee)

      withDomain(RideV4) { d =>
        d.appendBlock(Seq(amountAssetIssue, priceAssetIssue, order1FeeAssetIssue, order2FeeAssetIssue).distinct*)
        d.appendAndAssertFailed(exchange)
        d.liquidDiff.scriptsComplexity shouldBe complexity
      }
    }

    withClue("price asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = Some(TestValues.rejectAssetScript))
      val amountAssetIssue    = TxHelpers.issue(script = Some(TestValues.assetScript))
      val order1FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))
      val order2FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))

      test(priceAssetIssue, amountAssetIssue, order1FeeAssetIssue, order2FeeAssetIssue, TestValues.rejectAssetScriptComplexity)
    }

    withClue("amount asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = Some(TestValues.assetScript))
      val amountAssetIssue    = TxHelpers.issue(script = Some(TestValues.rejectAssetScript))
      val order1FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))
      val order2FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))

      test(
        priceAssetIssue,
        amountAssetIssue,
        order1FeeAssetIssue,
        order2FeeAssetIssue,
        TestValues.assetScriptComplexity + TestValues.rejectAssetScriptComplexity
      )
    }

    withClue("order1 matcher fee asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = Some(TestValues.assetScript))
      val amountAssetIssue    = TxHelpers.issue(script = Some(TestValues.assetScript))
      val order1FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.rejectAssetScript))
      val order2FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))

      test(
        priceAssetIssue,
        amountAssetIssue,
        order1FeeAssetIssue,
        order2FeeAssetIssue,
        2 * TestValues.assetScriptComplexity
      )
    }

    withClue("order2 matcher fee asset fails") {
      val priceAssetIssue     = TxHelpers.issue(script = Some(TestValues.assetScript))
      val amountAssetIssue    = TxHelpers.issue(script = Some(TestValues.assetScript))
      val order1FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))
      val order2FeeAssetIssue = TxHelpers.issue(script = Some(TestValues.rejectAssetScript))

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
      val exchange = TxHelpers.exchangeFromOrders(order1, order2)

      withDomain(
        domainSettingsWithFS(
          TestFunctionalitySettings.withFeatures(BlockchainFeatures.SmartAssets, BlockchainFeatures.SmartAccountTrading, BlockchainFeatures.OrderV3)
        )
      ) { d =>
        d.appendBlock(Seq(tradeableAssetIssue, feeAssetIssue).distinct*)
        val newBlock = d.createBlock(2.toByte, Seq(exchange))
        val diff = BlockDiffer
          .fromBlock(d.blockchainUpdater, Some(d.lastBlock), newBlock, None, MiningConstraint.Unlimited, newBlock.header.generationSignature)
          .explicitGet()
        diff.snapshot.scriptsComplexity shouldBe complexity

        val feeUnits = FeeValidation.getMinFee(d.blockchainUpdater, exchange).explicitGet().minFeeInWaves / FeeValidation.FeeUnit
        if (complexity > 0) feeUnits shouldBe 7
        else feeUnits shouldBe 3
      }
    }

    withClue("fee") {
      val tradeableAssetIssue = TxHelpers.issue()
      val feeAssetIssue       = TxHelpers.issue(script = Some(TestValues.assetScript))
      test(tradeableAssetIssue, feeAssetIssue, 0)
    }

    withClue("asset") {
      val tradeableAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))
      val feeAssetIssue       = TxHelpers.issue()
      test(tradeableAssetIssue, feeAssetIssue, 1)
    }

    withClue("fee and asset") {
      val tradeableAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))
      val feeAssetIssue       = TxHelpers.issue(script = Some(TestValues.assetScript))
      test(tradeableAssetIssue, feeAssetIssue, 1)
    }

    withClue("fee and asset (same asset)") {
      val tradeableAssetIssue = TxHelpers.issue(script = Some(TestValues.assetScript))
      test(tradeableAssetIssue, tradeableAssetIssue, 1)
    }
  }

  property("buyMatcherFee/sellMatcherFee validation") {
    val issueFee            = 1.waves
    val exchangeFee         = 0.003.waves
    val matcherStartBalance = issueFee * 2 + exchangeFee
    val buyMatcherFee       = -1
    val sellMatcherFee      = Long.MinValue - buyMatcherFee + exchangeFee

    val sender      = testWallet.generateNewAccount().get
    def mkIssueTx   = IssueTransaction.selfSigned(2.toByte, sender, "IA_01", "", 100, 2, true, None, issueFee, ntpTime.getTimestamp()).explicitGet()
    val priceAsset  = mkIssueTx
    val amountAsset = mkIssueTx
    val assetPair   = AssetPair(priceAsset.asset, amountAsset.asset)

    def mkOrder(orderType: OrderType): Order =
      Order
        .selfSigned(
          3.toByte,
          sender,
          sender.publicKey,
          assetPair,
          orderType,
          1,
          1,
          ntpTime.getTimestamp(),
          ntpTime.getTimestamp() + 100000,
          100
        )
        .explicitGet()

    def mkExchangeTx: ExchangeTransaction =
      ExchangeTransaction
        .signed(
          2.toByte,
          sender.privateKey,
          mkOrder(OrderType.BUY),
          mkOrder(OrderType.SELL),
          1,
          1,
          buyMatcherFee,
          sellMatcherFee,
          exchangeFee,
          ntpTime.getTimestamp()
        )
        .explicitGet()

    withDomain(DomainPresets.RideV5) { d =>
      d.appendBlock(
        GenesisTransaction.create(sender.toAddress, matcherStartBalance, ntpTime.getTimestamp()).explicitGet(),
        priceAsset,
        amountAsset
      )

      d.balance(sender.toAddress) shouldBe exchangeFee
      d.appendBlockE(mkExchangeTx) should produce("Matcher fee can not be negative")
    }
  }

  property(
    s"orders' ETH public keys with leading zeros and shortened byte representation are allowed only after ${BlockchainFeatures.ConsensusImprovements} activation"
  ) {
    val signer = Bip32ECKeyPair.generateKeyPair("i1".getBytes)
    signer.getPublicKey.toByteArray.length shouldBe <(EthereumKeyLength)

    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(BlockchainFeatures.ConsensusImprovements -> 4),
      Seq(AddrWithBalance(signer.toWavesAddress))
    ) { d =>
      val issue = TxHelpers.issue(TxHelpers.defaultSigner)
      val buyOrder = Order(
        4.toByte,
        OrderAuthentication.Eip712Signature(ByteStr(new Array[Byte](64))),
        defaultSigner.publicKey,
        AssetPair(issue.asset, Waves),
        OrderType.BUY,
        TxExchangeAmount.unsafeFrom(1),
        TxOrderPrice.unsafeFrom(1),
        System.currentTimeMillis(),
        System.currentTimeMillis() + 10000,
        TxMatcherFee.unsafeFrom(0.003.waves)
      )
      val signedBuyOrder = buyOrder.copy(
        orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(EthOrders.signOrder(buyOrder, signer)))
      )
      val sellOrder = TxHelpers.order(OrderType.SELL, issue.asset, Waves, version = Order.V4)

      val tx = TxHelpers.exchange(signedBuyOrder, sellOrder, version = TxVersion.V3)

      d.appendBlock(issue)

      d.appendAndCatchError(tx) shouldBe TransactionDiffer.TransactionValidationError(
        GenericError("Invalid public key for Ethereum orders"),
        tx
      )
      d.appendBlock()
      d.appendAndAssertSucceed(tx)
    }
  }

  property(s"orders' native public keys with leading zeros are allowed before and after ${BlockchainFeatures.ConsensusImprovements} activation") {
    val signer = KeyPair("h0".getBytes)
    signer.publicKey.arr.head shouldBe 0.toByte

    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(BlockchainFeatures.ConsensusImprovements -> 4),
      AddrWithBalance.enoughBalances(signer)
    ) { d =>
      val issue     = TxHelpers.issue(TxHelpers.defaultSigner)
      val buyOrder  = () => TxHelpers.order(OrderType.BUY, issue.asset, Waves, sender = signer)
      val sellOrder = () => TxHelpers.order(OrderType.SELL, issue.asset, Waves)

      val tx = () => TxHelpers.exchange(buyOrder(), sellOrder())

      d.appendBlock(issue)
      d.appendAndAssertSucceed(tx())

      d.blockchain.isFeatureActivated(BlockchainFeatures.ConsensusImprovements) shouldBe false
      d.appendBlock()
      d.blockchain.isFeatureActivated(BlockchainFeatures.ConsensusImprovements) shouldBe true

      d.appendAndAssertSucceed(tx())
    }
  }

  property(
    s"correctly recover signer public key when v < 27 in orders' signature data only after ${BlockchainFeatures.ConsensusImprovements} activation"
  ) {
    val signer =
      Bip32ECKeyPair.create(EthEncoding.toBytes("0x01db4a036ea48572bf27630c72a1513f48f0b4a6316606fd01c23318befdf984"), Array.emptyByteArray)

    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(BlockchainFeatures.ConsensusImprovements -> 4),
      Seq(AddrWithBalance(signer.toWavesAddress))
    ) { d =>
      val issue = TxHelpers.issue(TxHelpers.defaultSigner)
      val buyOrder = Order(
        4.toByte,
        OrderAuthentication.Eip712Signature(ByteStr(new Array[Byte](64))),
        defaultSigner.publicKey,
        AssetPair(issue.asset, Waves),
        OrderType.BUY,
        TxExchangeAmount.unsafeFrom(1),
        TxOrderPrice.unsafeFrom(1),
        System.currentTimeMillis(),
        System.currentTimeMillis() + 10000,
        TxMatcherFee.unsafeFrom(0.003.waves)
      )
      val signature = EthOrders.signOrder(buyOrder, signer)
      val v         = signature.last
      val signedBuyOrder = buyOrder.copy(
        orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(signature.init :+ (v - 27).toByte))
      )
      val sellOrder = TxHelpers.order(OrderType.SELL, issue.asset, Waves, version = Order.V4)

      val tx = TxHelpers.exchange(signedBuyOrder, sellOrder, version = TxVersion.V3)

      d.appendBlock(issue)

      d.appendAndCatchError(tx) shouldBe TransactionDiffer.TransactionValidationError(
        GenericError("Invalid order signature format"),
        tx
      )
      d.appendBlock()
      d.appendAndAssertSucceed(tx)
    }
  }

  property(s"NODE-970. Non-empty attachment field is allowed only after ${BlockchainFeatures.TransactionStateSnapshot.description} activation") {
    val matcher = TxHelpers.defaultSigner
    val issuer  = TxHelpers.secondSigner

    withDomain(
      ConsensusImprovements.setFeaturesHeight(BlockchainFeatures.TransactionStateSnapshot -> 4),
      AddrWithBalance.enoughBalances(matcher, issuer)
    ) { d =>
      val issue = TxHelpers.issue(issuer)
      val exchange = () =>
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, Waves, issue.asset, version = Order.V4, attachment = Some(ByteStr.fill(1)(1))),
          TxHelpers.order(OrderType.SELL, Waves, issue.asset, version = Order.V4, sender = TxHelpers.secondSigner),
          version = TxVersion.V3
        )

      d.appendBlock(issue)
      d.appendBlockE(exchange()) should produce("Attachment field for orders is not supported yet")
      d.appendBlock()
      d.appendBlockENoCheck(exchange()) should beRight
    }
  }

  def script(caseType: String, v: Boolean, complex: Boolean = false): Seq[String] = Seq(true, false).map { full =>
    val expr =
      s"""
         |  strict c = ${if (complex) (1 to 16).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"}
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
      buyerScriptSrc: String,
      sellerScriptSrc: String,
      txScript: String
  ): (GenesisTransaction, List[TransferTransaction], List[Transaction], ExchangeTransaction, KeyPair) = {
    val enoughFee = 100000000

    val txScriptCompiled = ScriptCompiler(txScript, isAssetScript = false, estimator).explicitGet()._1
    val sellerScript     = ScriptCompiler(sellerScriptSrc, isAssetScript = false, estimator).explicitGet()._1
    val buyerScript      = ScriptCompiler(buyerScriptSrc, isAssetScript = false, estimator).explicitGet()._1

    val buyer   = TxHelpers.signer(1)
    val seller  = TxHelpers.signer(2)
    val matcher = TxHelpers.signer(3)

    val genesis          = TxHelpers.genesis(matcher.toAddress, Long.MaxValue)
    val tr1              = TxHelpers.transfer(matcher, buyer.toAddress, Long.MaxValue / 3)
    val tr2              = TxHelpers.transfer(matcher, seller.toAddress, Long.MaxValue / 3)
    val issue1           = TxHelpers.issue(buyer, amount = 1000000, decimals = 8, reissuable = false, name = "asset1")
    val issue2           = TxHelpers.issue(seller, amount = 1000000, decimals = 8, reissuable = false, name = "asset2")
    val setMatcherScript = TxHelpers.setScript(matcher, txScriptCompiled)
    val setSellerScript  = TxHelpers.setScript(seller, sellerScript)
    val setBuyerScript   = TxHelpers.setScript(buyer, buyerScript)
    val o1 = TxHelpers.order(
      OrderType.BUY,
      issue1.asset,
      issue2.asset,
      amount = 1000000,
      price = 1000000,
      fee = enoughFee,
      sender = seller,
      matcher = matcher,
      version = Order.V2
    )
    val o2 = TxHelpers.order(
      OrderType.SELL,
      issue1.asset,
      issue2.asset,
      amount = 1000000,
      price = 1000000,
      fee = enoughFee,
      sender = buyer,
      matcher = matcher,
      version = Order.V2
    )
    val exchangeTx = TxHelpers.exchange(
      order1 = o1,
      order2 = o2,
      matcher = matcher,
      amount = 1000000,
      price = 1000000,
      buyMatcherFee = enoughFee,
      sellMatcherFee = enoughFee,
      fee = enoughFee,
      version = TxVersion.V2
    )

    (genesis, List(tr1, tr2), List(issue1, issue2, setMatcherScript, setSellerScript, setBuyerScript), exchangeTx, matcher)
  }

  def simpleTradePreconditions: Seq[(Seq[GenesisTransaction], IssueTransaction, IssueTransaction, ExchangeTransaction)] = {
    val buyer   = TxHelpers.signer(1)
    val seller  = TxHelpers.signer(2)
    val matcher = TxHelpers.signer(3)

    val genesis = Seq(buyer, seller).map(acc => TxHelpers.genesis(acc.toAddress))
    val issue1  = TxHelpers.issue(seller, ENOUGH_AMT, name = "asset1")
    val issue2  = TxHelpers.issue(buyer, ENOUGH_AMT, name = "asset2")

    for {
      maybeAsset1 <- Seq(Some(issue1.id()), None).map(Asset.fromCompatId)
      maybeAsset2 <- Seq(Some(issue2.id()), None).map(Asset.fromCompatId) if maybeAsset1.compatId != maybeAsset2.compatId
      exchange <- Seq(
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, maybeAsset1, maybeAsset2, sender = buyer, matcher = matcher, version = Order.V1),
          TxHelpers.order(OrderType.SELL, maybeAsset1, maybeAsset2, sender = seller, matcher = matcher, version = Order.V1),
          matcher,
          version = TxVersion.V2
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, maybeAsset1, maybeAsset2, sender = buyer, matcher = matcher, version = Order.V2),
          TxHelpers.order(OrderType.SELL, maybeAsset1, maybeAsset2, sender = seller, matcher = matcher, version = Order.V2),
          matcher,
          version = TxVersion.V2
        ),
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, maybeAsset1, maybeAsset2, sender = buyer, matcher = matcher, version = Order.V3),
          TxHelpers.order(OrderType.SELL, maybeAsset1, maybeAsset2, sender = seller, matcher = matcher, version = Order.V3),
          matcher,
          version = TxVersion.V2
        )
      )
    } yield (genesis, issue1, issue2, exchange)
  }

  /** Checks whether generated ExchangeTransactionV2 is valid. In case of using orders of version 3 it is possible that matched amount of received
    * asset is less than matcher's fee in that asset. It leads to negative asset balance error
    */
  def transactionWithOrdersV3IsValid(ex: ExchangeTransaction): Boolean = {
    (ex.buyOrder, ex.sellOrder) match {
      case (_: Order, _: Order) =>
        val isBuyerReceiveAmountGreaterThanFee =
          if (ex.buyOrder.assetPair.amountAsset == ex.buyOrder.matcherFeeAssetId) {
            ExchangeTransactionDiff.getReceiveAmount(ex.buyOrder, 8, 8, ex.amount.value, ex.price.value).explicitGet() > ex.buyMatcherFee
          } else true

        val isSellerReceiveAmountGreaterThanFee =
          if (ex.sellOrder.assetPair.amountAsset == ex.sellOrder.matcherFeeAssetId) {
            ExchangeTransactionDiff.getReceiveAmount(ex.sellOrder, 8, 8, ex.amount.value, ex.price.value).explicitGet() > ex.sellMatcherFee
          } else true

        isBuyerReceiveAmountGreaterThanFee && isSellerReceiveAmountGreaterThanFee
      case _ => true
    }
  }

  /** Generates sequence of Longs with predefined sum and size */
  def getSeqWithPredefinedSum(sum: Long, count: Int): Seq[Long] = {

    val (lastRemainder, values) = (1 until count)
      .foldLeft((sum, List.empty[Long])) { case ((remainder, result), index) =>
        val next = java.util.concurrent.ThreadLocalRandom.current.nextLong(1, remainder / (count - index))
        (remainder - next) -> (next :: result)
      }

    Random.shuffle(lastRemainder :: values)
  }

  /** Generates sequence of sell orders for one big buy order */
  def sellOrdersForBigBuyOrder(
      matcher: KeyPair,
      sellers: Seq[KeyPair],
      assetPair: AssetPair,
      price: Long,
      matcherFeeAssetId: Asset,
      totalAmount: Long,
      totalMatcherFee: Long
  ): Seq[Order] = {
    val randomAmountsAndFees =
      getSeqWithPredefinedSum(totalAmount, sellers.length) zip getSeqWithPredefinedSum(totalMatcherFee, sellers.length)

    val sellers2AmountsAndFees = sellers zip randomAmountsAndFees

    sellers2AmountsAndFees.map { case (seller, (amount, fee)) =>
      TxHelpers.order(
        orderType = OrderType.SELL,
        amountAsset = assetPair.amountAsset,
        priceAsset = assetPair.priceAsset,
        feeAsset = matcherFeeAssetId,
        amount = amount,
        price = price,
        fee = fee,
        sender = seller,
        matcher = matcher
      )
    }
  }

  /** Returns preconditions for tests based on case when there is one big buy order and few small sell orders
    *
    * @param totalBuyMatcherFeeBoundaries
    *   function for manipulating of total matcher's fee paid by buyer in exchange transactions
    * @param sellersTotalAmount
    *   function for manipulating of total sell orders amount
    */
  def oneBuyFewSellsPreconditions(
      totalBuyMatcherFeeBoundaries: Long => (Long, Long),
      sellersTotalAmount: Long => Long
  ): (Seq[GenesisTransaction], IssueTransaction, IssueTransaction, MassTransferTransaction, Seq[ExchangeTransaction], Order) = {
    val matcher               = TxHelpers.signer(1)
    val sellOrdersCount       = 5
    val sellers               = (1 to 5).map(idx => TxHelpers.signer(idx + 1))
    val buyer                 = TxHelpers.signer(sellOrdersCount + 2)
    val bigBuyOrderAmount     = 3 * 100000L * 100000000L
    val price                 = 3 * 100000L
    val bigBuyOrderMatcherFee = 100000L

    val issue1 = TxHelpers.issue(buyer, Long.MaxValue - 1000L, name = "asset1")
    val issue2 = TxHelpers.issue(buyer, Long.MaxValue - 1000L, name = "asset2")

    val totalBuyMatcherFeeForExchangeTransactions = totalBuyMatcherFeeBoundaries(bigBuyOrderMatcherFee)._2

    val bigBuyOrder = TxHelpers.order(
      orderType = OrderType.BUY,
      amountAsset = issue2.asset,
      priceAsset = issue1.asset,
      feeAsset = issue1.asset,
      amount = bigBuyOrderAmount,
      price = price,
      fee = bigBuyOrderMatcherFee,
      sender = buyer,
      matcher = matcher
    )

    val sellOrders = sellOrdersForBigBuyOrder(
      matcher = matcher,
      assetPair = AssetPair(issue2.asset, issue1.asset),
      price = price,
      matcherFeeAssetId = issue2.asset,
      sellers = sellers,
      totalAmount = sellersTotalAmount(bigBuyOrderAmount),
      totalMatcherFee = bigBuyOrderMatcherFee
    )

    val genesis = (matcher +: buyer +: sellers).map(acc => TxHelpers.genesis(acc.toAddress))

    val massTransfer = TxHelpers.massTransfer(
      from = buyer,
      to = sellers.map(seller => ParsedTransfer(seller.toAddress, TxNonNegativeAmount.unsafeFrom(issue2.quantity.value / sellOrdersCount))),
      asset = issue2.asset,
      fee = 1000L,
      version = TxVersion.V1
    )

    val buyMatcherFees = getSeqWithPredefinedSum(totalBuyMatcherFeeForExchangeTransactions, sellOrdersCount)

    val exchanges = (sellOrders zip buyMatcherFees).map { case (sellOrder, buyMatcherFee) =>
      TxHelpers.exchange(
        order1 = bigBuyOrder,
        order2 = sellOrder,
        matcher = matcher,
        amount = sellOrder.amount.value,
        price = bigBuyOrder.price.value,
        buyMatcherFee = buyMatcherFee,
        sellMatcherFee = sellOrder.matcherFee.value,
        fee = (bigBuyOrder.matcherFee.value + sellOrder.matcherFee.value) / 2
      )
    }

    (genesis, issue1, issue2, massTransfer, exchanges, bigBuyOrder)
  }

}
