package com.wavesplatform.state.diffs

import cats.{Order => _, _}
import com.wavesplatform.OrderOps._
import com.wavesplatform.account.{AddressScheme, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Version.ExprV1
import com.wavesplatform.lang.directives.DirectiveParser
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompilerV1}
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.ValidationError.AccountBalanceError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.{Order, _}
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV1, IssueTransactionV2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.smart.script.{Script, ScriptCompiler}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.utils.functionCosts
import com.wavesplatform.{NoShrink, TransactionGen, crypto}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Inside, Matchers, PropSpec}

class ExchangeTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with Inside with NoShrink {

  val MATCHER: PrivateKeyAccount = PrivateKeyAccount.fromSeed("matcher").explicitGet()

  val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id       -> 0,
      BlockchainFeatures.SmartAssets.id         -> 0,
      BlockchainFeatures.SmartAccountTrading.id -> 0,
      BlockchainFeatures.Ride4DApps.id          -> 0
    )
  )

  property("preserves waves invariant, stores match info, rewards matcher") {

    val preconditionsAndExchange: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id())
      maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1)
      exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
    } yield (gen1, gen2, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1, issue2))), TestBlock.create(Seq(exchange)), fs) {
          case (blockDiff, state) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            blockDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
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
        exchangeV1GeneratorP(buyer, seller, None, Some(issue1.id()), fixedMatcherFee = Some(300000)),
        exchangeV2GeneratorP(buyer, seller, None, Some(issue1.id()), fixedMatcherFee = Some(300000))
      )
    } yield {
      (gen1, gen2, issue1, exchange)
    }

    forAll(preconditions) {
      case (gen1, gen2, issue1, exchange) =>
        whenever(exchange.amount > 300000) {
          assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(exchange)), fs) {
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

  def createExTx(buy: Order, sell: Order, price: Long, matcher: PrivateKeyAccount, ts: Long): Either[ValidationError, ExchangeTransaction] = {
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

    val preconditions: Gen[(PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
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
        val assetPair = AssetPair(Some(issue1.id()), None)
        val buy       = Order.buy(buyer, matcher, assetPair, 1000000L, price, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, 1L, price, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fs) {
          case (blockDiff, state) =>
            blockDiff.portfolios(tx.sender).balance shouldBe tx.buyMatcherFee + tx.sellMatcherFee - tx.fee
            state.portfolio(tx.sender).balance shouldBe 0L
        }
    }
  }

  property("Not enough balance") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
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
        val assetPair = AssetPair(Some(issue1.id()), None)
        val buy       = Order.buy(buyer, matcher, assetPair, issue1.quantity + 1, price, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, issue1.quantity + 1, price, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx)), fs) { totalDiffEi =>
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

    val preconditions: Gen[
      (PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
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
    val assetPair                                          = AssetPair(None, Some(issue1.id()))

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
        val enoughFee = CommonValidation.ScriptExtraFee + CommonValidation.FeeConstants(ExchangeTransaction.typeId) * CommonValidation.FeeUnit
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
            val newSig = ByteStr(crypto.sign(so.senderPublicKey.publicKey, bo.bodyBytes()))
            e1.copy(buyOrder = bo.updateProofs(Proofs(Seq(newSig))).asInstanceOf[OrderV1])
          case e2 @ ExchangeTransactionV2(bo, so, _, _, _, _, _, _, _) =>
            val newSig = ByteStr(crypto.sign(bo.senderPublicKey.publicKey, so.bodyBytes()))
            e2.copy(sellOrder = so.updateProofs(Proofs(Seq(newSig))))
        }

        val preconBlocks = Seq(
          TestBlock.create(Seq(gen1, gen2)),
          TestBlock.create(Seq(issue1, issue2))
        )

        val blockWithExchange = TestBlock.create(Seq(exchangeWithResignedOrder))

        assertLeft(preconBlocks, blockWithExchange, fs)("Script doesn't exist and proof doesn't validate as signature")
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
            ByteStr(crypto.sign(exchange.sender.publicKey, exchange.sellOrder.bodyBytes())),
            ByteStr(crypto.sign(exchange.sellOrder.senderPublicKey.publicKey, exchange.sellOrder.bodyBytes()))
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

        assertLeft(preconBlocks, blockWithExchange, fs)("Script doesn't exist and proof doesn't validate as signature")
    }
  }

  property("Disable use OrderV1 on SmartAccount") {
    val enoughFee        = 100000000
    val script           = "true"
    val txScriptCompiled = ScriptCompiler(script, isAssetScript = false).explicitGet()._1

    val sellerScript = Some(ScriptCompiler(script, isAssetScript = false).explicitGet()._1)
    val buyerScript  = Some(ScriptCompiler(script, isAssetScript = false).explicitGet()._1)

    val chainId = AddressScheme.current.chainId

    forAll(for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      genesis = GenesisTransaction.create(MATCHER, Long.MaxValue, ts).explicitGet()
      tr1     = createWavesTransfer(MATCHER, buyer.toAddress, Long.MaxValue / 3, enoughFee, ts + 1).explicitGet()
      tr2     = createWavesTransfer(MATCHER, seller.toAddress, Long.MaxValue / 3, enoughFee, ts + 2).explicitGet()
      asset1 = IssueTransactionV2
        .selfSigned(2: Byte, chainId, buyer, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, None, enoughFee, ts + 3)
        .explicitGet()
      asset2 = IssueTransactionV2
        .selfSigned(2: Byte, chainId, seller, "Asset#2".getBytes, "".getBytes, 1000000, 8, false, None, enoughFee, ts + 4)
        .explicitGet()
      setMatcherScript = SetScriptTransaction
        .selfSigned(1: Byte, MATCHER, Some(txScriptCompiled), enoughFee, ts + 5)
        .explicitGet()
      setSellerScript = SetScriptTransaction
        .selfSigned(1: Byte, seller, sellerScript, enoughFee, ts + 6)
        .explicitGet()
      setBuyerScript = SetScriptTransaction
        .selfSigned(1: Byte, buyer, buyerScript, enoughFee, ts + 7)
        .explicitGet()
      assetPair = AssetPair(Some(asset1.id()), Some(asset2.id()))
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

  def scriptGen(caseType: String, v: Boolean): String = {
    s"""
       |match tx {
       | case o: $caseType => $v
       | case _ => ${!v}
       |}
      """.stripMargin
  }

  def changeOrderSignature(signWith: Array[Byte], o: Order): Order = {
    lazy val newProofs = Proofs(Seq(ByteStr(crypto.sign(signWith, o.bodyBytes()))))

    o match {
      case o1 @ OrderV1(_, _, _, _, _, _, _, _, _, _) =>
        o1.copy(proofs = newProofs)
      case o2 @ OrderV2(_, _, _, _, _, _, _, _, _, _) =>
        o2.copy(proofs = newProofs)
    }
  }

  def changeTxSignature(signWith: Array[Byte], et: ExchangeTransaction): ExchangeTransaction = {
    lazy val newSignature = ByteStr(crypto.sign(signWith, et.bodyBytes()))
    lazy val newProofs    = Proofs(Seq(newSignature))

    et match {
      case e1 @ ExchangeTransactionV1(_, _, _, _, _, _, _, _, _) =>
        e1.copy(signature = newSignature)

      case e2 @ ExchangeTransactionV2(_, _, _, _, _, _, _, _, _) =>
        e2.copy(proofs = newProofs)
    }
  }

  def compile(scriptText: String, ctx: CompilerContext): Either[String, (Script, Long)] = {
    val compiler = new ExpressionCompilerV1(ctx)

    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.lines
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      expr       <- compiler.compile(scriptWithoutDirectives, directives)
      script     <- ExprScript(expr)
      complexity <- ScriptEstimator(ctx.varDefs.keySet, functionCosts(ExprV1), expr)
    } yield (script, complexity)
  }

  def smartTradePreconditions(buyerScriptSrc: String,
                              sellerScriptSrc: String,
                              txScript: String): Gen[(GenesisTransaction, List[TransferTransaction], List[Transaction], ExchangeTransaction)] = {
    val enoughFee = 100000000

    val txScriptCompiled = ScriptCompiler(txScript, isAssetScript = false).explicitGet()._1

    val sellerScript = Some(ScriptCompiler(sellerScriptSrc, isAssetScript = false).explicitGet()._1)
    val buyerScript  = Some(ScriptCompiler(buyerScriptSrc, isAssetScript = false).explicitGet()._1)

    val chainId = AddressScheme.current.chainId

    for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      genesis = GenesisTransaction.create(MATCHER, Long.MaxValue, ts).explicitGet()
      tr1     = createWavesTransfer(MATCHER, buyer.toAddress, Long.MaxValue / 3, enoughFee, ts + 1).explicitGet()
      tr2     = createWavesTransfer(MATCHER, seller.toAddress, Long.MaxValue / 3, enoughFee, ts + 2).explicitGet()
      asset1 = IssueTransactionV2
        .selfSigned(2: Byte, chainId, buyer, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, None, enoughFee, ts + 3)
        .explicitGet()
      asset2 = IssueTransactionV2
        .selfSigned(2: Byte, chainId, seller, "Asset#2".getBytes, "".getBytes, 1000000, 8, false, None, enoughFee, ts + 4)
        .explicitGet()
      setMatcherScript = SetScriptTransaction
        .selfSigned(1: Byte, MATCHER, Some(txScriptCompiled), enoughFee, ts + 5)
        .explicitGet()
      setSellerScript = SetScriptTransaction
        .selfSigned(1: Byte, seller, sellerScript, enoughFee, ts + 6)
        .explicitGet()
      setBuyerScript = SetScriptTransaction
        .selfSigned(1: Byte, buyer, buyerScript, enoughFee, ts + 7)
        .explicitGet()
      assetPair = AssetPair(Some(asset1.id()), Some(asset2.id()))
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
    maybeAsset1              <- Gen.option(issue1.id())
    maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1)
    exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
  } yield (gen1, gen2, issue1, issue2, exchange)

}
