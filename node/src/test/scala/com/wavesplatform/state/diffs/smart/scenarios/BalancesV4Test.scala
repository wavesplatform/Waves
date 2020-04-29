package com.wavesplatform.state.diffs.smart.scenarios

import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.utils._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, Asset => AssetType, _}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript/*, Script*/}
//import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.parser._
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
//import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BalancesV4Test extends PropSpec with PropertyChecks with WithState with TransactionGen with NoShrink {

  val MinFee: Long            = (Constants.UnitsInWave / 1000L).toLong
  val DataTxFee: Long         = 15000000L
  val InvokeScriptTxFee: Long = 15000000L
  val MassTransferTxFee: Long = 15000000L
  val SetScriptFee: Long      = (Constants.UnitsInWave / 1000L).toLong
  val SetAssetScriptFee: Long = Constants.UnitsInWave

  val rideV4Activated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 0,
        BlockchainFeatures.SmartAccounts.id -> 0,
        BlockchainFeatures.BlockV5.id    -> 0
      )
    )

  def script(a: ByteStr) = {
    val ctx = {
        val directives = DirectiveSet(V4, Account, DAppType).explicitGet()
        PureContext.build(Global, V4).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
          WavesContext.build(directives)
      }

    val script =
      s"""
         | {-#STDLIB_VERSION 4 #-}
         | {-#SCRIPT_TYPE ACCOUNT #-}
         | {-#CONTENT_TYPE DAPP #-}
         |
         | @Callable(i)
         | func bar() = {
         |  let balance = wavesBalance(Address(base58'${a}'))
         |   [
         |     IntegerEntry("available", balance.available),
         |     IntegerEntry("regular", balance.regular),
         |     IntegerEntry("generating", balance.generating),
         |     IntegerEntry("effective", balance.effective)
         |   ]
         | }
      """.stripMargin
    val dApp = ContractCompiler.compile(script, ctx.compilerContext, V4).explicitGet()
    ContractScript(V4, dApp).explicitGet()
  }

  val functionCall =
     Some(
       FUNCTION_CALL(
         User("bar"),
         List()
       )
     )

  val preconditionsAndTransfer = for {
    master    <- accountGen
    acc1    <- accountGen
    acc2 <- accountGen
    dapp <- accountGen
    ts        <- positiveIntGen
    genesis = Seq(
      GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet(),
      GenesisTransaction.create(acc1.toAddress, 25 * Constants.UnitsInWave + 3 * MinFee, ts).explicitGet(),
      GenesisTransaction.create(dapp.toAddress, 10 * Constants.UnitsInWave + SetScriptFee + 2 * InvokeScriptTxFee + 1 * Constants.UnitsInWave, ts).explicitGet()
    )
    setScript = SetScriptTransaction.selfSigned(1.toByte, dapp, Some(script(ByteStr(acc1.toAddress.bytes))), SetScriptFee, ts).explicitGet()
    ci = InvokeScriptTransaction.selfSigned(1.toByte, master, dapp.toAddress, functionCall, Nil, InvokeScriptTxFee, Waves, ts + 3).explicitGet()
    lease1 = LeaseTransaction.selfSigned(2.toByte, acc1, dapp.toAddress, 10 * Constants.UnitsInWave, MinFee, ts + 2).explicitGet()
    lease2 = LeaseTransaction.selfSigned(2.toByte, acc1, dapp.toAddress, 10 * Constants.UnitsInWave, MinFee, ts + 3).explicitGet()
    leaseD = LeaseTransaction.selfSigned(2.toByte, dapp, acc1.toAddress, 1 * Constants.UnitsInWave, MinFee, ts + 3).explicitGet()
    cancel1 = LeaseCancelTransaction.signed(1.toByte, acc1.publicKey, lease1.id(), MinFee, ts + 4, acc1.privateKey).explicitGet()
    t = TransferTransaction.selfSigned(TxVersion.V2, dapp, acc1.toAddress, Waves, 1 * Constants.UnitsInWave, Waves, InvokeScriptTxFee, None, ts + 5).explicitGet()
  } yield {
    (genesis ++ Seq(setScript, lease1, lease2), Seq(cancel1, leaseD, t), master, acc1, acc2, dapp, ci)
  }

  property("Waves balance details") {

    forAll(preconditionsAndTransfer) {
      case (genesis, b, master, acc1, acc2, dapp, ci) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis)) ++
            (0 to 1000).map(_ => TestBlock.create(Seq())) ++
            Seq(TestBlock.create(b), TestBlock.create(Seq())),
          TestBlock.create(Seq(ci)), rideV4Activated) {
             case (d, s) =>
               val apiBalance = com.wavesplatform.api.common.CommonAccountsApi(d, null, s).balanceDetails(acc1.toAddress)
               val data = d.accountData(dapp.toAddress)
               data.data("available") shouldBe IntegerDataEntry("available", apiBalance.available)
               data.data("regular") shouldBe IntegerDataEntry("regular", apiBalance.regular)
               data.data("generating") shouldBe IntegerDataEntry("generating", apiBalance.generating)
               data.data("effective") shouldBe IntegerDataEntry("effective", apiBalance.effective)
               //println(s"diff = $d\nbalance = ${s.wavesPortfolio(acc1.toAddress)}\ndetail = ${apiBalance}\nheight = ${s.height}")

          }
    }
  }

  property("Asset balance change while processing script result") {
    def assetSctipt(acc: ByteStr) = {
     val ctx = {
        val directives = DirectiveSet(V4, AssetType, Expression).explicitGet()
        PureContext.build(Global, V4).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
          WavesContext.build(directives)
      }
  
     val script = s"""
     {-# STDLIB_VERSION 4 #-}
     {-# CONTENT_TYPE EXPRESSION #-}
     {-# SCRIPT_TYPE ASSET #-}
 
     assetBalance(Address(base58'${acc}'), this.id) == 10000000000
     """
     val parsedScript = Parser.parseExpr(script).get.value
     ExprScript(V4, ExpressionCompiler(ctx.compilerContext, parsedScript).explicitGet()._1)
          .explicitGet()
    }

    def dappScript(acc: ByteStr, asset: ByteStr) = {
      val ctx = {
          val directives = DirectiveSet(V4, Account, DAppType).explicitGet()
          PureContext.build(Global, V4).withEnvironment[Environment] |+|
            CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
            WavesContext.build(directives)
        }

      val script =
        s"""
           | {-#STDLIB_VERSION 4 #-}
           | {-#SCRIPT_TYPE ACCOUNT #-}
           | {-#CONTENT_TYPE DAPP #-}
           |
           | @Callable(i)
           | func bar() = {
           |   [
           |    ScriptTransfer(Address(base58'${acc}'), 1, base58'${asset}'),
           |    Reissue(base58'${asset}', false, 1)
           |   ]
           | }
        """.stripMargin
      val dApp = ContractCompiler.compile(script, ctx.compilerContext, V4).explicitGet()
      ContractScript(V4, dApp).explicitGet()
    }

    val functionCall =
       Some(
         FUNCTION_CALL(
           User("bar"),
           List()
        )
      )

    val time   = new TestTime
    def nextTs = time.getTimestamp()

    forAll(for { a <- accountGen ; b <- accountGen } yield (a,b)) {
      case (acc1, acc2) =>
        val g1 = GenesisTransaction.create(acc1.toAddress, ENOUGH_AMT, nextTs).explicitGet()
        val issue = IssueTransaction(TxVersion.V1, acc1.publicKey, "testsname".utf8Bytes, "testdesc".utf8Bytes, 10000000000L, 8.toByte, reissuable = true, script = Some(assetSctipt(ByteStr(acc2.toAddress.bytes)) /*script(ByteStr(acc1.toAddress.bytes))*/), SetAssetScriptFee, nextTs).signWith(acc1.privateKey)
        val setScript = SetScriptTransaction.selfSigned(1.toByte, acc1, Some(dappScript(ByteStr(acc2.toAddress.bytes), issue.id())), SetScriptFee, nextTs).explicitGet()
        val ci = InvokeScriptTransaction.selfSigned(1.toByte, acc1, acc1.toAddress, functionCall, Nil, InvokeScriptTxFee, Waves, nextTs).explicitGet()

        assertDiffAndState(
          Seq(TestBlock.create(Seq(g1, issue, setScript))),
          TestBlock.create(Seq(ci)), rideV4Activated) {
             case (d, s) =>
               d.scriptResults(ci.id()).errorMessage shouldBe Some(InvokeScriptResult.ErrorMessage(3, "Transaction is not allowed by token-script"))
          }

    }
  }

}
