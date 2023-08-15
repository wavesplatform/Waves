package com.wavesplatform.utx

import scala.concurrent.duration.*
import com.wavesplatform.TestValues
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.{Height, TxMeta}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.concurrent.Eventually

//noinspection RedundantDefaultArgument
class UtxFailedTxsSpec extends FlatSpec with WithDomain with Eventually {
  val dApp: KeyPair = TxHelpers.secondSigner

  override implicit val patienceConfig: PatienceConfig = super.patienceConfig.copy(timeout = 1 second)

  "UTX pool" should s"drop failed Invoke with complexity <= ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    d.appendBlock(TxHelpers.setScript(dApp, genScript(ContractLimits.FailFreeInvokeComplexity)))

    val tx = TxHelpers.invoke(dApp.toAddress)
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isLeft)
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)
    utx.putIfNew(tx, forceValidate = false).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")

    utx.addAndScheduleCleanup(Seq(tx))
    eventually {
      utx.size shouldBe 0
    }
  }

  it should s"accept failed Invoke with complexity > ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    d.appendBlock(TxHelpers.setScript(dApp, genScript(ContractLimits.FailFreeInvokeComplexity * 2)))

    val tx = TxHelpers.invoke(dApp.toAddress)

    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.addAndScheduleCleanup(Nil)
    Thread.sleep(5000)
    utx.size shouldBe 1

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, None)._1 shouldBe Some(Seq(tx))
  }

  it should s"reject Invoke with complexity > ${ContractLimits.FailFreeInvokeComplexity} and failed transfer" in utxTest { (d, utx) =>
    val scriptText = s"""{-# STDLIB_VERSION 4 #-}
                        |{-# CONTENT_TYPE DAPP #-}
                        |{-# SCRIPT_TYPE ACCOUNT #-}
                        |
                        |@Callable(i)
                        |func default() = {
                        |  if (${genExpr(1500, result = true)}) then [
                        |    ScriptTransfer(i.caller, 15, base58'${TestValues.asset}')
                        |  ] else []
                        |}
                        |""".stripMargin
    d.appendBlock(TxHelpers.setScript(dApp, TxHelpers.script(scriptText)))

    val tx = TxHelpers.invoke(dApp.toAddress)

    utx.putIfNew(tx, forceValidate = true).resultE should produce("negative asset balance")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.cleanUnconfirmed()
    utx.all shouldBe Seq(tx)

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, None)._1 shouldBe None
    intercept[RuntimeException](d.appendBlock(tx))

    d.blockchain.transactionMeta(tx.id()) shouldBe None
  }

  it should s"accept failed Invoke with complexity > ${ContractLimits.FailFreeInvokeComplexity} and failed transfer after SC activation" in withFS(
    TestFunctionalitySettings
      .withFeatures(BlockchainFeatures.BlockV5, BlockchainFeatures.Ride4DApps, BlockchainFeatures.SynchronousCalls, BlockchainFeatures.SmartAccounts)
  )(utxTest { (d, utx) =>
    val scriptText = s"""{-# STDLIB_VERSION 4 #-}
                        |{-# CONTENT_TYPE DAPP #-}
                        |{-# SCRIPT_TYPE ACCOUNT #-}
                        |
                        |@Callable(i)
                        |func default() = {
                        |  if (${genExpr(1500, result = true)}) then [
                        |    ScriptTransfer(i.caller, 15, base58'${TestValues.asset}')
                        |  ] else []
                        |}
                        |""".stripMargin
    d.appendBlock(TxHelpers.setScript(dApp, TxHelpers.script(scriptText)))

    val tx = TxHelpers.invoke(dApp.toAddress)

    utx.putIfNew(tx, forceValidate = true).resultE should produce(s"Transfer error: asset '${TestValues.asset}' is not found on the blockchain")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.addAndScheduleCleanup(Nil)
    Thread.sleep(5000)
    utx.size shouldBe 1

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, None)._1 shouldBe Some(Seq(tx))
    d.appendBlock(tx)

    d.blockchain.transactionMeta(tx.id()) shouldBe Some(TxMeta(Height(3), Status.Failed, 1212))
  })

  it should s"drop failed Invoke with asset script with complexity <= ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = Some(genAssetScript(800)))
    d.appendBlock(
      TxHelpers.setScript(dApp, genScript(0, result = true)),
      issue
    )

    val tx = TxHelpers.invoke(dApp.toAddress, payments = Seq(Payment(1L, issue.asset)), fee = TestValues.invokeFee(1))
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isLeft)
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)
    utx.putIfNew(tx, forceValidate = false).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")

    utx.addTransaction(tx, verify = false)
    utx.cleanUnconfirmed()
    utx.all shouldBe Nil
  }

  it should s"accept failed Invoke with asset script with complexity > ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = Some(genAssetScript(ContractLimits.FailFreeInvokeComplexity * 2)))
    d.appendBlock(TxHelpers.setScript(dApp, genScript(0, result = true)), issue)

    val tx = TxHelpers.invoke(dApp.toAddress, payments = Seq(Payment(1L, issue.asset)), fee = TestValues.invokeFee(1))
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isRight)
    utx.removeAll(Seq(tx))
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)

    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.addAndScheduleCleanup(Nil)
    Thread.sleep(5000)
    utx.size shouldBe 1

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, None)._1 shouldBe Some(Seq(tx))
  }

  it should s"drop failed Exchange with asset script with complexity <= ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = Some(genAssetScript(800)))
    d.appendBlock(issue)

    val tx =
      TxHelpers.exchangeFromOrders(TxHelpers.orderV3(OrderType.BUY, issue.asset), TxHelpers.orderV3(OrderType.SELL, issue.asset))
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isLeft)
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)
    utx.putIfNew(tx, forceValidate = false).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")

    utx.addAndScheduleCleanup(Seq(tx))
    eventually {
      utx.size shouldBe 0
    }
  }

  it should s"accept failed Exchange with asset script with complexity > ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = Some(genAssetScript(ContractLimits.FailFreeInvokeComplexity * 2)))
    d.appendBlock(issue)

    val tx =
      TxHelpers.exchangeFromOrders(TxHelpers.orderV3(OrderType.BUY, issue.asset), TxHelpers.orderV3(OrderType.SELL, issue.asset))

    assert(utx.putIfNew(tx, forceValidate = false).resultE.isRight)
    utx.removeAll(Seq(tx))
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)

    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.addAndScheduleCleanup(Nil)
    Thread.sleep(5000)
    utx.size shouldBe 1

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, None)._1 shouldBe Some(Seq(tx))
  }

  it should "cleanup transaction when script result changes" in utxTest { (d, utx) =>
    val (script, _) = ScriptCompiler
      .compile(
        """
          |{-# STDLIB_VERSION 4 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |
          |@Callable(i)
          |func test1000() = {
          |  if (height % 2 == 0) then
          |    []
          |  else
          |    if (!sigVerify(base58'', base58'', base58'')
          |    && !sigVerify(base58'', base58'', base58'')
          |    && !sigVerify(base58'', base58'', base58'')
          |    && !sigVerify(base58'', base58'', base58'')) then
          |      throw("height is odd")
          |    else [IntegerEntry("h", height)]
          |  }
          |  """.stripMargin,
        ScriptEstimatorV3(fixOverflow = true, overhead = true)
      )
      .explicitGet()

    d.appendBlock(TxHelpers.setScript(dApp, script))
    assert(d.blockchainUpdater.height % 2 == 0)

    (1 to 100).foreach { _ =>
      val invoke = TxHelpers.invoke(dApp.toAddress, Some("test1000"))
      utx.putIfNew(invoke, forceValidate = true).resultE.explicitGet()
    }

    utx.size shouldBe 100
    d.appendBlock() // Height is odd
    utx.addAndScheduleCleanup(Nil)
    eventually(timeout(10 seconds), interval(500 millis)) {
      utx.size shouldBe 0
      utx.all shouldBe Nil
    }
  }

  private[this] def genExpr(targetComplexity: Int, result: Boolean): String = {
    s"""
       |if ($result) then
       |  ${"sigVerify(base58'', base58'', base58'') ||" * ((targetComplexity / 200) - 1)} true
       |else
       |  ${"sigVerify(base58'', base58'', base58'') ||" * ((targetComplexity / 200) - 1)} false""".stripMargin
  }

  private[this] def genScript(targetComplexity: Int, result: Boolean = false): Script = {
    val expr = genExpr(targetComplexity, result) // ((1 to (targetComplexity / 2) - 2).map(_ => "true") :+ result.toString).mkString("&&")

    val scriptText = s"""
                        |{-#STDLIB_VERSION 4#-}
                        |{-#SCRIPT_TYPE ACCOUNT#-}
                        |{-#CONTENT_TYPE DAPP#-}
                        |
                        |@Callable(i)
                        |func default() = {
                        |  if ($expr) then [] else throw("reached err")
                        |}
                        |""".stripMargin
    TxHelpers.script(scriptText.stripMargin)
  }

  private[this] def genAssetScript(targetComplexity: Int, result: Boolean = false): Script = {
    val expr = genExpr(targetComplexity, result) // ((1 to (targetComplexity / 2) - 2).map(_ => "true") :+ result.toString).mkString("&&")

    val scriptText =
      s"""
         |{-#STDLIB_VERSION 4#-}
         |{-#SCRIPT_TYPE ASSET#-}
         |{-#CONTENT_TYPE EXPRESSION#-}
         |
         |if ($expr) then true else throw("reached err")
         |""".stripMargin
    val (script, _) = ScriptCompiler.compile(scriptText, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
    script
  }

  private[this] var settings = domainSettingsWithFS(
    TestFunctionalitySettings.withFeatures(
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAccountTrading,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.BlockV5,
      BlockchainFeatures.OrderV3
    )
  )

  private[this] def withFS(fs: FunctionalitySettings)(f: => Unit): Unit = {
    val oldSettings = settings
    settings = domainSettingsWithFS(fs)
    try f
    finally settings = oldSettings
  }

  private[this] def utxTest(f: (Domain, UtxPoolImpl) => Unit): Unit = {
    val balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner, dApp)

    withDomain(settings, balances) { d =>
      val utx = new UtxPoolImpl(ntpTime, d.blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
      f(d, utx)
      utx.close()
    }
  }
}
