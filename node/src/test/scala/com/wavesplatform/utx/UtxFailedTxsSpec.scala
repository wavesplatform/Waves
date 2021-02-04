package com.wavesplatform.utx

import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import monix.reactive.subjects.PublishSubject
import org.scalatest.concurrent.Eventually
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

//noinspection RedundantDefaultArgument
class UtxFailedTxsSpec extends FlatSpec with Matchers with WithDomain with Eventually {
  val dApp = TxHelpers.secondSigner

  "UTX pool" should s"drop failed Invoke with complexity <= ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    d.appendBlock(TxHelpers.setScript(dApp, genScript(ContractLimits.FailFreeInvokeComplexity)))

    val tx = TxHelpers.invoke(dApp.toAddress, "test")
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isLeft)
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)
    utx.putIfNew(tx, forceValidate = false).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")

    utx.addAndCleanup(Seq(tx))
    eventually {
      utx.size shouldBe 0
    }
  }

  it should s"accept failed Invoke with complexity > ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    d.appendBlock(TxHelpers.setScript(dApp, genScript(ContractLimits.FailFreeInvokeComplexity * 2)))

    val tx = TxHelpers.invoke(dApp.toAddress, "test")
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isRight)
    utx.removeAll(Seq(tx))
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)

    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.addAndCleanup(Nil)
    Thread.sleep(5000)
    utx.size shouldBe 1

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited)._1 shouldBe Some(Seq(tx))
  }

  it should s"drop failed Invoke with asset script with complexity <= ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = genAssetScript(800))
    d.appendBlock(
      TxHelpers.setScript(dApp, genScript(0, result = true)),
      issue
    )

    val tx = TxHelpers.invoke(dApp.toAddress, "test", payments = Seq(Payment(1L, issue.asset)))
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isLeft)
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)
    utx.putIfNew(tx, forceValidate = false).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")

    utx.addAndCleanup(Seq(tx))
    eventually {
      utx.size shouldBe 0
    }
  }

  it should s"accept failed Invoke with asset script with complexity > ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = genAssetScript(ContractLimits.FailFreeInvokeComplexity * 2))
    d.appendBlock(TxHelpers.setScript(dApp, genScript(0, result = true)), issue)

    val tx = TxHelpers.invoke(dApp.toAddress, "test", payments = Seq(Payment(1L, issue.asset)))
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isRight)
    utx.removeAll(Seq(tx))
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)

    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.addAndCleanup(Nil)
    Thread.sleep(5000)
    utx.size shouldBe 1

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited)._1 shouldBe Some(Seq(tx))
  }

  it should s"drop failed Exchange with asset script with complexity <= ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = genAssetScript(800))
    d.appendBlock(issue)

    val tx =
      TxHelpers.exchange(TxHelpers.order(OrderType.BUY, issue.asset), TxHelpers.order(OrderType.SELL, issue.asset))
    assert(utx.putIfNew(tx, forceValidate = false).resultE.isLeft)
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)
    utx.putIfNew(tx, forceValidate = false).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")

    utx.addAndCleanup(Seq(tx))
    eventually {
      utx.size shouldBe 0
    }
  }

  it should s"accept failed Exchange with asset script with complexity > ${ContractLimits.FailFreeInvokeComplexity}" in utxTest { (d, utx) =>
    val issue = TxHelpers.issue(script = genAssetScript(ContractLimits.FailFreeInvokeComplexity * 2))
    d.appendBlock(issue)

    val tx =
      TxHelpers.exchange(TxHelpers.order(OrderType.BUY, issue.asset), TxHelpers.order(OrderType.SELL, issue.asset))

    assert(utx.putIfNew(tx, forceValidate = false).resultE.isRight)
    utx.removeAll(Seq(tx))
    assert(utx.putIfNew(tx, forceValidate = true).resultE.isLeft)

    utx.putIfNew(tx, forceValidate = true).resultE should produce("reached err")
    utx.putIfNew(tx, forceValidate = false).resultE shouldBe Right(true)

    utx.addAndCleanup(Nil)
    Thread.sleep(5000)
    utx.size shouldBe 1

    utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited)._1 shouldBe Some(Seq(tx))
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
        ScriptEstimatorV3
      )
      .explicitGet()

    d.appendBlock(TxHelpers.setScript(dApp, script))
    assert(d.blockchainUpdater.height % 2 == 0)

    (1 to 100).foreach { _ =>
      val invoke = TxHelpers.invoke(dApp.toAddress, "test1000")
      utx.putIfNew(invoke, forceValidate = true).resultE.explicitGet()
    }

    utx.size shouldBe 100
    d.appendBlock() // Height is odd
    utx.addAndCleanup(Nil)
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

    val scriptText =
      s"""
        |{-#STDLIB_VERSION 4#-}
        |{-#SCRIPT_TYPE ACCOUNT#-}
        |{-#CONTENT_TYPE DAPP#-}
        |
        |@Callable(i)
        |func test() = {
        |  if ($expr) then [] else throw("reached err")
        |}
        |""".stripMargin
    val (script, _) = ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()
    script
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
    val (script, _) = ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()
    script
  }

  private[this] def utxTest(f: (Domain, UtxPoolImpl) => Unit): Unit = {
    val settings = domainSettingsWithFS(
      TestFunctionalitySettings.withFeatures(
        BlockchainFeatures.SmartAssets,
        BlockchainFeatures.SmartAccounts,
        BlockchainFeatures.SmartAccountTrading,
        BlockchainFeatures.Ride4DApps,
        BlockchainFeatures.BlockV5,
        BlockchainFeatures.OrderV3
      )
    )

    withDomain(settings) { d =>
      d.appendBlock(
        TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Long.MaxValue / 3),
        TxHelpers.genesis(dApp.toAddress, Long.MaxValue / 3)
      )

      val utx = new UtxPoolImpl(ntpTime, d.blockchainUpdater, PublishSubject(), settings.utxSettings)
      f(d, utx)
      utx.close()
    }
  }
}
