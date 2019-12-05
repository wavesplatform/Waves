package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{Assertion, CancelAfterFailure}

import scala.concurrent.duration._

class RideV4ActivationSuite extends BaseTransactionSuite with CancelAfterFailure {
  import RideV4ActivationSuite._

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((16, activationHeight - 1)))
      .buildNonConflicting()

  private val smartAcc  = pkByAddress(firstAddress)
  private val callerAcc = pkByAddress(secondAddress)

  private val dAppV4 =
    """{-# STDLIB_VERSION 4 #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |
      |@Callable(i)
      |func default () = [BooleanEntry("0", true)]
      |
      |""".stripMargin
  private val accountV4 =
    """{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |(tx.sender == this)""".stripMargin
  private val assetV4 =
    """
      |{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |this.quantity > 0
      |""".stripMargin
  private val assetV3 =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |this.quantity > 0
      |""".stripMargin

  test("can't set V4 contracts before the feature activation") {
    def assertFeatureNotActivated[R](f: => R): Assertion = assertApiError(f) { e =>
      e.statusCode shouldBe 400
      e.id shouldBe StateCheckFailed.Id
      e.message should include("Ride V4 and multiple attached payments for Invoke Script Transaction feature has not been activated")
    }

    assertFeatureNotActivated(sender.setScript(smartAcc.stringRepr, Some(dAppV4.compiled)))
    assertFeatureNotActivated(sender.setScript(smartAcc.stringRepr, Some(accountV4.compiled)))
    assertFeatureNotActivated(sender.issue(
      smartAcc.stringRepr, "Asset", "", 1, 0, script = Some(assetV4.compiled)
    ))

    val assetId = sender
      .issue(smartAcc.stringRepr, "Asset", "", 1, 0, script = Some(assetV3.compiled))
      .id
    assertFeatureNotActivated(sender.setAssetScript(
      assetId, smartAcc.stringRepr, script = Some(accountV4.compiled), fee = setAssetScriptFee + smartFee
    ))
  }

  test("can compile via API before the feature activation") {
    sender.scriptCompile(dAppV4).script shouldBe dAppV4.compiled
    sender.scriptCompile(accountV4).script shouldBe accountV4.compiled
    sender.scriptCompile(assetV4).script shouldBe assetV4.compiled
  }

  test("can decompile via API before the feature activation") {
    sender.scriptDecompile(dAppV4.compiled).script shouldBe dAppV4
    sender.scriptDecompile(accountV4.compiled).script shouldBe accountV4
    sender.scriptDecompile(assetV4.compiled).script shouldBe
      """{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |(this.quantity > 0)""".stripMargin
  }


  test(s"wait height $activationHeight for the feature activation") {
    sender.waitForHeight(activationHeight, 5.minutes)
  }


  test("can set asset script V4 after the function activation") {
    val assetId = sender.issue(
      smartAcc.stringRepr, "Test", "", 1000, 0, script = Some(assetV4.compiled), waitForTx = true
    ).id

    sender
      .setAssetScript(assetId, smartAcc.stringRepr, setAssetScriptFee + smartFee, Some(assetV4.compiled), waitForTx = true)
      .id
  }

  test("can set account script V4 after the feature activation") {
    sender.setScript(smartAcc.stringRepr, Some(accountV4.compiled), waitForTx = true)
    sender.setScript(smartAcc.stringRepr, Some(dAppV4.compiled), fee = setScriptFee + smartFee, waitForTx = true)

    sender.invokeScript(callerAcc.stringRepr, smartAcc.stringRepr, None, waitForTx = true)._1.id
  }

}

object RideV4ActivationSuite {
  private val estimator = ScriptEstimatorV2
  val activationHeight = 9

  implicit class CompiledFromString(script: String) {
    def compiled: String = ScriptCompiler.compile(script, estimator).explicitGet()._1.bytes().base64
  }

}
