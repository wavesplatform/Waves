package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.RideV4ActivationSuite._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import org.scalatest.{Assertion, CancelAfterFailure}

import scala.concurrent.duration.DurationInt

class RideV5ActivationSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val activationHeight = 6

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.Ride4DApps.id, 0)))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.SynchronousCalls.id, activationHeight - 1)))
      .buildNonConflicting()

  private def smartAccV5 = firstKeyPair
  private def callerAcc  = secondKeyPair

  private val dAppV5 =
    """
      |{-# STDLIB_VERSION 5 #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |@Callable(i)
      |func default() = [BooleanEntry("0", true)]
      |
      |@Callable(i)
      |func payBack() = [ScriptTransfer(i.caller, i.payments[0].amount, value(i.payments[0].assetId))]
      |
    """.stripMargin

  private val accountV5 =
    """
      |{-# STDLIB_VERSION 5 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |(tx.sender == this)
    """.stripMargin

  private val assetV5 =
    """
      |{-# STDLIB_VERSION 5 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |this.quantity > 0
    """.stripMargin

  private val assetV4 =
    """
      |{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |this.quantity > 0
    """.stripMargin

  private val scriptWithoutVersion =
    """
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
    """.stripMargin

  test("can't set V5 contracts before the feature activation") {
    def assertFeatureNotActivated[R](f: => R): Assertion = assertApiError(f) { e =>
      e.statusCode shouldBe 400
      e.id shouldBe StateCheckFailed.Id
      e.message should include("Ride V5, dApp-to-dApp invocations feature has not been activated")
    }

    assertFeatureNotActivated(sender.setScript(smartAccV5, Some(dAppV5.compiled)))
    assertFeatureNotActivated(sender.setScript(smartAccV5, Some(accountV5.compiled)))
    assertFeatureNotActivated(
      sender.issue(smartAccV5, "Asset", "", 1, 0, script = Some(assetV5.compiled))
    )

    val assetId = sender
      .issue(smartAccV5, "Asset", "", 1, 0, script = Some(assetV4.compiled))
      .id
    assertFeatureNotActivated(
      sender.setAssetScript(
        assetId,
        smartAccV5,
        script = Some(accountV5.compiled),
        fee = setAssetScriptFee + smartFee
      )
    )
  }

  test("can compile via API before the feature activation") {
    sender.scriptCompile(dAppV5).script shouldBe dAppV5.compiled
    sender.scriptCompile(accountV5).script shouldBe accountV5.compiled
    sender.scriptCompile(assetV5).script shouldBe assetV5.compiled
  }

  test("can decompile via API before the feature activation") {
    sender.scriptDecompile(dAppV5.compiled).script shouldBe
      """{-# STDLIB_VERSION 5 #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |
        |@Callable(i)
        |func default () = [BooleanEntry("0", true)]
        |
        |
        |
        |@Callable(i)
        |func payBack () = [ScriptTransfer(i.caller, i.payments[0].amount, value(i.payments[0].assetId))]
        |
        |""".stripMargin

    sender.scriptDecompile(accountV5.compiled).script shouldBe
      """{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |(tx.sender == this)""".stripMargin

    sender.scriptDecompile(assetV5.compiled).script shouldBe
      """{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |(this.quantity > 0)""".stripMargin
  }

  test("script compiles as Ride V4 before the feature activation if STDLIB_VERSION isn't specified") {
    sender.scriptDecompile(sender.scriptCompile(scriptWithoutVersion).script).script should startWith("{-# STDLIB_VERSION 4 #-}")
  }

  test("wait for the feature activation") {
    sender.waitForHeight(activationHeight, 5.minutes)
  }

  test("script compiles as Ride V5 before the feature activation if STDLIB_VERSION isn't specified") {
    sender.scriptDecompile(sender.scriptCompile(scriptWithoutVersion).script).script should startWith("{-# STDLIB_VERSION 5 #-}")
  }

  test("can set asset script V5 after the feature activation") {
    val assetId = sender
      .issue(
        smartAccV5,
        "Test",
        "",
        1000,
        0,
        script = Some(assetV5.compiled),
        waitForTx = true
      )
      .id

    sender.setAssetScript(assetId, smartAccV5, setAssetScriptFee + smartFee, Some(assetV5.compiled), waitForTx = true)
  }

  test("can set and invoke account script V5 after the feature activation") {
    sender.setScript(smartAccV5, Some(accountV5.compiled), waitForTx = true)
    sender.setScript(smartAccV5, Some(dAppV5.compiled), fee = setScriptFee + smartFee, waitForTx = true)

    sender.invokeScript(callerAcc, smartAccV5.toAddress.toString, waitForTx = true)
  }
}
