package com.wavesplatform.it.sync.smartcontract
import com.typesafe.config.Config
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class UtilsEstimatorToggleSuite extends BaseTransactionSuite with CancelAfterFailure {
  val estimatorV2ActivationHeight = 5
  val estimatorV3ActivationHeight = 8

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(NodeConfigs.Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.BlockReward.id, estimatorV2ActivationHeight),
          (BlockchainFeatures.BlockV5.id, estimatorV3ActivationHeight)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  val differentlyEstimatedScript: String =
    """
      | {-# STDLIB_VERSION 3 #-}
      | {-# CONTENT_TYPE EXPRESSION #-}
      |
      | let me = addressFromStringValue("")
      | func get() = getStringValue(me, "")
      | get() == get()
    """.stripMargin

  val v1Estimation = 467
  val v2Estimation = 342
  val v3Estimation = 330

  test("check estimations") {
    val compiledScript =
      ScriptCompiler.compile(differentlyEstimatedScript, ScriptEstimatorV1)
        .explicitGet()._1
        .bytes()
        .base64

    sender.scriptEstimate(compiledScript).complexity shouldBe v1Estimation
    sender.waitForHeight(estimatorV2ActivationHeight)
    sender.scriptEstimate(compiledScript).complexity shouldBe v2Estimation
    sender.waitForHeight(estimatorV3ActivationHeight)
    sender.scriptEstimate(compiledScript).complexity shouldBe v3Estimation
  }
}
