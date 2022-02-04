package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit, ScriptExtraFee}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalacheck.Gen

object TestValues {
  val keyPair: KeyPair   = TxHelpers.defaultSigner
  val address: Address   = keyPair.toAddress
  val asset: IssuedAsset = IssuedAsset(ByteStr(("A" * 32).getBytes("ASCII")))
  val bigMoney: Long     = com.wavesplatform.state.diffs.ENOUGH_AMT
  val timestamp: Long    = System.currentTimeMillis()
  val fee: Long          = 1e6.toLong

  val invokeFee: Long = FeeUnit * FeeConstants(InvokeScriptTransaction.typeId)

  def invokeFee(scripts: Int = 0, issues: Int = 0): Gen[Long] =
    invokeFee + scripts * ScriptExtraFee + issues * FeeConstants(IssueTransaction.typeId) * FeeUnit

  val (script, scriptComplexity) = ScriptCompiler
    .compile(
      """
      |{-# STDLIB_VERSION 2 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |true
      |""".stripMargin,
      ScriptEstimatorV1
    )
    .explicitGet()

  val (assetScript, assetScriptComplexity) = ScriptCompiler
    .compile(
      """
      |{-# STDLIB_VERSION 2 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |true
      |""".stripMargin,
      ScriptEstimatorV1
    )
    .explicitGet()

  val (rejectAssetScript, rejectAssetScriptComplexity) = ScriptCompiler
    .compile(
      """
      |{-# STDLIB_VERSION 2 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |false
      |""".stripMargin,
      ScriptEstimatorV1
    )
    .explicitGet()
}
