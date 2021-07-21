package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.script.ScriptCompiler

object TestValues {
  val keyPair: KeyPair   = TxHelpers.defaultSigner
  def address: Address   = keyPair.toAddress
  val asset: IssuedAsset = IssuedAsset(ByteStr(("A" * 32).getBytes("ASCII")))
  val bigMoney: Long     = com.wavesplatform.state.diffs.ENOUGH_AMT
  val timestamp: Long    = System.currentTimeMillis()
  val fee: Long          = 1e6.toLong

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
