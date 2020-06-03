package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.script.ScriptCompiler

object TestValues {
  val OneWaves      = 1e8.toLong
  val ThousandWaves = OneWaves * 1000

  val keyPair: KeyPair   = TxHelpers.defaultSigner
  val address: Address   = keyPair.toAddress
  val asset: IssuedAsset = IssuedAsset(ByteStr(("A" * 32).getBytes("ASCII")))
  val bigMoney = com.wavesplatform.state.diffs.ENOUGH_AMT
  val timestamp = System.currentTimeMillis()
  val fee                = 1e6.toLong

  val Right((script, scriptComplexity)) = ScriptCompiler.compile(
    """
      |{-# STDLIB_VERSION 2 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |true
      |""".stripMargin,
    ScriptEstimatorV1
  )

  val Right((assetScript, assetScriptComplexity)) = ScriptCompiler.compile(
    """
      |{-# STDLIB_VERSION 2 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |true
      |""".stripMargin,
    ScriptEstimatorV1
  )
}
