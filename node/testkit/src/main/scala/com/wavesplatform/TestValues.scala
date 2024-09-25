package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.{AssetDescription, Height}
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit, ScriptExtraFee}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{TransactionType, TxHelpers}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

object TestValues {
  val keyPair: KeyPair   = TxHelpers.defaultSigner
  val address: Address   = keyPair.toAddress
  val asset: IssuedAsset = IssuedAsset(ByteStr(("A" * 32).getBytes("ASCII")))
  val bigMoney: Long     = com.wavesplatform.state.diffs.ENOUGH_AMT
  val timestamp: Long    = System.currentTimeMillis()
  val fee: Long          = 1e6.toLong

  val invokeFee: Long = FeeUnit * FeeConstants(TransactionType.InvokeScript)

  def invokeFee(scripts: Int = 0, issues: Int = 0): Long =
    invokeFee + scripts * ScriptExtraFee + issues * FeeConstants(TransactionType.Issue) * FeeUnit

  lazy val (script, scriptComplexity) = ScriptCompiler
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

  lazy val (assetScript, assetScriptComplexity) = ScriptCompiler
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

  lazy val (rejectAssetScript, rejectAssetScriptComplexity) = ScriptCompiler
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

  val assetDescription: AssetDescription = AssetDescription(
    asset.id,
    TxHelpers.defaultSigner.publicKey,
    null,
    null,
    0,
    reissuable = true,
    BigInt(1),
    Height(1),
    None,
    0,
    nft = false,
    0,
    Height(1)
  )
}
