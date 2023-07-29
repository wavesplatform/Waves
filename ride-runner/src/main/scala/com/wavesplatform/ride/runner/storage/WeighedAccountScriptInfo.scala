package com.wavesplatform.ride.runner.storage

import com.google.common.collect.Interners
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.AccountScriptInfo

// TODO AccountScriptInfo instead of separate fields
case class WeighedAccountScriptInfo private (
    publicKey: PublicKey,
    scriptInfoWeight: Int,
    script: Script,
    verifierComplexity: Long,
    complexitiesByEstimator: Map[Int, Map[String, Long]]
) {
  val accountScriptInfo = AccountScriptInfo(
    publicKey = publicKey,
    script = script,
    verifierComplexity = verifierComplexity,
    complexitiesByEstimator = complexitiesByEstimator
  )
}

object WeighedAccountScriptInfo {
  private val interner = Interners.newWeakInterner[WeighedAccountScriptInfo]() // Most scripts are the same

  def apply(
      publicKey: PublicKey,
      scriptInfoWeight: Int,
      script: Script,
      verifierComplexity: Long,
      complexitiesByEstimator: Map[Int, Map[String, Long]]
  ): WeighedAccountScriptInfo = interner.intern(
    new WeighedAccountScriptInfo(publicKey, scriptInfoWeight, script, verifierComplexity, complexitiesByEstimator)
  )
}
