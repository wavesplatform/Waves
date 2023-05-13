package com.wavesplatform.ride.runner.storage

import com.google.common.collect.Interners
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.AccountScriptInfo

case class WeighedAccountScriptInfo private (
    scriptInfoWeight: Int,
    script: Script,
    verifierComplexity: Long,
    complexitiesByEstimator: Map[Int, Map[String, Long]]
) {
  val accountScriptInfo = AccountScriptInfo(
    // It doesn't have this, because we expect that a user doesn't run scripts with actions
    publicKey = EmptyPublicKey,
    script = script,
    verifierComplexity = verifierComplexity,
    complexitiesByEstimator = complexitiesByEstimator
  )
}

object WeighedAccountScriptInfo {
  private val interner = Interners.newWeakInterner[WeighedAccountScriptInfo]() // Most scripts are the same

  def apply(
      scriptInfoWeight: Int,
      script: Script,
      verifierComplexity: Long,
      complexitiesByEstimator: Map[Int, Map[String, Long]]
  ): WeighedAccountScriptInfo = interner.intern(
    new WeighedAccountScriptInfo(
      scriptInfoWeight,
      script,
      verifierComplexity,
      complexitiesByEstimator
    )
  )
}
