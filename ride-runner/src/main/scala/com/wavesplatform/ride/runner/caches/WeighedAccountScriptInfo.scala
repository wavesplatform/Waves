package com.wavesplatform.ride.runner.caches

import com.google.common.collect.Interners
import com.wavesplatform.state.AccountScriptInfo

case class WeighedAccountScriptInfo private (scriptInfoWeight: Int, accountScriptInfo: AccountScriptInfo)

object WeighedAccountScriptInfo {
  private val interner = Interners.newWeakInterner[WeighedAccountScriptInfo]() // Most scripts are the same

  def apply(scriptInfoWeight: Int, accountScriptInfo: AccountScriptInfo): WeighedAccountScriptInfo =
    interner.intern(new WeighedAccountScriptInfo(scriptInfoWeight, accountScriptInfo))
}
