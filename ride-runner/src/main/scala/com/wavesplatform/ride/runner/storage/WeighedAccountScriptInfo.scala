package com.wavesplatform.ride.runner.storage

import com.wavesplatform.state.AccountScriptInfo

case class WeighedAccountScriptInfo(scriptInfoWeight: Int, scriptInfo: AccountScriptInfo)
