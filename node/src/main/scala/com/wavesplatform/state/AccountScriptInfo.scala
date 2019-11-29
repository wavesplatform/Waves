package com.wavesplatform.state

import com.wavesplatform.lang.script.Script

case class AccountScriptInfo(script: Script, verifierComplexity: Long, callableComplexity: Map[String, Long] = Map.empty)
