package com.wavesplatform.transaction

sealed trait ApplicationStatus
case object Succeeded                 extends ApplicationStatus
case object ScriptExecutionFailed     extends ApplicationStatus
case object ScriptExecutionInProgress extends ApplicationStatus
