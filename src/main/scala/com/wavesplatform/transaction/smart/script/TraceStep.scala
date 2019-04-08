package com.wavesplatform.transaction.smart.script

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ScriptResult
import com.wavesplatform.transaction.ValidationError

sealed abstract class TraceStep

case class AccountVerifierTrace(
    address: Address,
    resultE: Option[ValidationError]
) extends TraceStep

case class AssetVerifierTrace(
    id:     ByteStr,
    errorO: Option[ValidationError]
) extends TraceStep

case class InvokeScriptTrace(
    dAppAddress: Address,
    function:    FUNCTION_CALL,
    resultE:     Either[ValidationError, ScriptResult]
) extends TraceStep

