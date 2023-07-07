package com.wavesplatform.api.http.utils

import cats.syntax.either.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.utils.UtilsApiRoute.{DefaultAddress, DefaultPublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.TxValidationError.GenericError
import play.api.libs.json.JsObject

sealed trait Evaluation extends Product with Serializable {
  def txLike: InvokeScriptTransactionLike
  def dAppToExpr(dApp: DApp, script: Script): Either[ValidationError, EXPR]
}

object Evaluation {
  def parse(stdLibVersion: StdLibVersion, address: Address, request: JsObject): Either[ValidationError, Evaluation] =
    parseEvaluateRequest(request).flatMap {
      case evRequest: UtilsExprRequest =>
        evRequest.parseCall(stdLibVersion).map { expr =>
          ExprEvaluation(expr, UtilsEvaluator.emptyInvokeScriptLike(address))
        }
      case evRequest: UtilsInvocationRequest =>
        evRequest.toInvocation.map { invocation =>
          InvocationEvaluation(invocation, UtilsEvaluator.toInvokeScriptLike(invocation, address))
        }

      case _ => throw new RuntimeException("Impossible")
    }
}

case class ExprEvaluation(expr: Terms.EXPR, txLike: InvokeScriptTransactionLike) extends Evaluation {
  def dAppToExpr(dApp: DApp, script: Script): Either[ValidationError, EXPR] =
    Right(ContractEvaluator.buildSyntheticCall(dApp, expr, ByteStr(DefaultAddress.bytes), DefaultPublicKey))
}

case class InvocationEvaluation(invocation: ContractEvaluator.Invocation, txLike: InvokeScriptTransactionLike) extends Evaluation {
  def dAppToExpr(dApp: DApp, script: Script): Either[ValidationError, EXPR] =
    ContractEvaluator.buildExprFromInvocation(dApp, invocation, script.stdLibVersion).bimap(e => GenericError(e.message), _.expr)
}
