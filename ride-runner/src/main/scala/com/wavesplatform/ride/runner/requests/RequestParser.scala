package com.wavesplatform.ride.runner.requests

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.utils.UtilsEvaluator.*
import com.wavesplatform.api.http.utils.{UtilsEvaluator, UtilsInvocationRequest}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.ride.runner.input.RideRunnerInputParser.*
import play.api.libs.json.{JsError, JsObject, JsSuccess}

object RequestParser {

  def parse(stdLibVersion: StdLibVersion, address: Address, request: JsObject): Either[ValidationError, Evaluation] = {
    (request.value.get("expr"), request.validate[UtilsInvocationRequest]) match {
      case (Some(_), JsSuccess(_, _)) if request.fields.size > 1 => Left(ConflictingRequestStructure)
      // Handles if both specified, but UtilsInvocationRequest e.g. has a wrong argument type.
      // All fields in UtilsInvocationRequest are optional, so JSON with no fields is successfully parsed as UtilsInvocationRequest.
      case (Some(_), _: JsError) => Left(ConflictingRequestStructure)
      case (Some(exprRequest), _) =>
        parseCall(exprRequest, stdLibVersion).map { expr =>
          ExprEvaluation(expr, UtilsEvaluator.emptyInvokeScriptLike(address))
        }
      case (None, JsSuccess(invocationRequest, _)) =>
        invocationRequest.toInvocation.map { invocation =>
          InvocationEvaluation(invocation, UtilsEvaluator.toInvokeScriptLike(invocation, address))
        }
      case (None, e: JsError) => Left(ParseJsonError(e))
    }
  }

}
