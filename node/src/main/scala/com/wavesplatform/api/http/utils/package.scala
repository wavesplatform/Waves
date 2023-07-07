package com.wavesplatform.api.http

import cats.syntax.either.*
import com.wavesplatform.api.http.utils.UtilsEvaluator.{ConflictingRequestStructure, ParseJsonError}
import com.wavesplatform.lang.ValidationError
import play.api.libs.json.{JsError, JsObject, JsSuccess}

package object utils {
  val empty32Bytes = new Array[Byte](32)

  def parseEvaluateRequest(request: JsObject): Either[ValidationError, UtilsEvaluationRequest] = {
    def withoutCommonFields = request - "state"
    (request.validate[UtilsExprRequest], request.validate[UtilsInvocationRequest]) match {
      case (JsSuccess(_, _), JsSuccess(_, _)) if withoutCommonFields.fields.size > 1 =>
        ConflictingRequestStructure.asLeft
      case (JsSuccess(exprRequest, _), _)       => exprRequest.asRight
      case (_, JsSuccess(invocationRequest, _)) => invocationRequest.asRight
      case (_, e: JsError)                      => ParseJsonError(e).asLeft
    }
  }
}
