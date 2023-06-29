package com.wavesplatform.api.http.utils

import cats.syntax.either.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.state.BlockchainOverrides
import com.wavesplatform.transaction.TxValidationError.GenericError
import play.api.libs.json.*

case class UtilsExprRequest(
    expr: Either[ByteStr, String],
    state: BlockchainOverrides = BlockchainOverrides()
) {
  def parseCall(version: StdLibVersion): Either[GenericError, Terms.EXPR] = expr match {
    case Left(binaryCall) => SerdeV1.deserialize(binaryCall.arr).bimap(GenericError(_), _._1)
    case Right(textCall)  => UtilsEvaluator.compile(version)(textCall)
  }
}

object UtilsExprRequest {
  implicit val byteStrFormat: Format[ByteStr]            = com.wavesplatform.utils.byteStrFormat
  implicit val exprReads: Reads[Either[ByteStr, String]] = com.wavesplatform.api.http.eitherReads[ByteStr, String]
  implicit val reads: Reads[UtilsExprRequest]            = Json.using[Json.WithDefaultValues].reads[UtilsExprRequest]
}
