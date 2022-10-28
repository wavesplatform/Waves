package com.wavesplatform.api.http

import com.wavesplatform.account.Address
import com.wavesplatform.lang.contract.meta.FunctionSignatures
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TraceStep
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.*

trait JsonFormats {
  implicit lazy val wavesAddressWrites: Writes[Address] = Writes(w => JsString(w.toString))

  implicit lazy val TransactionJsonWrites: OWrites[Transaction] = OWrites(_.json())

  implicit lazy val logWrites: Writes[TraceStep] = Writes(_.json)

  implicit lazy val functionSignaturesWrites: Writes[FunctionSignatures] =
    (o: FunctionSignatures) =>
      Json.obj(
        "version"          -> o.version.toString,
        "callableFuncTypes" -> Json.obj(
          o.argsWithFuncName.map {
            case (functionName, args) =>
              val functionArgs: JsValueWrapper =
                args.map {
                  case (argName, argType) =>
                    Json.obj(
                      "name" -> argName,
                      "type" -> argType.name
                    )
                }
              functionName -> functionArgs
          }.toSeq*
        )
      )

}
