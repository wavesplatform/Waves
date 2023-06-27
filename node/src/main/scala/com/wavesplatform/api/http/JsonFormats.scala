package com.wavesplatform.api.http

import com.wavesplatform.account.Address
import com.wavesplatform.lang.contract.meta.FunctionSignatures
import com.wavesplatform.transaction.Transaction
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

trait JsonFormats {
  implicit lazy val wavesAddressWrites: Writes[Address] = Writes(w => JsString(w.toString))

  implicit lazy val TransactionJsonWrites: OWrites[Transaction] = OWrites(_.json())

  implicit lazy val functionSignaturesWrites: Writes[FunctionSignatures] =
    (o: FunctionSignatures) =>
      Json.obj(
        "version" -> o.version.toString,
        "callableFuncTypes" -> Json.obj(
          o.argsWithFuncName.map { case (functionName, args) =>
            val functionArgs: JsValueWrapper =
              args.map { case (argName, argType) =>
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
