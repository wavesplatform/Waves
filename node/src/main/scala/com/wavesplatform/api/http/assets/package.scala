package com.wavesplatform.api.http

import play.api.libs.json._
import supertagged.TaggedType

package object assets {

  object ProofStr extends TaggedType[String]
  type ProofStr = ProofStr.Type

  implicit object MaybeStringReads extends Reads[ProofStr] {
    override def reads(json: JsValue): JsResult[ProofStr] = {
      json match {
        case JsNull      => JsSuccess(ProofStr(""))
        case JsString(s) => JsSuccess(ProofStr(s))
        case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsstring"))))
      }
    }
  }
}
