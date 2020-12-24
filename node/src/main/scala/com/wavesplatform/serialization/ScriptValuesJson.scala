package com.wavesplatform.serialization

import com.wavesplatform.api.http.ApiError
import play.api.libs.json.{JsObject, Json}

object ScriptValuesJson {
  import com.wavesplatform.lang.v1.compiler.Terms
  def serializeValue(e: Terms.EVALUATED): JsObject = e match {
    case Terms.CONST_LONG(num)   => Json.obj("type" -> "Int", "value"        -> num)
    case Terms.CONST_BYTESTR(bs) => Json.obj("type" -> "ByteVector", "value" -> bs.toString)
    case Terms.CONST_STRING(str) => Json.obj("type" -> "String", "value"     -> str)
    case Terms.CONST_BOOLEAN(b)  => Json.obj("type" -> "Boolean", "value"    -> b)
    case Terms.CaseObj(caseType, fields) =>
      Json.obj("type" -> caseType.name, "value" -> JsObject(fields.view.mapValues(serializeValue).toSeq))
    case Terms.ARR(xs)      => Json.obj("type" -> "Array", "value" -> xs.map(serializeValue))
    case Terms.FAIL(reason) => Json.obj("type" -> "Fail", "error"  -> ApiError.ScriptExecutionError.Id, "message" -> reason)
  }
}
