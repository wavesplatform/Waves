package com.wavesplatform.serialization

import com.wavesplatform.api.http.ApiError
import com.wavesplatform.lang.v1.compiler.Terms._
import play.api.libs.json.{JsObject, Json}

object ScriptValuesJson {
  def serializeValue(e: EVALUATED): JsObject = e match {
    case CONST_LONG(num)           => Json.obj("type" -> "Int", "value"         -> num)
    case CONST_BYTESTR(bs)         => Json.obj("type" -> "ByteVector", "value"  -> bs.toString)
    case CONST_STRING(str)         => Json.obj("type" -> "String", "value"      -> str)
    case CONST_BOOLEAN(b)          => Json.obj("type" -> "Boolean", "value"     -> b)
    case CONST_BIGINT(b)           => Json.obj("type" -> "BigInt", "value"      -> b)
    case CaseObj(caseType, fields) => Json.obj("type" -> caseType.name, "value" -> JsObject(fields.view.mapValues(serializeValue).toSeq))
    case ARR(xs)                   => Json.obj("type" -> "Array", "value"       -> xs.map(serializeValue))
    case FAIL(reason)              => Json.obj("type" -> "Fail", "error"        -> ApiError.ScriptExecutionError.Id, "message" -> reason)
  }
}
