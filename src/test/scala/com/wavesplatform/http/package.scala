package com.wavesplatform

import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher}
import play.api.libs.json.{JsValue, Reads}

import scala.reflect.ClassTag

package object http {
  import scala.language.implicitConversions

  def sameSignature(target: Array[Byte])(actual: Array[Byte]): Boolean = target sameElements actual

  implicit class JsFieldTypeChecker(val s: String) extends AnyVal {
    def ofType[A <: JsValue](implicit r: Reads[A], t: ClassTag[A]) = HavePropertyMatcher[JsValue, Any] { json =>
      val actualValue = (json \ s).validate[A]
      HavePropertyMatchResult(actualValue.isSuccess, s, t.runtimeClass.getSimpleName, (json \ s).get)
    }
  }

  implicit def tuple2ToHPM(v: (String, JsValue)): HavePropertyMatcher[JsValue, JsValue] =
    HavePropertyMatcher[JsValue, JsValue] { json =>
      val actualFieldValue = (json \ v._1).as[JsValue]
      HavePropertyMatchResult(actualFieldValue == v._2, v._1, v._2, actualFieldValue)
    }
}
