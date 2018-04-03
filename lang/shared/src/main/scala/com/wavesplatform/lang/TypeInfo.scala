package com.wavesplatform.lang

import cats.implicits._
import com.wavesplatform.lang.ctx.Obj
import scodec.bits.ByteVector

import scala.reflect.ClassTag

final case class TypeInfo[T](classTag: ClassTag[T],
                             superClass: Option[TypeInfo[_]],
                             typeParams: Set[TypeInfo[_]],
                             interfaces: Set[TypeInfo[_]]) { self =>
  def <:< (other: TypeInfo[_]) = {
    def loop(left: Set[TypeInfo[_]], seen: Set[TypeInfo[_]]): Boolean = {
      left.nonEmpty && {
        val next = left.head
        val supers = next.interfaces ++ next.superClass
        supers(other) || {
          val xs = left ++ supers filterNot seen
          loop(xs - next, seen + next)
        }
      }
    }

    if (self.classTag.runtimeClass == other.classTag.runtimeClass) {
      self.typeParams == other.typeParams
    } else loop(Set(self), Set.empty)
  }
}

object TypeInfo {
  def typeInfo[T](implicit ti: TypeInfo[T]): TypeInfo[T] = ti
  implicit val anyTypeInfo: TypeInfo[Any] = {
    val tag = reflect.classTag[Any]
    TypeInfo(tag, None, Set.empty, Set.empty)
  }
  implicit val equalsTypeInfo: TypeInfo[Equals] = {
    val tag = reflect.classTag[Equals]
    TypeInfo(tag, anyTypeInfo.some, Set.empty, Set.empty)
  }
  implicit val serializableTypeInfo: TypeInfo[Serializable] = {
    val tag = reflect.classTag[Serializable]
    TypeInfo(tag, anyTypeInfo.some, Set.empty, Set.empty)
  }
  implicit val productTypeInfo: TypeInfo[Product] = {
    val tag = reflect.classTag[Product]
    TypeInfo(tag, anyTypeInfo.some, Set.empty, Set(equalsTypeInfo))
  }
  implicit val anyValTypeInfo: TypeInfo[AnyVal] = {
    val tag = reflect.classTag[AnyVal]
    TypeInfo(tag, anyTypeInfo.some, Set.empty, Set.empty)
  }
  implicit val nothingTypeInfo: TypeInfo[Nothing] = {
    val tag = reflect.classTag[Nothing]
    TypeInfo[Nothing](
      tag,
      None,
      Set.empty,
      Set.empty
    )
  }
  implicit val unitTypeInfo: TypeInfo[Unit] = {
    val tag = reflect.classTag[Unit]
    TypeInfo(
      tag,
      anyValTypeInfo.some,
      Set.empty,
      Set.empty
    )
  }
  implicit val longTypeInfo: TypeInfo[Long] = {
    val tag = reflect.classTag[Long]
    TypeInfo(
      tag,
      anyValTypeInfo.some,
      Set.empty,
      Set.empty
    )
  }
  implicit val byteVectorTypeInfo: TypeInfo[ByteVector] = {
    val tag = reflect.classTag[ByteVector]
    TypeInfo(
      tag,
      None, //oh, shii~
      Set.empty,
      Set(serializableTypeInfo)
    )
  }

  implicit val booleanTypeInfo: TypeInfo[Boolean] = {
    val tag = reflect.classTag[Boolean]
    TypeInfo(
      tag,
      anyValTypeInfo.some,
      Set.empty,
      Set.empty
    )
  }

  implicit val stringTypeInfo: TypeInfo[String] = {
    val tag = reflect.classTag[String]
    TypeInfo(
      tag,
      anyValTypeInfo.some,
      Set.empty,
      Set(serializableTypeInfo)
    )
  }

  implicit val objTypeInfo: TypeInfo[Obj] = {
    val tag = reflect.classTag[Obj]
    TypeInfo(
      tag,
      anyTypeInfo.some,
      Set.empty,
      Set(productTypeInfo, serializableTypeInfo)
    )
  }

  implicit def optionTypeInfo[A](implicit tia: TypeInfo[A]): TypeInfo[Option[A]] = {
    TypeInfo(
      reflect.classTag[Option[A]],
      anyTypeInfo.some,
      Set(tia),
      Set(productTypeInfo, serializableTypeInfo)
    )
  }
}
