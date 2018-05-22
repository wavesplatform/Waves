package com.wavesplatform.lang

import cats.implicits._
import com.wavesplatform.lang.v1.evaluator.ctx.{AnyObj, CaseObj, Obj}
import scodec.bits.ByteVector

import scala.reflect.ClassTag

final case class TypeInfo[T](classTag: ClassTag[T], superClass: Option[TypeInfo[_]], typeParams: Set[TypeInfo[_]], interfaces: Set[TypeInfo[_]]) {
  self =>
  def <:<(other: TypeInfo[_]) = {
    def loop(left: Set[TypeInfo[_]], seen: Set[TypeInfo[_]]): Boolean = {
      left.nonEmpty && {
        val next   = left.head
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

  override def toString: ExecutionError = classTag.runtimeClass.getSimpleName + (if (typeParams.isEmpty) "" else s"[${typeParams.mkString(",")}]")
}

object TypeInfo {
  def typeInfo[T](implicit ti: TypeInfo[T]): TypeInfo[T] = ti

  private def fromAny[A: ClassTag](typeParams: Set[TypeInfo[_]] = Set.empty, interfaces: Set[TypeInfo[_]] = Set.empty): TypeInfo[A] = {
    TypeInfo(reflect.classTag[A], anyTypeInfo.some, typeParams, interfaces)
  }

  private def fromAnyVal[A: ClassTag](typeParams: Set[TypeInfo[_]] = Set.empty, interfaces: Set[TypeInfo[_]] = Set.empty): TypeInfo[A] = {
    TypeInfo(reflect.classTag[A], anyValTypeInfo.some, typeParams, interfaces)
  }

  implicit val anyTypeInfo: TypeInfo[Any] = {
    val tag = reflect.classTag[Any]
    TypeInfo(tag, None, Set.empty, Set.empty)
  }

  implicit val equalsTypeInfo: TypeInfo[Equals] =
    fromAny()

  implicit val serializableTypeInfo: TypeInfo[Serializable] =
    fromAny()

  implicit val productTypeInfo: TypeInfo[Product] =
    fromAny(interfaces = Set(serializableTypeInfo))

  implicit val anyValTypeInfo: TypeInfo[AnyVal] =
    fromAny()

  implicit val nothingTypeInfo: TypeInfo[Nothing] = {
    val tag = reflect.classTag[Nothing]
    TypeInfo[Nothing](
      tag,
      None,
      Set.empty,
      Set.empty
    )
  }

  implicit val unitTypeInfo: TypeInfo[Unit] =
    fromAnyVal()
  implicit val longTypeInfo: TypeInfo[Long] =
    fromAnyVal()

  /**
    * BitwiseOperations also should be in interfaces,
    * but i dont know how to create TypeInfo with
    * recursive type parameters.
    */
  implicit val byteVectorTypeInfo: TypeInfo[ByteVector] =
    fromAny(interfaces = Set(serializableTypeInfo))

  implicit val booleanTypeInfo: TypeInfo[Boolean] =
    fromAnyVal()

  implicit val stringTypeInfo: TypeInfo[String] =
    fromAnyVal(interfaces = Set(serializableTypeInfo))

  implicit val objTypeInfo: TypeInfo[Obj] =
    fromAny(interfaces = Set(productTypeInfo, serializableTypeInfo))

  implicit val caseObjTypeInfo: TypeInfo[CaseObj] =
    fromAny(interfaces = Set(productTypeInfo, serializableTypeInfo))

  implicit val anyObjTypeInfo: TypeInfo[AnyObj] =
    fromAny(interfaces = Set(productTypeInfo, serializableTypeInfo))

  implicit def optionTypeInfo[A](implicit tia: TypeInfo[A]): TypeInfo[Option[A]] =
    fromAny(Set(tia), Set(productTypeInfo, serializableTypeInfo))

  implicit def listTypeInfo[A](implicit tia: TypeInfo[A]): TypeInfo[IndexedSeq[A]] =
    fromAny(Set(tia), Set(productTypeInfo, serializableTypeInfo))
}
