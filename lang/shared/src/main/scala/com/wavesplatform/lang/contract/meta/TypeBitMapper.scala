package com.wavesplatform.lang.contract.meta

import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, LIST, UNION}

import scala.util.Try

trait TypeBitMapper {
  def toIndex(t: FINAL): Either[String, Int]
  def fromIndex(i: Int): Either[String, FINAL]
  def length: Int
}

case class SingleTypeMapper(types: Seq[FINAL]) extends TypeBitMapper {
  if (types.size > 8) throw new Exception("Too big meta types list")

  override def toIndex(t: FINAL): Either[String, Int] =
    types.indexOf(t) match {
      case -1 => Left(s"Unexpected callable func arg type: $t")
      case i  => Right(1 << i)
    }

  override def fromIndex(i: Int): Either[String, FINAL] =
    Try(types(i)).toEither
      .leftMap(_ => s"Unexpected callable type absence for index=$i")

  override def length: Int =
    types.length
}

case class UnionTypeMapper(bitMapper: TypeBitMapper) extends TypeBitMapper {
  override def toIndex(t: FINAL): Either[String, Int] =
    t match {
      case UNION(types, _) => types.traverse(bitMapper.toIndex).map(_.sum)
      case other           => bitMapper.toIndex(other)
    }

  override def fromIndex(i: Int): Either[String, FINAL] =
    fromIndexRec(i, 0, Nil)

  private def fromIndexRec(i: Int, offset: Int, types: List[FINAL]): Either[String, FINAL] =
    if (i < 0)
      Left(s"Unexpected negative index=$i")
    else if (i == 0)
      constructType(types)
    else if (i % 2 == 1)
      bitMapper.fromIndex(offset).flatMap(t => fromIndexRec(i >> 1, offset + 1, t :: types))
    else
      fromIndexRec(i >> 1, offset + 1, types)

  private def constructType(foundTypes: List[FINAL]): Either[String, FINAL] =
    foundTypes match {
      case Nil           => Left("Unexpected callable func arg type absence")
      case single :: Nil => Right(single)
      case l             => Right(UNION.create(l))
    }

  override def length: Int =
    bitMapper.length
}

case class ListTypeMapper(bitMapper: TypeBitMapper) extends TypeBitMapper {
  override def toIndex(t: FINAL): Either[String, Int] =
    t match {
      case LIST(t) => bitMapper.toIndex(t).map(i => i + (1 << bitMapper.length))
      case t       => bitMapper.toIndex(t)
    }

  override def fromIndex(i: Int): Either[String, FINAL] =
    if ((i >> bitMapper.length) % 2 == 1)
      bitMapper.fromIndex(i ^ (1 << bitMapper.length)).map(LIST.apply)
    else
      bitMapper.fromIndex(i)

  override def length: Int =
    bitMapper.length + 1
}
