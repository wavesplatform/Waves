package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.evaluator.ctx.{CaseObj, CaseType}
import scodec.bits.ByteVector

object Types {

  sealed trait TYPEPLACEHOLDER
  case class TYPEPARAM(char: Byte)               extends TYPEPLACEHOLDER
//  case class OPTIONTYPEPARAM(t: TYPEPLACEHOLDER) extends TYPEPLACEHOLDER
  case class LISTTYPEPARAM(t: TYPEPLACEHOLDER)   extends TYPEPLACEHOLDER

  sealed trait TYPE extends TYPEPLACEHOLDER {
    type Underlying
    def name: String = ???
    def fields: List[(String, TYPE)] = List()
    def l: List[TYPE] = List(this)
    override def toString: String = name
  }
  sealed abstract class AUTO_TAGGED_TYPE[T] extends TYPE {
    override type Underlying = T
  }

  case object NOTHING    extends AUTO_TAGGED_TYPE[Nothing] { override val name = "Nothing" }
  case object UNIT       extends AUTO_TAGGED_TYPE[Unit] { override val name = "Unit" }
  case object LONG       extends AUTO_TAGGED_TYPE[Long] { override val name = "Int" }
  case object BYTEVECTOR extends AUTO_TAGGED_TYPE[ByteVector]  { override val name = "ByteVector" }
  case object BOOLEAN    extends AUTO_TAGGED_TYPE[Boolean] { override val name = "Boolean" }
  case object STRING     extends AUTO_TAGGED_TYPE[String] { override val name = "String" }
//  case class OPTION(innerType: TYPE) extends TYPE {
//    type Underlying = Option[innerType.Underlying]
//    override def toString: String = "OPTION("++innerType.toString++")"
//  }
  case class LIST(innerType: TYPE) extends TYPE {
    type Underlying = IndexedSeq[innerType.Underlying]
    override def toString: String = "LIST("++innerType.toString++")"
  }
  case class CASETYPEREF(override val name: String, override val fields: List[(String, TYPE)])   extends AUTO_TAGGED_TYPE[CaseObj]
  class UNION(override val l: List[TYPE]) extends AUTO_TAGGED_TYPE[CaseObj] {
    override lazy val fields: List[(String, TYPE)] = l.map(_.fields.toSet).reduce(_ intersect _).toList
    override def toString: String = "UNION("++l.sortBy(_.toString).mkString("|")++")"
  }
  object UNION {
    def of(l: CaseType*): TYPE = UNION.create(l.map(_.typeRef).toList)

    def create(l: Seq[TYPE]): TYPE = l.flatMap {
       case UNION(l) => l
       case t => List(t)
    } match {
       case Seq(t) => t
       case l => new UNION(l.distinct.toList)
    }
    def apply(l: TYPE*): TYPE = create(l.toList)
    def unapply(u: UNION): Option[List[TYPE]] = Some(u.l)

    implicit class UnionExt(l1: TYPE) {
      def equivalent(l2: TYPE): Boolean = (l1, l2) match {
        case ((l1: UNION),(l2: UNION)) => l1.l.toSet == l2.l.toSet
        case (l1, l2) => l1 == l2
      }

      def >=(l2: TYPE): Boolean = (l1, l2) match {
        case ((l1: UNION),(l2: UNION)) =>
           val bigger = l1.l.toSet
           l2.l.forall(bigger.contains)
        case ((l1: UNION), l2) =>
           l1.l.contains(l2)
        case (l1, l2) => l1 == l2
      }
    }
  }

  def asNonable(typeName: TYPEPARAM)(t: Map[TYPEPARAM, TYPE]): Either[String, TYPE] = Right(UNION.create(List(t(typeName), UNIT)))
  def fromNonable(typeName: TYPEPARAM)(t: Map[TYPEPARAM, TYPE]): Either[String, TYPE] = Right(t(typeName) match {
    case UNION(l) => l.filter(_ != UNIT) match {
       case Seq(t) => t
       case l => new UNION(l)
    }
    case t => t
  })
  def canBeEq(type1: TYPEPARAM, type2: TYPEPARAM)(t: Map[TYPEPARAM, TYPE]) = {
    (t(type1), t(type2)) match {
      case (UNION(l1), UNION(l2)) =>
        Either.cond(l1.exists(l2.toSet), BOOLEAN, "Comparing values have incompatible types")
      case (UNION(l), t) =>
        Either.cond(l.contains(t), BOOLEAN, "Comparing values have incompatible types")
      case (t, UNION(l)) =>
        Either.cond(l.contains(t), BOOLEAN, "Comparing values have incompatible types")
      case (NOTHING, _) => Right(BOOLEAN)
      case (_, NOTHING) => Right(BOOLEAN)
      case (t1, t2) =>
        Either.cond(t1 == t1, BOOLEAN, "Comparing values have incompatible types")
    }
  }
}
