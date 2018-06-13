package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.evaluator.ctx.{CaseObj, CaseType}
import scodec.bits.ByteVector

object Types {

  sealed trait TYPEPLACEHOLDER
  case class TYPEPARAM(char: Byte)               extends TYPEPLACEHOLDER
  case class OPTIONTYPEPARAM(t: TYPEPLACEHOLDER) extends TYPEPLACEHOLDER
  case class LISTTYPEPARAM(t: TYPEPLACEHOLDER)   extends TYPEPLACEHOLDER

  sealed trait TYPE extends TYPEPLACEHOLDER {
    type Underlying
    def name: String = ???
    def fields: List[(String, TYPE)] = List()
    def l: List[TYPE] = List(this)
  }
  sealed abstract class AUTO_TAGGED_TYPE[T] extends TYPE {
    override type Underlying = T
  }

  case object NOTHING    extends AUTO_TAGGED_TYPE[Nothing]
  case object UNIT       extends AUTO_TAGGED_TYPE[Unit]
  case object LONG       extends AUTO_TAGGED_TYPE[Long]
  case object BYTEVECTOR extends AUTO_TAGGED_TYPE[ByteVector]
  case object BOOLEAN    extends AUTO_TAGGED_TYPE[Boolean]
  case object STRING     extends AUTO_TAGGED_TYPE[String]
  case class OPTION(innerType: TYPE) extends TYPE {
    type Underlying = Option[innerType.Underlying]
  }
  case class LIST(innerType: TYPE) extends TYPE {
    type Underlying = IndexedSeq[innerType.Underlying]
  }
  case class CASETYPEREF(override val name: String, override val fields: List[(String, TYPE)])   extends AUTO_TAGGED_TYPE[CaseObj]
  class UNION(override val l: List[TYPE]) extends AUTO_TAGGED_TYPE[CaseObj] {
    override lazy val fields: List[(String, TYPE)] = l.map(_.fields.toSet).reduce(_ intersect _).toList
  }
  object UNION {
    def of(l: CaseType*): TYPE = UNION.create(l.map(_.typeRef).toList)

    def create(l: Seq[TYPE]): TYPE = l.flatMap {
       case UNION(l) => l
       case t => List(t)
    } match {
       case Seq(t) => t
       case l => new UNION(l.toList)
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
}
