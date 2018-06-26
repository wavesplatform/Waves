package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.evaluator.ctx.CaseObj
import scodec.bits.ByteVector

object Types {

  sealed trait TYPEPLACEHOLDER
  case class TYPEPARAM(char: Byte)             extends TYPEPLACEHOLDER
  case class LISTTYPEPARAM(t: TYPEPLACEHOLDER) extends TYPEPLACEHOLDER

  sealed trait TYPE extends TYPEPLACEHOLDER {
    type Underlying
    def name: String
    def fields: List[(String, TYPE)] = List()
    def l: List[PLAIN_TYPE]
    override def toString: String = name
  }
  sealed abstract class AUTO_TAGGED_TYPE[T](override val name: String) extends TYPE {
    override type Underlying = T
  }

  sealed trait PLAIN_TYPE extends TYPE {
    override val l = List(this)
  }

  case object NOTHING extends AUTO_TAGGED_TYPE[Nothing](name = "Nothing") {
    override val l = List()
  }
  case object UNIT       extends AUTO_TAGGED_TYPE[Unit](name = "Unit") with PLAIN_TYPE
  case object LONG       extends AUTO_TAGGED_TYPE[Long](name = "Int") with PLAIN_TYPE
  case object BYTEVECTOR extends AUTO_TAGGED_TYPE[ByteVector](name = "ByteVector") with PLAIN_TYPE
  case object BOOLEAN    extends AUTO_TAGGED_TYPE[Boolean](name = "Boolean") with PLAIN_TYPE
  case object STRING     extends AUTO_TAGGED_TYPE[String](name = "String") with PLAIN_TYPE

  case class LIST(innerType: TYPE) extends PLAIN_TYPE {
    type Underlying = IndexedSeq[innerType.Underlying]
    override lazy val name: String = "LIST(" ++ innerType.toString ++ ")"
  }
  case class CASETYPEREF(override val name: String, override val fields: List[(String, TYPE)]) extends AUTO_TAGGED_TYPE[CaseObj](name) with PLAIN_TYPE

  class UNION(override val l: List[PLAIN_TYPE]) extends AUTO_TAGGED_TYPE[CaseObj](name = "UNION(" ++ l.sortBy(_.toString).mkString("|") ++ ")") {
    override lazy val fields: List[(String, TYPE)] = l.map(_.fields.toSet).reduce(_ intersect _).toList
  }

  object UNION {
    def create(l: Seq[PLAIN_TYPE]): TYPE = {
      l.flatMap {
        case UNION(l) => l
        case t        => List(t)
      } match {
        case Seq(t) => t
        case l      => new UNION(l.distinct.toList)
      }
    }
    def apply(l: PLAIN_TYPE*): TYPE                 = create(l.toList)
    def unapply(u: UNION): Option[List[PLAIN_TYPE]] = Some(u.l)

    implicit class UnionExt(l1: TYPE) {
      def equivalent(l2: TYPE): Boolean = (l1, l2) match {
        case ((l1: UNION), (l2: UNION)) => l1.l.toSet == l2.l.toSet
        case (l1, l2)                   => l1 == l2
      }

      def >=(l2: TYPE): Boolean = (l1, l2) match {
        case ((l1: UNION), (l2: UNION)) =>
          val bigger = l1.l.toSet
          l2.l.forall(bigger.contains)
        case ((l1: UNION), l2) =>
          l1.l.contains(l2)
        case (l1, l2) => l1 == l2
      }
    }
  }

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
