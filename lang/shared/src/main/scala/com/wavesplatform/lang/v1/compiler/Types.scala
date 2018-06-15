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
  case class CASETYPEREF(name: String)   extends AUTO_TAGGED_TYPE[CaseObj]
  case class UNION(l: List[CASETYPEREF]) extends AUTO_TAGGED_TYPE[CaseObj]
  object UNION {

    def of(l: CaseType*): UNION = UNION(l.map(_.typeRef).toList)

    def apply(l: CASETYPEREF*): UNION = new UNION(l.toList)

    implicit class UnionExt(l1: UNION) {
      def equivalent(l2: UNION): Boolean = l1.l.toSet == l2.l.toSet

      def >=(l2: UNION): Boolean = {
        val bigger = l1.l.toSet
        l2.l.forall(bigger.contains)
      }
    }
  }
}
