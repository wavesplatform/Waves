package com.wavesplatform.lang.v1.compiler

object Types {

  sealed trait TYPE
  sealed trait SINGLE extends TYPE
  sealed trait REAL   extends FINAL with SINGLE
  sealed trait FINAL extends TYPE {
    def fields: List[(String, FINAL)] = List()
    def l: List[REAL]
    def u: UNION = UNION(l)
    def name: String
    override def toString: String = name
  }
  sealed trait PARAMETERIZED extends TYPE

  case class TYPEPARAM(char: Byte)               extends PARAMETERIZED with SINGLE
  case class PARAMETERIZEDLIST(t: TYPE)          extends PARAMETERIZED with SINGLE
  case class PARAMETERIZEDUNION(l: List[SINGLE]) extends PARAMETERIZED
  case object NOTHING                            extends FINAL { override val name = "Nothing"; override val l = List() }
  case object LONG                               extends REAL { override val name = "Int"; override val l = List(this) }
  case object BYTESTR                            extends REAL { override val name = "ByteVector"; override val l = List(this) }
  case object BOOLEAN                            extends REAL { override val name = "Boolean"; override val l = List(this) }
  case object STRING                             extends REAL { override val name = "String"; override val l = List(this) }
  case class LIST(innerType: FINAL) extends REAL {
    override lazy val name: String = "LIST(" ++ innerType.toString ++ ")"
    override def l: List[REAL]     = List(this)
  }
  case class UNION(override val l: List[REAL]) extends FINAL {
    override lazy val fields = l.map(_.fields.toSet).reduce(_ intersect _).toList
    override val name        = "UNION(" ++ l.sortBy(_.toString).mkString("|") ++ ")"
  }

  case class CASETYPEREF(override val name: String, override val fields: List[(String, FINAL)]) extends REAL {
    override def l: List[REAL] = List(this)
  }

  def toFinal(resultType: TYPE, resolvedPlaceholders: Map[TYPEPARAM, FINAL]): FINAL = {
    resultType match {
      case NOTHING               => NOTHING
      case tp: TYPEPARAM         => resolvedPlaceholders(tp)
      case PARAMETERIZEDUNION(l) => UNION.create(l.map(l => toFinal(l, resolvedPlaceholders)))
      case PARAMETERIZEDLIST(t)  => LIST(toFinal(t, resolvedPlaceholders))
      case c: FINAL              => c
    }
  }

  object UNION {
    def create(l: Seq[FINAL]): UNION = {
      UNION(l.flatMap {
        case NOTHING      => List.empty
        case UNION(inner) => inner
        case s: REAL      => List(s)
      }.toList.distinct)
    }
    def apply(l: REAL*): UNION = create(l.toList)

    def reduce(u: UNION): FINAL = u.l match {
      case Nil      => throw new Exception("Empty union")
      case x :: Nil => x
      case _        => u
    }
  }

  implicit class TypeExt(l1: FINAL) {
    def equivalent(l2: FINAL): Boolean = (l1, l2) match {
      case (l1: UNION, l2: UNION) => l1.l.toSet == l2.l.toSet
      case (l1: FINAL, l2: FINAL) => l1.u equivalent l2.u
    }

    def >=(l2: FINAL): Boolean = (l1, l2) match {
      case (l1: UNION, l2: UNION) =>
        val bigger = l1.l.toSet
        l2.l.forall(bigger.contains)
      case (_, NOTHING)           => true
      case (NOTHING, _)           => false
      case (l1: FINAL, l2: FINAL) => l1.u >= l2.u
    }

    def <=(l2: FINAL): Boolean = l2 >= l1
  }

  val UNIT: CASETYPEREF = CASETYPEREF("Unit", List.empty)
  val optionByteVector = UNION(BYTESTR, UNIT)
  val optionLong           = UNION(LONG, UNIT)
  val listByteVector: LIST = LIST(BYTESTR)
  val listString: LIST = LIST(STRING)
}
