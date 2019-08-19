package com.wavesplatform.lang.v1.compiler

object Types {

  sealed trait TYPE
  sealed trait SINGLE extends TYPE
  sealed trait REAL   extends FINAL with SINGLE
  sealed trait FINAL extends TYPE {
    def fields: List[(String, FINAL)] = List()
    def typeList: List[REAL]
    def union: UNION = UNION(typeList)
    def name: String
    override def toString: String = name
  }
  sealed trait PARAMETERIZED extends TYPE

  case class TYPEPARAM(char: Byte)               extends PARAMETERIZED with SINGLE { override def toString: String = char.toChar.toString }
  case class PARAMETERIZEDLIST(t: TYPE)          extends PARAMETERIZED with SINGLE { override def toString: String = s"List[$t]" }
  case class PARAMETERIZEDUNION(l: List[SINGLE]) extends PARAMETERIZED             { override def toString: String = l.mkString("|")}
  case object NOTHING                            extends FINAL { override val name = "Nothing"; override val typeList = List() }
  case object LONG                               extends REAL { override val name = "Int"; override val typeList = List(this) }
  case object BYTESTR                            extends REAL { override val name = "ByteVector"; override val typeList = List(this) }
  case object BOOLEAN                            extends REAL { override val name = "Boolean"; override val typeList = List(this) }
  case object STRING                             extends REAL { override val name = "String"; override val typeList = List(this) }
  case class LIST(innerType: FINAL) extends REAL {
    override lazy val name: String    = "List[" ++ innerType.toString ++ "]"
    override def typeList: List[REAL] = List(this)
  }
  case class UNION(override val typeList: List[REAL], n: Option[String] = None) extends FINAL {
    override lazy val fields = typeList.map(_.fields.toSet).reduce(_ intersect _).toList
    override val name        = if (n.nonEmpty) n.get else typeList.sortBy(_.toString).mkString("|")
  }

  case class CASETYPEREF(override val name: String, override val fields: List[(String, FINAL)]) extends REAL {
    override def typeList: List[REAL] = List(this)
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
    def create(l: Seq[FINAL], n: Option[String] = None): UNION = {
      UNION(l.flatMap {
                case NOTHING         => List.empty
                case UNION(inner, _) => inner
                case s: REAL         => List(s)
              }
              .toList
              .distinct,
            n)
    }
    def apply(l: REAL*): UNION = create(l.toList)

    def reduce(u: UNION): FINAL = u.typeList match {
      case Nil      => throw new Exception("Empty union")
      case x :: Nil => x
      case _        => u
    }
  }

  implicit class TypeExt(l1: FINAL) {
    def equivalent(l2: FINAL): Boolean = (l1, l2) match {
      case (l1: UNION, l2: UNION) => l1.typeList.toSet == l2.typeList.toSet
      case (l1: FINAL, l2: FINAL) => l1.union equivalent l2.union
    }

    def >=(l2: FINAL): Boolean = (l1, l2) match {
      case (l1: UNION, l2: UNION) =>
        val bigger = l1.typeList.toSet
        l2.typeList.forall(bigger.contains)
      case (_, NOTHING)           => true
      case (NOTHING, _)           => false
      case (l1: FINAL, l2: FINAL) => l1.union >= l2.union
    }

    def <=(l2: FINAL): Boolean = l2 >= l1
  }

  val UNIT: CASETYPEREF    = CASETYPEREF("Unit", List.empty)
  val optionByteVector     = UNION(BYTESTR, UNIT)
  val optionLong           = UNION(LONG, UNIT)
  val listByteVector: LIST = LIST(BYTESTR)
  val listString: LIST     = LIST(STRING)

  val nativeTypeList = List(
    "Int",
    "ByteVector",
    "Boolean",
    "String"
  )
}
