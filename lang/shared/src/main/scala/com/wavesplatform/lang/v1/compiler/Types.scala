package com.wavesplatform.lang.v1.compiler

object Types {

  sealed trait TYPEPLACEHOLDER
  sealed trait FINAL extends TYPEPLACEHOLDER with TYPE {
    def name: String
    override def toString: String = name
  }
  sealed trait PARAMETERIZED extends TYPEPLACEHOLDER
  sealed trait SINGLE        extends TYPEPLACEHOLDER { val l = List(this) }
  object TYPEPLACEHOLDER {
    case class TYPEPARAM(char: Byte)                 extends PARAMETERIZED with SINGLE
    case class PARAMETERIZEDLIST(t: TYPEPLACEHOLDER) extends PARAMETERIZED with SINGLE
    case class PARAMETERIZEDUNION(l: List[SINGLE])   extends PARAMETERIZED with TYPE
  }

  def typePlaceholdertoType(resultType: TYPEPLACEHOLDER, resolvedPlaceholders: Map[TYPEPLACEHOLDER.TYPEPARAM, TYPE]): TYPE = {
    resultType match {
      case tp: TYPEPLACEHOLDER.TYPEPARAM         => resolvedPlaceholders(tp)
      case TYPEPLACEHOLDER.PARAMETERIZEDUNION(l) => UNION.create(l.map(l => typePlaceholdertoType(l, resolvedPlaceholders)))
      case TYPEPLACEHOLDER.PARAMETERIZEDLIST(t)  => LIST(typePlaceholdertoType(t, resolvedPlaceholders))
      case c: FINAL                              => c
    }
  }

  def typeToConcretePlaceholder(t: TYPE): FINAL = t match {
    case u: UNION => u
    case NOTHING  => ??? // never happens
    case f: FINAL => f
  }

  sealed trait TYPE {
    def fields: List[(String, TYPE)] = List()
    def l: List[SINGLE]
  }

  case object NOTHING    extends FINAL with TYPE   { override val name = "Nothing"; override val l = List() }
  case object UNIT       extends FINAL with SINGLE { override val name = "Unit" }
  case object LONG       extends FINAL with SINGLE { override val name = "Int" }
  case object BYTEVECTOR extends FINAL with SINGLE { override val name = "ByteVector" }
  case object BOOLEAN    extends FINAL with SINGLE { override val name = "Boolean" }
  case object STRING     extends FINAL with SINGLE { override val name = "String" }
  case class LIST(innerType: TYPE with FINAL) extends FINAL with SINGLE {
    override lazy val name: String = "LIST(" ++ innerType.toString ++ ")"
  }
  case class CASETYPEREF(override val name: String, override val fields: List[(String, TYPE)]) extends FINAL with SINGLE

  case class UNION(override val l: List[FINAL with SINGLE]) extends TYPE with FINAL {
    override lazy val fields: List[(String, TYPE)] = l.map(_.fields.toSet).reduce(_ intersect _).toList
    override val name                              = "UNION(" ++ l.sortBy(_.toString).mkString("|") ++ ")"
  }

  object UNION {
    def create(l: Seq[TYPE]): UNION = {
      UNION(l.flatMap {
        case NOTHING              => List.empty
        case UNION(inner)         => inner
        case s: FINAL with SINGLE => List(s)
      }.toList)
    }
    def apply(l: TYPE*): TYPE = create(l.toList)

  }

  implicit class TypeExt(l1: TYPE) {
    def equivalent(l2: TYPE): Boolean = (l1, l2) match {
      case ((l1: UNION), (l2: UNION)) => l1.l.toSet == l2.l.toSet
      case (l1, l2)                   => UNION.create(Seq(l1)) equivalent UNION.create(Seq(l2))
    }

    def >=(l2: TYPE): Boolean = (l1, l2) match {
      case ((l1: UNION), (l2: UNION)) =>
        val bigger = l1.l.toSet
        l2.l.forall(bigger.contains)
      case (_, NOTHING) => true
      case (NOTHING, _) => false
      case (l1, l2)     => UNION.create(Seq(l1)) >= UNION.create(Seq(l2))
    }
  }
}
