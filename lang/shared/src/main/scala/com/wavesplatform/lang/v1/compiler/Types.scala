package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.evaluator.ctx.DefinedType

object Types {

  sealed trait TYPEPLACEHOLDER
  sealed trait CONCRETE      extends TYPEPLACEHOLDER
  sealed trait PARAMETERIZED extends TYPEPLACEHOLDER
  sealed trait SINGLE        extends TYPEPLACEHOLDER
  object TYPEPLACEHOLDER {
    case class TYPEPARAM(char: Byte)                 extends PARAMETERIZED with SINGLE
    case class PARAMETERIZEDLIST(t: TYPEPLACEHOLDER) extends PARAMETERIZED with SINGLE
    case class PARAMETERIZEDUNION(l: List[SINGLE])   extends PARAMETERIZED
    case class LIST(t: CONCRETE)                     extends CONCRETE with SINGLE
    case class UNION(l: List[CONCRETE with SINGLE])  extends CONCRETE
  }

  def typePlaceholdertoType(resultType: TYPEPLACEHOLDER,
                            resolvedPlaceholders: Map[TYPEPLACEHOLDER.TYPEPARAM, TYPE],
                            knownTypes: Map[String, DefinedType]): TYPE = {
    resultType match {
      case tp: TYPEPLACEHOLDER.TYPEPARAM         => resolvedPlaceholders(tp)
      case TYPEPLACEHOLDER.PARAMETERIZEDUNION(l) => UNION.create(l.map(l => typePlaceholdertoType(l, resolvedPlaceholders, knownTypes)))
      case TYPEPLACEHOLDER.PARAMETERIZEDLIST(t)  => LIST(typePlaceholdertoType(t, resolvedPlaceholders, knownTypes))
      case c: CONCRETE                           => concreteToType(c, knownTypes)
    }
  }

  def concreteToType(concrete: CONCRETE, knownTypes: Map[String, DefinedType]): TYPE = concrete match {
    case TYPEPLACEHOLDER.LIST(t: CONCRETE) => LIST(concreteToType(t, knownTypes))
    case UNIT                              => UNIT
    case LONG                              => LONG
    case BYTEVECTOR                        => BYTEVECTOR
    case BOOLEAN                           => BOOLEAN
    case STRING                            => STRING
    case c: CASETYPEREF                    => c
    case TYPEPLACEHOLDER.UNION(l)          => UNION.create(l.flatMap(l1 => concreteToType(l1, knownTypes).l))

  }

  def f(i: SINGLE_TYPE): CONCRETE with SINGLE = i match {
    case UNIT           => UNIT
    case LONG           => LONG
    case BYTEVECTOR     => BYTEVECTOR
    case BOOLEAN        => BOOLEAN
    case STRING         => STRING
    case LIST(lt)       => TYPEPLACEHOLDER.LIST(typeToConcretePlaceholder(lt))
    case c: CASETYPEREF => c
  }

  def typeToConcretePlaceholder(t: TYPE): CONCRETE = t match {
    case UNION(l)            => TYPEPLACEHOLDER.UNION(l map f)
    case NOTHING             => ??? // never happens
    case single: SINGLE_TYPE => f(single)
  }

  sealed trait TYPE {
    def name: String
    def fields: List[(String, TYPE)] = List()
    def l: List[SINGLE_TYPE]
    override def toString: String = name
  }

  case object NOTHING                                                                          extends TYPE { override val name = "Nothing"; override val l = List() }
  sealed trait SINGLE_TYPE                                                                     extends TYPE { override val l = List(this) }
  case object UNIT                                                                             extends SINGLE_TYPE with CONCRETE with SINGLE { override val name = "Unit" }
  case object LONG                                                                             extends SINGLE_TYPE with CONCRETE with SINGLE { override val name = "Int" }
  case object BYTEVECTOR                                                                       extends SINGLE_TYPE with CONCRETE with SINGLE { override val name = "ByteVector" }
  case object BOOLEAN                                                                          extends SINGLE_TYPE with CONCRETE with SINGLE { override val name = "Boolean" }
  case object STRING                                                                           extends SINGLE_TYPE with CONCRETE with SINGLE { override val name = "String" }
  case class LIST(innerType: TYPE)                                                             extends SINGLE_TYPE { override lazy val name: String = "LIST(" ++ innerType.toString ++ ")" }
  case class CASETYPEREF(override val name: String, override val fields: List[(String, TYPE)]) extends SINGLE_TYPE with CONCRETE with SINGLE

  case class UNION(override val l: List[SINGLE_TYPE]) extends TYPE {
    override lazy val fields: List[(String, TYPE)] = l.map(_.fields.toSet).reduce(_ intersect _).toList
    override val name                              = "UNION(" ++ l.sortBy(_.toString).mkString("|") ++ ")"
  }

  object UNION {
    def create(l: Seq[TYPE]): UNION = {
      UNION(l.flatMap {
        case NOTHING        => List.empty
        case UNION(inner)   => inner
        case s: SINGLE_TYPE => List(s)
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
