package com.wavesplatform.lang.v1.compiler

import cats.implicits._

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
  case class PARAMETERIZEDTUPLE(t: List[TYPE])   extends PARAMETERIZED             { override def toString: String = t.mkString("(", ", ", ")") }
  case object NOTHING                            extends REAL { override val name = "Nothing"; override val typeList = List() }
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

    override def equals(obj: Any): Boolean =
      obj match {
        case UNION(typeList, _) if typeList.sortBy(_.toString) == this.typeList.sortBy(_.toString) => true
        case _ => false
      }

    def unfold: UNION = {
      val unfolded = typeList.flatMap {
        case t: TUPLE =>
          t.unfold match {
            case u: UNION => u.unfold.typeList
            case single   => List(single)
          }
        case single => List(single)
      }
      UNION.create(unfolded)
    }
  }

  case class TUPLE(types: List[FINAL]) extends REAL {
    override def name: String = types.mkString("(", ", ", ")")
    override def typeList: List[REAL] = List(this)

    override def fields: List[(String, FINAL)] =
      types.mapWithIndex { case (t, i) => s"_${i + 1}" -> t }

    /*
        (A1 | ... | An, ..., Z1 | ... | Zk)
                         ||
                         \/
        (A1, ..., Z1) | ... | (A1, ..., Zk) | ... | (An, ..., Zk)
    */
    def unfold: FINAL = {
      def combine(accTypes: List[TUPLE], nextTypes: List[REAL]): List[TUPLE] =
        if (accTypes.isEmpty)
          nextTypes.map(t => TUPLE(List(t)))
        else
          for {
            a <- accTypes
            b <- nextTypes
          } yield TUPLE(a.types :+ b)

      UNION.reduce(UNION.create(
        types.map(_.typeList).foldLeft(List.empty[TUPLE])(combine)
      ))
    }
  }

  case class CASETYPEREF(override val name: String, override val fields: List[(String, FINAL)], hideConstructor: Boolean = false) extends REAL {
    override def typeList: List[REAL] = List(this)
  }

  def toFinal(resultType: TYPE, resolvedPlaceholders: Map[TYPEPARAM, FINAL]): FINAL = {
    resultType match {
      case NOTHING               => NOTHING
      case tp: TYPEPARAM         => resolvedPlaceholders(tp)
      case PARAMETERIZEDUNION(l) => UNION.create(l.map(l => toFinal(l, resolvedPlaceholders)))
      case PARAMETERIZEDLIST(t)  => LIST(toFinal(t, resolvedPlaceholders))
      case PARAMETERIZEDTUPLE(t) => TUPLE(t.map(toFinal(_, resolvedPlaceholders)))
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
    def apply(l: FINAL*): UNION = create(l.toList)

    def reduce(u: UNION): FINAL = u.typeList match {
      case Nil      => throw new Exception("Empty union")
      case x :: Nil => x
      case _        => u
    }
  }

  implicit class TypeExt(l1: FINAL) {
    def equivalent(l2: FINAL): Boolean = (l1, l2) match {
      case (l1: TUPLE, l2: TUPLE) =>
        (l1.types.length == l2.types.length) && {
        val unfolded = l1.unfold
        if (l1 == unfolded)
          (l1.types zip l2.types).forall { case (t1, t2) => t1 equivalent t2 }
        else
          unfolded equivalent l2.unfold
    }
      case (l1: REAL, l2: REAL)   => l1 == l2
      case (l1: UNION, l2: UNION) =>
          l1.typeList.length == l2.typeList.length &&
          (l1.unfold.typeList.sortBy(_.name) zip l2.unfold.typeList.sortBy(_.name))
            .forall { case (t1, t2) => t1 equivalent t2 }
      case (l1: FINAL, l2: FINAL) => l1.union equivalent l2.union
    }

    def >=(l2: FINAL): Boolean = (l1, l2) match {
      case (l1: UNION, l2: UNION) =>
        val bigger = l1.typeList.toSet
        l2.typeList.forall(bigger.contains)
      case (_, NOTHING)           => true
      case (NOTHING, _)           => false
      case (LIST(t1), LIST(t2))   => t1 >= t2
      case (TUPLE(types1), TUPLE(types2)) =>
        types1.length == types2.length && (types1 zip types2).forall { case (t1, t2) => t1 >= t2 }
      case (l1: FINAL, l2: FINAL) => l1.union >= l2.union
    }

    def <=(l2: FINAL): Boolean = l2 >= l1
  }

  val UNIT: CASETYPEREF    = CASETYPEREF("Unit", List.empty)
  val optionByteVector     = UNION(BYTESTR, UNIT)
  val optionLong           = UNION(LONG, UNIT)
  val listByteVector: LIST = LIST(BYTESTR)
  val listString: LIST     = LIST(STRING)
}
