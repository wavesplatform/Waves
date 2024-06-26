package com.wavesplatform.lang.v1.compiler

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Types._

object TypeInferrer {

  case class MatchResult(tpe: FINAL, name: TYPEPARAM)
  def apply(seq: Seq[(FINAL, TYPE)], knownTypes: Map[String, FINAL] = Map.empty): Either[String, Map[TYPEPARAM, FINAL]] = {
    val matching = seq.map(x => matchTypes(x._1, x._2, knownTypes, x._1.name, x._2.toString))
    matching.find(_.isLeft) match {
      case Some(left) => left.asInstanceOf[Left[String, Nothing]]
      case None =>
        val matchResults: Map[TYPEPARAM, Seq[MatchResult]] = matching.flatMap(_.explicitGet()).groupBy(_.name)
        val resolved = matchResults.view.mapValues {
          case h :: Nil => Right(h.tpe)
          case matchResults =>
            val commonType = matchResults.map(_.tpe).toVector.reduce(findCommonType(_, _))
            commonType match {
              case NOTHING => Right(NOTHING)
              case commonTuple: TUPLE =>
                val matchingTuples   = matchResults.collect { case MatchResult(t: TUPLE, _) => t }.toList
                val commonTypeExists = checkTuplesCommonType(matchingTuples, commonTuple)
                typeMatchResult(matchResults, matchResults.head.name, commonTuple, commonTypeExists)
              case p: SINGLE => Right(p)
              case ANY       => Right(ANY)
              case u @ UNION(plainTypes, _) =>
                val commonTypeExists = plainTypes.exists { p =>
                  matchResults.map(_.tpe).forall(e => e >= p)
                }
                typeMatchResult(matchResults, matchResults.head.name, u, commonTypeExists)
            }
        }
        resolved.find(_._2.isLeft) match {
          case Some((_, left)) => left.asInstanceOf[Left[String, Nothing]]
          case None =>
            Right(resolved.mapValues { t =>
              t.explicitGet() match {
                case UNION(x :: Nil, _) => x
                case x                  => x
              }
            }.toMap)
        }
    }
  }

  private def typeMatchResult(
      matchResults: Seq[MatchResult],
      placeholder: TYPEPARAM,
      commonType: FINAL,
      commonTypeExists: Boolean
  ) = {
    Either.cond(
      commonTypeExists,
      commonType,
      s"Can't match inferred types of $placeholder over ${matchResults.map(_.tpe).mkString(", ")}"
    )
  }

  private def checkTuplesCommonType(matchingTuples: List[TUPLE], commonTuple: TUPLE): Boolean =
    (groupByPosition(matchingTuples.map(_.types)) zip commonTuple.types)
      .forall {
        case (groupedTypes, t: TUPLE) =>
          checkTuplesCommonType(groupedTypes.collect { case tt: TUPLE => tt }, t)
        case (groupedTypes, singleType) =>
          singleType.typeList.length < groupedTypes.map(_.typeList.length).sum
      }

  // ((1, "a", false), (2, "b", true)) => ((1, 2), ("a", "b"), (false, true))
  private def groupByPosition[T](list: List[List[T]]): List[List[T]] =
    list.foldRight(List.fill(list.head.length)(List.empty[T])) {
      case (list, acc) => (list zip acc).map { case (element, p) => element :: p }
    }

  private def matchTypes(
      argType: FINAL,
      placeholder: TYPE,
      knownTypes: Map[String, FINAL],
      argTypeStr: String,
      matchingTypeStr: String
  ): Either[String, Option[MatchResult]] = {
    lazy val err = s"Non-matching types: expected: $matchingTypeStr, actual: $argTypeStr"

    (placeholder, argType) match {
      case (tp @ TYPEPARAM(_), _)                       => Right(Some(MatchResult(argType, tp)))
      case (PARAMETERIZEDLIST(innerTypeParam), LIST(t)) => matchTypes(t, innerTypeParam, knownTypes, argTypeStr, matchingTypeStr)
      case (PARAMETERIZEDLIST(innerTypeParam), UNION(l, _)) =>
        l.foldLeft(Right(UNION(List(), None)): Either[String, FINAL]) { (u, tl) =>
          u match {
            case Left(e) => Left(e)
            case Right(UNION(l, _)) =>
              tl match {
                case LIST(t: REAL) => Right(UNION(t :: l, None))
                case _             => Left(err)
              }
            case _ => ???
          }
        } flatMap { t =>
          matchTypes(t, innerTypeParam, knownTypes, argTypeStr, matchingTypeStr)
        }
      case (PARAMETERIZEDLIST(_), _)                      => Left(err)
      case (PARAMETERIZEDTUPLE(typeParams), TUPLE(types)) => matchTupleTypes(err, typeParams, types, knownTypes)
      case (PARAMETERIZEDTUPLE(_), _)                     => Left(err)
      case (PARAMETERIZEDUNION(l), _) =>
        val concretes = UNION.create(
          l.filter(_.isInstanceOf[REAL])
            .map(_.asInstanceOf[REAL])
        )
        val parameterized = l
          .filter(_.isInstanceOf[PARAMETERIZED])
          .map(_.asInstanceOf[PARAMETERIZED])
        if (concretes >= UNION.create(argType.typeList)) Right(None)
        else {
          val error = s"Can't resolve correct type for parameterized $placeholder, actual: $argType".asLeft[Option[MatchResult]]
          parameterized
            .foldLeft(error) {
              case (result, nextParameter) =>
                val nonMatchedArgTypes = argType match {
                  case NOTHING            => ???
                  case UNION(argTypes, _) => UNION(argTypes.filterNot(concretes.typeList.contains))
                  case ANY                => ANY
                  case s: SINGLE          => s
                }
                if (result.isLeft)
                  matchTypes(nonMatchedArgTypes, nextParameter, knownTypes, argTypeStr, matchingTypeStr)
                else {
                  val nextResult = matchTypes(nonMatchedArgTypes, nextParameter, knownTypes, argTypeStr, matchingTypeStr)
                  if (nextResult.isLeft)
                    result
                  else
                    error
                }
            }
        }

      case (_: REAL, UNION(types, _)) =>
        types.foldLeft(Right(None): Either[String, Option[MatchResult]]) { (acc, t) =>
          for {
            _ <- acc
            b <- matchTypes(t, placeholder, knownTypes, argTypeStr, matchingTypeStr)
          } yield b
        }

      case (LIST(tp), LIST(t))            => matchTypes(t, tp, knownTypes, argTypeStr, matchingTypeStr)
      case (TUPLE(types1), TUPLE(types2)) => matchTupleTypes(err, types1, types2, knownTypes)
      case (placeholder: FINAL, _) =>
        Either.cond(placeholder >= argType, None, err)
    }
  }

  private def matchTupleTypes(
      err: => String,
      placeholderTypes: List[TYPE],
      targetTypes: List[FINAL],
      knownTypes: Map[String, FINAL]
  ) =
    if (placeholderTypes.length != targetTypes.length)
      Left(err)
    else
      (placeholderTypes zip targetTypes)
        .traverse { case (typeParam, t) => matchTypes(t, typeParam, knownTypes, t.name, typeParam.toString) }
        .map(_ => None)

  // match, e.g. many ifs
  def findCommonType(list: List[FINAL]): FINAL = list match {
    case Nil        => NOTHING
    case one :: Nil => one
    case head :: tail =>
      val t = findCommonType(tail)
      findCommonType(head, t)
  }

  // if-then-else
  def findCommonType(t1: FINAL, t2: FINAL, mergeTuples: Boolean = true): FINAL =
    findCommonTypeR(t1, t2, mergeTuples)

  private def findCommonTypeR(t1: FINAL, t2: FINAL, mergeTuples: Boolean): FINAL = (t1, t2) match {
    case (ANY, _)             => ANY
    case (_, ANY)             => ANY
    case (t1, NOTHING)        => t1
    case (NOTHING, t2)        => t2
    case (t1, t2) if t1 == t2 => t1

    case (r: UNION, a: UNION) => UNION.create((r.typeList.toSet ++ a.typeList.toSet).toSeq)
    case (u: UNION, t: FINAL) => if (u.typeList.contains(t)) u else UNION.create(u.typeList :+ t)
    case (t: FINAL, u: UNION) => if (u.typeList.contains(t)) u else UNION.create(u.typeList :+ t)

    case (LIST(it1), LIST(it2)) => LIST(findCommonTypeR(it1, it2, mergeTuples))
    case (TUPLE(types1), TUPLE(types2)) if mergeTuples && types1.length == types2.length =>
      TUPLE((types1 zip types2).map { case (t1, t2) => findCommonTypeR(t1, t2, mergeTuples) })
    case (p1: FINAL, p2: FINAL) => UNION.create(List(p1, p2))
  }
}
