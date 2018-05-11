package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Terms._

object TypeInferrer {

  case class MatchResult(tpe: TYPE, name: TYPEPARAM)
  // (ACTUAL, EXPECTED)
  def apply(seq: Seq[(TYPE, TYPEPLACEHOLDER)]): Either[String, Map[TYPEPARAM, TYPE]] = {
    val matching = seq.map(x => matchTypes(x._1, x._2))
    matching.find(_.isLeft) match {
      case Some(left) => left.asInstanceOf[Left[String, Nothing]]
      case None =>
        import cats.instances.option._
        import cats.instances.vector._
        import cats.syntax.all._
        val matchResults = matching.flatMap(_.right.get).groupBy(_.name)

        // a function like Option[T], T => Option[Option[T]]
        // can lead to different interpretations of `T`.
        // `Nothing`, `TypeRef('XXX')` should find common type of `TypeRef('XXX')`
        val resolved = matchResults.mapValues { matchResults =>
          if (matchResults.size == 1) Right(matchResults.head.tpe)
          else {
            val maybeCommonType = matchResults.tail.map(_.tpe).toVector.foldLeftM(matchResults.head.tpe)(findCommonType)
            maybeCommonType match {
              case None             => Left(s"Can't match inferred types of ${matchResults.head.name} over ${matchResults.map(_.tpe)}")
              case Some(commonType) => Right(commonType)
            }
          }
        }

        resolved.find(_._2.isLeft) match {
          case Some((_, left)) => left.asInstanceOf[Left[String, Nothing]]
          case None            => Right(resolved.mapValues(_.right.get))
        }
    }
  }

  def matchTypes(actual: TYPE, expected: TYPEPLACEHOLDER): Either[String, Option[MatchResult]] = {
    lazy val err = s"Non-matching types: expected: $expected, actual: $actual"

    expected match {
      case realType: TYPE =>
        Either.cond(matchType(realType, actual).isDefined, None, err)
      case tp @ TYPEPARAM(char) =>
        Right(Some(MatchResult(actual, tp)))
      case tp @ OPTIONTYPEPARAM(innerTypeParam) =>
        actual match {
          case OPTION(t) => matchTypes(t, innerTypeParam)
          case _         => Left(err)
        }
    }
  }

  def inferResultType(resultType: TYPEPLACEHOLDER, resolved: Map[TYPEPARAM, TYPE]): Either[String, TYPE] = {
    resultType match {
      case plainType: TYPE => Right(plainType)
      case tp @ TYPEPARAM(_) =>
        resolved.get(tp) match {
          case None    => Left(s"Unknown functon return type $tp")
          case Some(r) => Right(r)
        }
      case OPTIONTYPEPARAM(t) => inferResultType(t, resolved).map(OPTION)
    }
  }

  def findCommonType(list: Seq[TYPE]): Option[TYPE] = list match {
    case one :: Nil => Some(one)
    case head :: tail =>
      for {
        t <- findCommonType(tail)
        r <- findCommonType(head, t)
      } yield r
  }
  def findCommonType(t1: TYPE, t2: TYPE): Option[TYPE]      = findCommonType(t1, t2, biDirectional = true)
  def matchType(required: TYPE, actual: TYPE): Option[TYPE] = findCommonType(required, actual, biDirectional = false)

  private def findCommonType(required: TYPE, actual: TYPE, biDirectional: Boolean): Option[TYPE] =
    if (actual == NOTHING) Some(required)
    else if (required == NOTHING && biDirectional) Some(actual)
    else if (required == actual) Some(required)
    else
      (required, actual) match {
        case (OPTION(it1), OPTION(it2)) => findCommonType(it1, it2, biDirectional).map(OPTION)
        case _                          => None
      }
}
