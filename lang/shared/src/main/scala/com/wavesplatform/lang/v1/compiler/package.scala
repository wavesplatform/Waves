package com.wavesplatform.lang.v1

import cats.Id
import cats.syntax.applicative.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.task.TaskM
import com.wavesplatform.lang.v1.task.imports.*

import scala.annotation.tailrec
import scala.collection.mutable.Queue

package object compiler {
  val UtilityFunctionPrefix = "$"
  val IsInstanceOf          = s"${UtilityFunctionPrefix}isInstanceOf"
  val TuplePrefix           = s"${UtilityFunctionPrefix}Tuple"
  val GetType               = s"${UtilityFunctionPrefix}getType"

  type CompileM[A] = TaskM[CompilerContext, CompilationError, A]

  implicit class EiExt[A](ei: Either[CompilationError, A]) {
    def toCompileM: CompileM[A] =
      ei.fold(
        raiseError[Id, CompilerContext, CompilationError, A],
        _.pure[CompileM]
      )
  }

  def containsBlockV2(e: EXPR): Boolean = {
    @tailrec
    def horTraversal(queue: Queue[EXPR]): Boolean = {
      queue.headOption match {
        case Some(expr) =>
          expr match {
            case BLOCK(_, _)                => true
            case GETTER(expr1, _)           => horTraversal(queue.tail += expr1)
            case LET_BLOCK(let, body)       => horTraversal(queue.tail ++ Queue(let.value, body))
            case IF(expr1, expr2, expr3)    => horTraversal(queue.tail ++ Queue(expr1, expr2, expr3))
            case FUNCTION_CALL(_, exprList) => horTraversal(queue.tail ++ exprList)
            case _                          => false
          }
        case None => false
      }
    }
    horTraversal(Queue(e))
  }

  def containsArray(e: EXPR): Boolean = {
    @tailrec
    def horTraversal(queue: Queue[EXPR]): Boolean = {
      queue.headOption match {
        case Some(expr) =>
          expr match {
            case ARR(_)                     => true
            case BLOCK(let: LET, body)      => horTraversal(queue.tail ++ Queue(let.value, body))
            case BLOCK(func: FUNC, body)    => horTraversal(queue.tail ++ Queue(func.body, body))
            case LET_BLOCK(let, body)       => horTraversal(queue.tail ++ Queue(let.value, body))
            case GETTER(expr1, _)           => horTraversal(queue.tail += expr1)
            case IF(expr1, expr2, expr3)    => horTraversal(queue.tail ++ Queue(expr1, expr2, expr3))
            case FUNCTION_CALL(_, exprList) => horTraversal(queue.tail ++ exprList)
            case _                          => false
          }
        case None => false
      }
    }
    horTraversal(Queue(e))
  }

  /** (a1,...,an),...,(z1,...,zk)
    * || \/ (a1,...,z1),...,(a1,...,zk),...,(an,...,zk)
    *
    * regroup( List( List(1, 2), List("a", "b", "c") ) ) = List( List(1, "a"), List(2, "a"), List(1, "b"), List(2, "b"), List(1, "c"), List(2, "c") )
    */
  def regroup[A](listOfLists: Seq[Seq[A]]): Seq[Seq[A]] = {
    def combine(acc: Seq[Seq[A]], next: Seq[A]): Seq[Seq[A]] =
      if (acc.isEmpty)
        next.map(Seq(_))
      else
        for {
          a <- acc
          b <- next
        } yield a :+ b

    listOfLists.foldLeft(Seq.empty[Seq[A]])(combine)
  }
}
