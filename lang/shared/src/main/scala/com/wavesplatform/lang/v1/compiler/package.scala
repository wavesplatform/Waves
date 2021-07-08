package com.wavesplatform.lang.v1

import cats.Id
import cats.syntax.applicative._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.task.TaskM
import com.wavesplatform.lang.v1.task.imports._

import scala.annotation.tailrec
import scala.collection.mutable.Queue

package object compiler {
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
}
