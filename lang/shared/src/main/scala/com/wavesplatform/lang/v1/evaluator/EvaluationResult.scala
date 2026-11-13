package com.wavesplatform.lang.v1.evaluator

import com.wavesplatform.lang.{CommonError, ExecutionError}
import monix.eval.Coeval

import scala.util.control.NonFatal

/** A deferred evaluation step: a value, or an error with the complexity left when it happened. An error skips the following `flatMap` and
  * `map` steps; a thrown non-fatal exception unwinds to the nearest `handleErrorWith`, or out of [[run]]. Steps are immutable and can be run
  * more than once. Replaces Monix `Coeval`, see docs/notes/ride-evaluator.md.
  */
sealed abstract class EvaluationResult[+A] {
  import EvaluationResult.*

  def flatMap[B](f: A => EvaluationResult[B]): EvaluationResult[B] = FlatMap(this, f)

  def map[B](f: A => B): EvaluationResult[B] = Map(this, f)

  /** Applies `f` to both values and errors, but not to thrown exceptions. */
  def transform[B](f: Either[(ExecutionError, Int), A] => Either[(ExecutionError, Int), B]): EvaluationResult[B] = Transform(this, f)

  def handleErrorWith[B >: A](handler: Throwable => EvaluationResult[B]): EvaluationResult[B] = HandleError(this, handler)

  /** Runs the steps on the calling thread. Throws the first non-fatal exception that no `handleErrorWith` handled. */
  def run(): Either[(ExecutionError, Int), A] = EvaluationResult.run(this).asInstanceOf[Either[(ExecutionError, Int), A]]
}

object EvaluationResult {
  private final case class Now[A](result: Either[(ExecutionError, Int), A])        extends EvaluationResult[A]
  private final case class Delay[A](thunk: () => Either[(ExecutionError, Int), A]) extends EvaluationResult[A]
  private final case class Suspend[A](thunk: () => EvaluationResult[A])            extends EvaluationResult[A]

  // A step that waits for the result of its source.
  private sealed abstract class Bind[A, B] extends EvaluationResult[B] { def source: EvaluationResult[A] }
  private final case class FlatMap[A, B](source: EvaluationResult[A], f: A => EvaluationResult[B])                extends Bind[A, B]
  private final case class Map[A, B](source: EvaluationResult[A], f: A => B)                                      extends Bind[A, B]
  private final case class HandleError[A](source: EvaluationResult[A], handler: Throwable => EvaluationResult[A]) extends Bind[A, A]
  private final case class Transform[A, B](
      source: EvaluationResult[A],
      f: Either[(ExecutionError, Int), A] => Either[(ExecutionError, Int), B]
  ) extends Bind[A, B]

  def apply[A](value: A): EvaluationResult[A]                  = Now(Right(value))
  def apply[A](error: String, limit: Int): EvaluationResult[A] = Now(Left((CommonError(error), limit)))

  def fromEither[A](result: Either[(ExecutionError, Int), A]): EvaluationResult[A] = Now(result)

  /** Evaluates `result` when the step runs; an exception it throws is a failure of the step. */
  def delay[A](result: => Either[(ExecutionError, Int), A]): EvaluationResult[A] = Delay(() => result)

  def defer[A](result: => EvaluationResult[A]): EvaluationResult[A] = Suspend(() => result)

  /** Runs `coeval` when the step runs, for the native functions that are given as a Coeval. */
  def fromCoeval[A](coeval: Coeval[Either[(ExecutionError, Int), A]]): EvaluationResult[A] =
    Delay(() => coeval.runAttempt().fold(e => throw e, identity))

  private def run(start: EvaluationResult[Any]): Either[(ExecutionError, Int), Any] = {
    // Steps waiting for the value of their source, innermost first.
    val pending = new java.util.ArrayDeque[Bind[?, ?]]()

    var current: EvaluationResult[Any] = start
    // The value of the last finished step, Right or Left, when current is null.
    var result: Either[(ExecutionError, Int), Any] = null
    var error: Throwable                           = null

    while (true) {
      if (error != null) {
        // Unwind to the nearest handler.
        var handled = false
        while (!handled && !pending.isEmpty) pending.pop() match {
          case HandleError(_, handler) =>
            val e = error
            error = null
            handled = true
            try current = handler(e)
            catch { case NonFatal(t) => error = t }
          case _ =>
        }
        if (!handled) throw error
      } else if (current != null) {
        try {
          current match {
            case Now(r) =>
              result = r
              current = null
            case Delay(thunk) =>
              result = thunk()
              current = null
            case Suspend(thunk) => current = java.util.Objects.requireNonNull(thunk())
            case node: Bind[?, ?] =>
              pending.push(node)
              current = node.source.asInstanceOf[EvaluationResult[Any]]
          }
        } catch { case NonFatal(t) => error = t; current = null }
      } else if (pending.isEmpty) {
        return result
      } else {
        try {
          pending.pop() match {
            case FlatMap(_, f) =>
              result match {
                case Right(value) =>
                  current = java.util.Objects.requireNonNull(f.asInstanceOf[Any => EvaluationResult[Any]](value))
                  result = null
                case Left(_) =>
              }
            case Map(_, f) =>
              result match {
                case Right(value) => result = Right(f.asInstanceOf[Any => Any](value))
                case Left(_)      =>
              }
            case Transform(_, f) =>
              result = f.asInstanceOf[Either[(ExecutionError, Int), Any] => Either[(ExecutionError, Int), Any]](result)
            case HandleError(_, _) =>
          }
        } catch { case NonFatal(t) => error = t; result = null }
      }
    }
    throw new IllegalStateException("unreachable")
  }
}
