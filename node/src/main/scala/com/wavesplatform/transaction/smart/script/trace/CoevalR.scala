package com.wavesplatform.transaction.smart.script.trace
import com.wavesplatform.lang.ValidationError
import monix.eval.Coeval

case class CoevalR[+A](v: Coeval[TracedResult[ValidationError, A]]) extends AnyVal {
  def flatMap[B](f: A => CoevalR[B]): CoevalR[B] = {
    val r = v.flatMap(
      t =>
        t.resultE match {
          case Right(value)  => f(value).v
          case l: Left[_, _] => Coeval.now(TracedResult(l.asInstanceOf[Either[ValidationError, B]]))
        }
    )
    CoevalR(r)
  }

  def map[B](f: A => B): CoevalR[B] =
    CoevalR(v.map(_.map(f)))

  def withFilter(f: A => Boolean): CoevalR[A] =
    CoevalR(v.map(_.withFilter(f)))

  def leftMap(f: ValidationError => ValidationError): CoevalR[A] =
    CoevalR(v.map(_.leftMap(f)))
}

object CoevalR {
  def traced[A](a: Either[ValidationError, A], trace: List[TraceStep] = Nil): CoevalR[A] =
    CoevalR(Coeval.now(TracedResult(a, trace)))
}
