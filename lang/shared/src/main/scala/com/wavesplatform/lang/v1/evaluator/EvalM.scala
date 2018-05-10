package com.wavesplatform.lang.v1.evaluator

import cats.data.{EitherT, Kleisli}
import cats.implicits._
import cats.{Monad, StackSafeMonad}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}
import monix.eval.Coeval

final case class EvalM[A](inner: Kleisli[Coeval, CoevalRef[EvaluationContext], Either[ExecutionError, A]]) {
  def ter(ctx: EvaluationContext): TrampolinedExecResult[A] = {
    val atom = CoevalRef.of(ctx)

    EitherT(inner.run(atom))
  }

  def run(ctx: EvaluationContext): Either[(EvaluationContext, ExecutionError), A] = {
    val atom = CoevalRef.of(ctx)

    val action = inner
      .run(atom)

    (for {
      result  <- action
      lastCtx <- atom.read
    } yield result.left.map(err => (lastCtx, err))).value
  }
}

object EvalM {

  implicit val monadInstance: Monad[EvalM] = new StackSafeMonad[EvalM] {
    override def pure[A](x: A): EvalM[A] =
      EvalM(Kleisli.pure[Coeval, CoevalRef[EvaluationContext], Either[ExecutionError, A]](x.asRight[ExecutionError]))

    override def flatMap[A, B](fa: EvalM[A])(f: A => EvalM[B]): EvalM[B] = {
      EvalM(fa.inner.flatMap({
        case Right(v)  => f(v).inner
        case Left(err) => Kleisli.pure(err.asLeft[B])
      }))
    }
  }

  def getContext: EvalM[EvaluationContext] =
    EvalM(Kleisli[Coeval, CoevalRef[EvaluationContext], Either[ExecutionError, EvaluationContext]](ref => {
      ref.read.map(_.asRight[ExecutionError])
    }))

  def setContext(ctx: EvaluationContext): EvalM[Unit] =
    EvalM(Kleisli[Coeval, CoevalRef[EvaluationContext], Either[ExecutionError, Unit]](ref => {
      ref.write(ctx).map(_.asRight[ExecutionError])
    }))

  def updateContext(f: EvaluationContext => EvaluationContext): EvalM[Unit] = getContext >>= (f andThen setContext)

  def liftValue[A](a: A): EvalM[A] =
    monadInstance.pure(a)

  def liftError[A](err: ExecutionError): EvalM[A] =
    EvalM(Kleisli[Coeval, CoevalRef[EvaluationContext], Either[ExecutionError, A]](_ => Coeval.delay(err.asLeft[A])))

  def liftTER[A](ter: Coeval[Either[ExecutionError, A]]): EvalM[A] =
    EvalM(Kleisli[Coeval, CoevalRef[EvaluationContext], Either[ExecutionError, A]](_ => ter))
}
