package com.wavesplatform.lang.v1.evaluation

import cats.{Monad, ~>}
import cats.data.{EitherT, Kleisli}
import com.wavesplatform.lang.{ExecutionError, ExecutionLog, TrampolinedExecResult}
import com.wavesplatform.lang.v1.EvaluationContext
import com.wavesplatform.lang.v1.EvaluationContext._
import com.wavesplatform.lang.v1.ctx.Context
import monix.eval.Coeval
import cats.implicits._

final case class EvalM[A](inner: Kleisli[Coeval, CoevalRef[EvaluationContext], A]) {
  def ter(ctx: Context): TrampolinedExecResult[A] = {
    val atom = CoevalRef.of(EvaluationContext(ctx))

    EitherT[Coeval, ExecutionError, A] {
      inner.run(atom)
        .attempt
        .map(_.leftMap(_.getMessage))
    }
  }

  def run(ctx: Context): Either[(Context, ExecutionLog, ExecutionError), A] = {
    val atom = CoevalRef.of(EvaluationContext(ctx))

    val action = inner
      .run(atom)
      .attempt

    (for {
      result  <- action
      lastCtx <- atom.read
    } yield result.left.map(err => (lastCtx.context, lastCtx.getLog, err.getMessage))).value
  }
}

object EvalM {
  type Inner[A] = Kleisli[Coeval, CoevalRef[EvaluationContext], A]

  private val innerMonad: Monad[Inner] = implicitly[Monad[Inner]]

  implicit val monadInstance: Monad[EvalM] = new Monad[EvalM] {
    override def pure[A](x: A): EvalM[A] =
      EvalM(Kleisli.pure[Coeval, CoevalRef[EvaluationContext], A](x))

    override def flatMap[A, B](fa: EvalM[A])(f: A => EvalM[B]): EvalM[B] =
      EvalM(fa.inner.flatMap(f andThen { _.inner }))

    override def tailRecM[A, B](a: A)(f: A => EvalM[Either[A, B]]): EvalM[B] =
      EvalM(innerMonad.tailRecM(a)(f andThen { _.inner }))
  }

  def getContext: EvalM[Context] =
    EvalM(Kleisli(_.read.map(_.context)))

  def updateContext(f: Context => Context): EvalM[Unit] =
    EvalM(Kleisli(_.update(context.modify(_)(f))))

  def liftValue[A](a: A): EvalM[A] =
    monadInstance.pure(a)

  def liftError[A](err: ExecutionError): EvalM[A] =
    EvalM(Kleisli(_ => Coeval.raiseError[A](new Exception(err))))

  def liftTER[A](ter: Coeval[Either[ExecutionError, A]]): EvalM[A] =
    EvalM(Kleisli(_ => {
      ter.flatMap({
        case Right(v) => Coeval.delay(v)
        case Left(err) => Coeval.raiseError(new Exception(err))
      })
    }))

  def writeLog(l: String): EvalM[Unit] =
    EvalM(Kleisli(_.update(_.logAppend(l))))
}
