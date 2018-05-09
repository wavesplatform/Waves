package com.wavesplatform.lang.v1.evaluation

import cats.{Monad, StackSafeMonad, ~>}
import cats.data.{EitherT, Kleisli, WriterT}
import com.wavesplatform.lang.{ExecutionError, ExecutionLog, TrampolinedExecResult}
import com.wavesplatform.lang.v1.ctx.Context
import com.wavesplatform.lang.v1.ctx.Context.Lenses._
import monix.eval.Coeval
import cats.implicits._
import com.wavesplatform.lang.v1.evaluation.H.EC

object H {
  type Env = (CoevalRef[Context], StringBuffer)
  type EC[A] = EitherT[Coeval, ExecutionError, A]
}
final case class EvalM[A](inner: Kleisli[Coeval, CoevalRef[Context], Either[ExecutionError, A]]) {
  def ter(ctx: Context): TrampolinedExecResult[A] = {
    val atom = CoevalRef.of(ctx)

    EitherT(inner.run(atom))
  }

  def run(ctx: Context): Either[(Context, ExecutionLog, ExecutionError), A] = {
    val atom = CoevalRef.of(ctx)

    val action = inner
      .run(atom)

    (for {
      result  <- action
      lastCtx <- atom.read
    } yield result.left.map(err => (lastCtx, "EMPTY", err))).value
  }
}

object EvalM {
  type Inner[A] = Kleisli[Coeval, CoevalRef[Context], A]

  private val innerMonad: Monad[Inner] = implicitly[Monad[Inner]]

  implicit val monadInstance: Monad[EvalM] = new StackSafeMonad[EvalM] {
    override def pure[A](x: A): EvalM[A] =
      EvalM(Kleisli.pure[Coeval, CoevalRef[Context], Either[ExecutionError, A]](x.asRight[ExecutionError]))

    override def flatMap[A, B](fa: EvalM[A])(f: A => EvalM[B]): EvalM[B] =
      EvalM(fa.inner.flatMap({
        case Right(v) => f(v).inner
        case Left(err) => Kleisli.pure(err.asLeft[B])
      }))
  }

  def getContext: EvalM[Context] =
    EvalM(Kleisli[Coeval, CoevalRef[Context], Either[ExecutionError, Context]](ref => {
      ref.read.map(_.asRight[ExecutionError])
    }))

  def updateContext(f: Context => Context): EvalM[Unit] =
    EvalM(Kleisli[Coeval, CoevalRef[Context], Either[ExecutionError, Unit]](ref => {
      ref.update(f).map(_.asRight[ExecutionError])
    }))

  def liftValue[A](a: A): EvalM[A] =
    monadInstance.pure(a)

  def liftError[A](err: ExecutionError): EvalM[A] =
    EvalM(Kleisli[Coeval, CoevalRef[Context], Either[ExecutionError, A]](_ => Coeval.delay(err.asLeft[A])))

  def liftTER[A](ter: Coeval[Either[ExecutionError, A]]): EvalM[A] =
    EvalM(Kleisli[Coeval, CoevalRef[Context], Either[ExecutionError, A]](_ => ter))
}
