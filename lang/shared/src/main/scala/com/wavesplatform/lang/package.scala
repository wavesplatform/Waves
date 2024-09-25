package com.wavesplatform

import cats.data.EitherT
import cats.{Eval, Id, Monad, StackSafeMonad}
import monix.eval.Coeval

package object lang {
  implicit def toError(msg: String): CommonError = CommonError(msg)

  type ExecutionLog = String

  type CoevalF[F[_], A]               = Coeval[F[A]]
  type EvalF[F[_], A]                 = Eval[F[A]]
  type TrampolinedExecResult[F[_], T] = EitherT[EvalF[F, *], ExecutionError, T]

  implicit val idCoevalFMonad: Monad[CoevalF[Id, *]] = new StackSafeMonad[CoevalF[Id, *]] {
    override def flatMap[A, B](fa: Coeval[A])(f: A => Coeval[B]): Coeval[B] =
      fa.flatMap(f).memoize

    override def pure[A](x: A): Coeval[A] =
      Coeval.now(x)
  }

  implicit class StringOps(val s: String) extends AnyVal {
    def isWellFormed: Boolean = {
      var i          = 0
      var wellFormed = true
      while (i < s.length && wellFormed) {
        val c = s.charAt(i)
        if (Character.isSurrogate(c)) {
          if (s.codePointAt(i) == c) wellFormed = false
          else i += 1
        }
        i += 1
      }
      wellFormed
    }
  }
}
