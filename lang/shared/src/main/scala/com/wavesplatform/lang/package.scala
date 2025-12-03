package com.wavesplatform

import cats.data.EitherT
import cats.{Eval, Monad, StackSafeMonad}
import monix.eval.Coeval

package object lang {
  implicit def toError(msg: String): CommonError = CommonError(msg)

  type ExecutionLog = String

  type CoevalF[F[_]]                  = [X] =>> Coeval[F[X]]
  type EvalF[F[_]]                    = [X] =>> Eval[F[X]]
  type TrampolinedExecResult[F[_], T] = EitherT[EvalF[F], ExecutionError, T]

  implicit val idCoevalFMonad: Monad[Coeval] = new StackSafeMonad[Coeval] {
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
