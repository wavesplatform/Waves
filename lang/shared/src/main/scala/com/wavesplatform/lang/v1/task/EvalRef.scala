package com.wavesplatform.lang.v1.task

import cats.Eval
import monix.execution.atomic.{Atomic, _}

sealed trait EvalRef[A] {
  def read: Eval[A]
  def write(a: A): Eval[Unit]
  def copy(): EvalRef[A]
}

object EvalRef {
  def of[A, R <: Atomic[A]](a: A)(implicit ab: AtomicBuilder[A, R]): EvalRef[A] = {
    new EvalRef[A] {
      private val atom: Atomic[A]            = Atomic(a)
      override def read: Eval[A]             = Eval.later(atom.get)
      override def write(a: A): Eval[Unit]   = Eval.later(atom.set(a))
      override def copy(): EvalRef[A]        = of(read.value)
    }
  }
}
