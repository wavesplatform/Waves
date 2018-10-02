package com.wavesplatform.lang.v1.task

import monix.eval.Coeval
import monix.execution.atomic.{Atomic, _}

sealed trait CoevalRef[A] {
  def read: Coeval[A]
  def write(a: A): Coeval[Unit]
  def copy(): CoevalRef[A]
}

object CoevalRef {
  def of[A, R <: Atomic[A]](a: A)(implicit ab: AtomicBuilder[A, R]): CoevalRef[A] = {
    new CoevalRef[A] {
      private val atom: Atomic[A]            = Atomic(a)
      override def read: Coeval[A]           = Coeval.delay(atom.get)
      override def write(a: A): Coeval[Unit] = Coeval.delay(atom.set(a))
      override def copy(): CoevalRef[A]      = of(read())
    }
  }
}
