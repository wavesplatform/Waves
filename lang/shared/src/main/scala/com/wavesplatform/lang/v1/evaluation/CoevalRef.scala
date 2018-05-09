package com.wavesplatform.lang.v1.evaluation

import java.util.concurrent.atomic.AtomicReference

import monix.eval.Coeval

sealed trait CoevalRef[A] {
  def read: Coeval[A]
  def write(a: A): Coeval[Unit]
  def update(f: A => A): Coeval[Unit]
}

object CoevalRef {
  def of[A](a: A): CoevalRef[A] = {
    new CoevalRef[A] {

      private val atom = new AtomicReference[A](a)

      override def read: Coeval[A] = Coeval.delay(atom.get())

      override def write(a: A): Coeval[Unit] = Coeval.delay(atom.lazySet(a))

      override def update(f: A => A): Coeval[Unit] = Coeval.delay {
        for {
          old <- read
          upd = f(old)
          _ <- write(upd)
        } yield ()
      }
    }
  }
}
