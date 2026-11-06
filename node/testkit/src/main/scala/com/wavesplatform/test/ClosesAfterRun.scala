package com.wavesplatform.test

import org.scalatest.{Args, Status, Suite, SuiteMixin}

/** A resource opened on first use; closing it does nothing until then. */
final class OnFirstUse[A <: AutoCloseable](open: => A) extends AutoCloseable {
  @volatile private var opened = false
  lazy val get: A = {
    val a = open
    opened = true
    a
  }
  override def close(): Unit = if (opened) get.close()
}

/** Closes what this suite instance opened once its own run completes, also when `run` throws.
  *
  * Not `afterAll`: ParallelTestExecution runs every test in a fresh instance, and BeforeAndAfterAll skips its hooks on those instances. Mix in
  * after BeforeAndAfterAll, so that `afterAll` still sees the resources. Traits that mix this in add to [[closeAfterRun]] and call `super`.
  */
trait ClosesAfterRun extends SuiteMixin { this: Suite =>
  protected def closeAfterRun(): Unit = ()

  abstract override def run(testName: Option[String], args: Args): Status = {
    val status =
      try super.run(testName, args)
      catch {
        case e: Throwable =>
          try closeAfterRun()
          catch { case c: Throwable => e.addSuppressed(c) }
          throw e
      }
    status.whenCompleted(_ => closeAfterRun())
    status
  }
}
