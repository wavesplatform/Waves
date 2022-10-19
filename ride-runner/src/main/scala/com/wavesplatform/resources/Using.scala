package com.wavesplatform.resources

import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try
import scala.util.Using.Releasable
import scala.util.control.{ControlThrowable, NonFatal}

/**
 * A utility for performing automatic resource management. Copied from scala.util. The main difference - this version contains a shutdown hook.
 *  See the main doc for [[Using `Using`]] for full details.
 */
object Using {

  final class Manager private {
    import Manager._

    private val closed = new AtomicBoolean(false)
    private val resources = new ConcurrentLinkedDeque[Resource[_]]()

    def apply[R: Releasable](resource: R): R = {
      acquire(resource)
      resource
    }

    def acquireWithShutdown[R](resource: R)(shutdown: R => Unit): R = {
      acquire(new AutoCloseable {
        override def close(): Unit = shutdown(resource)
      })
      resource
    }

    def acquire[R: Releasable](resource: R): Unit = {
      if (resource == null) throw new NullPointerException("null resource")
      if (!closed.get()) resources.addFirst(new Resource(resource))
    }

    private def manage[A](op: Manager => A): A = {
      @volatile var toThrow: Throwable = null
      try {
        Runtime.getRuntime.addShutdownHook(new Thread {
          override def start(): Unit = shutdown(toThrow)
        })
        op(this)
      } catch {
        case t: Throwable =>
          toThrow = t
          null.asInstanceOf[A] // compiler doesn't know `finally` will throw
      } finally shutdown(toThrow)
    }

    private def shutdown(toThrowOrig: Throwable): Unit =
      if (closed.compareAndSet(false, true)) {
        var toThrow = toThrowOrig
        while (!resources.isEmpty) {
          val resource = resources.poll()
          try resource.release()
          catch {
            case t: Throwable =>
              if (toThrow == null) toThrow = t
              else toThrow = preferentiallySuppress(toThrow, t)
          }
        }
        if (toThrow != null) throw toThrow
      }

  }

  object Manager {
    def apply[A](op: Manager => A): Try[A] = Try((new Manager).manage(op))

    final private class Resource[R](resource: R)(implicit releasable: Releasable[R]) {
      def release(): Unit = releasable.release(resource)
    }

  }

  private def preferentiallySuppress(primary: Throwable, secondary: Throwable): Throwable = {
    def score(t: Throwable): Int = t match {
      case _: VirtualMachineError => 4
      case _: LinkageError => 3
      case _: InterruptedException | _: ThreadDeath => 2
      case _: ControlThrowable => 0
      case e if !NonFatal(e) => 1 // in case this method gets out of sync with NonFatal
      case _ => -1
    }
    @inline def suppress(t: Throwable, suppressed: Throwable): Throwable = { t.addSuppressed(suppressed); t }

    if (score(secondary) > score(primary)) suppress(secondary, primary)
    else suppress(primary, secondary)
  }

}
