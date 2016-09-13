package scorex

import java.security.SecureRandom

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.reflect.runtime.universe
import scala.util._

package object utils {

  @tailrec
  final def untilTimeout[T](timeout: FiniteDuration,
                            delay: FiniteDuration = 100.milliseconds,
                            onFailure: => Unit = {})(fn: => T): T = {
    Try {
      fn
    } match {
      case Success(x) => x
      case _ if timeout > delay =>
        Thread.sleep(delay.toMillis)
        untilTimeout(timeout - delay, delay, onFailure)(fn)
      case Failure(e) =>
        onFailure
        throw e
    }
  }

  def randomBytes(howMany: Int = 32): Array[Byte] = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }

  def objectFromString[T](fullClassName: String): Try[T] = Try {
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
    val module = runtimeMirror.staticModule(fullClassName)
    val obj = runtimeMirror.reflectModule(module)
    obj.instance.asInstanceOf[T]
  }

}
