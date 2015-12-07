package scorex

import scala.annotation.tailrec

import scala.concurrent.duration._

package object TimeUtils {

  @tailrec
  final def untilTimeout[T](timeout: FiniteDuration,
                            delay: FiniteDuration = 100.milliseconds)(fn: => T): T = {
    util.Try {
      fn
    } match {
      case util.Success(x) => x
      case _ if timeout > delay =>
        Thread.sleep(delay.toMillis)
        untilTimeout(timeout - delay, delay)(fn)
      case util.Failure(e) => throw e
    }
  }
}
