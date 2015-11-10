package scorex

import scala.annotation.tailrec

package object utils {

  @tailrec
  final def retry[T](timeout: Int, delay: Int = 100)(fn: => T): T = {
    util.Try {
      fn
    } match {
      case util.Success(x) => x
      case _ if timeout > delay =>
        Thread.sleep(delay)
        retry(timeout - delay, delay)(fn)
      case util.Failure(e) => throw e
    }
  }
}
