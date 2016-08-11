package scorex.lagonaki.integration

import java.util.concurrent.TimeUnit

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}

import scala.concurrent.duration._
import scala.language.postfixOps

trait TestLock extends BeforeAndAfterAll with BeforeAndAfterEach with TimeLimitedTests { this: Suite =>

  import scorex.lagonaki.TestingCommons._

  override def timeLimit: Span = 5 * 60 * 1000 millis

  private var locked = false

  override protected def beforeAll(): Unit = {
    locked = lock.tryLock(timeLimit.toMillis / 10, TimeUnit.MILLISECONDS)
    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    try super.afterAll()
    finally {
      if (locked) lock.unlock()
    }
  }
}
