package scorex.lagonaki

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKitBase}
import akka.util.Timeout
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.Matchers

import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}

abstract class ActorTestingCommons extends TestKitBase
  with org.scalatest.path.FreeSpecLike
  with Matchers
  with ImplicitSender
  with PathMockFactory {

  protected implicit val testTimeout = Timeout(500 milliseconds)
  protected val testDuration = testTimeout.duration

  implicit final lazy val system = ActorSystem(getClass.getSimpleName)

  protected final def testSafely(fun: => Unit): Unit = {
    getClass.getSimpleName testSafely fun
  }

  protected final class ActorTestingStringWrapper(s: String) {
    def testSafely(fun: => Unit): Unit = {
      s - {
        try {
          fun
        } finally {
          try verifyExpectations
          finally shutdown()
        }
      }
    }
  }

  protected final implicit def convertTo(s: String): ActorTestingStringWrapper = new ActorTestingStringWrapper(s)
}