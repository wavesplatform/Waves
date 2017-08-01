package scorex.utils

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.scalatest.FunSuite

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}

class SynchronizedTest extends FunSuite {

  class A(val synchronizationToken: ReentrantReadWriteLock = new ReentrantReadWriteLock()) extends Synchronized {
    val mut = Synchronized(0)

    def read(): Int = read { implicit l =>
      mut()
    }

    def write(): Int = write { implicit l =>
      mut.set(1)
    }

    def readWhileWrite(): Int = write { _ =>
      read()
    }

    def longRead(): Unit = read { implicit l =>
      mut()
      Thread.sleep(1500)
    }

    def longWrite(): Unit = write { implicit l =>
      mut.set(1)
      Thread.sleep(1500)
      mut.set(1)
    }

    def nestedWrite(): Unit = write { _ =>
      Thread.sleep(200)
      write()
      Thread.sleep(200)
    }
  }

  private def sleep() = Thread.sleep(1500)

  test("nested writes work") {
    val a = new A()
    Await.result(Future(a.nestedWrite()), 600.millis)
  }

  test("nested writes hold write lock") {
    val a = new A()
    Future(a.nestedWrite())

    intercept[TimeoutException] {
      Await.result(Future(a.read()), 300.millis)
    }
  }

  test("nested writes hold write lock across multiple instances") {
    val token = new ReentrantReadWriteLock()

    val a1 = new A(token)
    val a2 = new A(token)
    Future(a1.nestedWrite())
    intercept[TimeoutException] {
      Await.result(Future(a2.read()), 200.millis)
    }
  }

  test("can read, write, read while write") {
    val a = new A()
    a.read()
    a.write()
    a.readWhileWrite()
  }

  test("can do concurrent reads") {
    val a = new A()
    Future(a.longRead())
    Thread.sleep(100)
    Await.result(Future(a.read()), 200.millis)
    sleep()
  }

  test("can't do concurrent writes") {
    val a = new A()
    Future(a.longWrite())
    Thread.sleep(100)
    intercept[TimeoutException] {
      Await.result(Future(a.write()), 100.millis)
    }
    sleep()
  }

  test("can't write while read") {
    val a = new A()
    Future(a.longRead())
    Thread.sleep(100)
    intercept[TimeoutException] {
      Await.result(Future(a.write()), 100.millis)
    }
    sleep()
  }

  test("can't read while write") {
    val a = new A()
    Future(a.longWrite())
    Thread.sleep(100)
    intercept[TimeoutException] {
      Await.result(Future(a.read()), 100.millis)
    }
    sleep()
  }
}
