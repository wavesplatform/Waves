package scorex.utils

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.scalatest.FunSuite

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}

class SynchronizedTest extends FunSuite {
  val st = new ReentrantReadWriteLock()

  class A extends Synchronized {
    val mut = Synchronized(0)

    override def synchronizationToken: ReentrantReadWriteLock = st

    def read(): Int = read { implicit l =>
      mut()
    }

    def write(): Int = write { implicit l =>
      mut.set(1)
    }

    def readWhileWrite(): Int = write { implicit l =>
      write()
      mut() + read()
    }

    def longRead(): Unit = read { implicit l =>
      mut()
      Thread.sleep(5000)
    }

    def longWrite(): Unit = write { implicit l =>
      mut.set(1)
      Thread.sleep(5000)
      mut.set(1)
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
    Await.result(Future(a.read()), 100.millis)
  }

  test("can't do concurrent writes") {
    val a = new A()
    Future(a.longWrite())
    Thread.sleep(100)
    intercept[TimeoutException] {
      Await.result(Future(a.write()), 100.millis)
    }
  }

  test("can't write while read") {
    val a = new A()
    Future(a.longRead())
    Thread.sleep(100)
    intercept[TimeoutException] {
      Await.result(Future(a.write()), 100.millis)
    }
  }

  test("can't read while write") {
    val a = new A()
    Future(a.longWrite())
    Thread.sleep(100)
    intercept[TimeoutException] {
      Await.result(Future(a.read()), 100.millis)
    }
  }
}
