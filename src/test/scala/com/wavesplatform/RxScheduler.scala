package com.wavesplatform

import monix.execution.Ack
import monix.reactive.subjects.PublishSubject

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._


trait RxScheduler {

  implicit val scheduler = monix.execution.Scheduler.singleThread("rx-scheduler")

  def test[A](f: => Future[A]): A = Await.result(f, 10.seconds)

  def send[A](p: PublishSubject[A])(a: A): Future[Ack] = p.onNext(a).map(ack => {
    Thread.sleep(500)
    ack
  })

}
