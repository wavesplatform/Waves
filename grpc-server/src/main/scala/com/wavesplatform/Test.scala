package com.wavesplatform

import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeRequest}
import io.grpc.{Channel, ManagedChannelBuilder}

object Test extends App {
  val channel: Channel = ManagedChannelBuilder
    .forAddress("localhost", 6881)
    .usePlaintext()
    .build()

  val service    = BlockchainUpdatesApiGrpc.blockingStub(channel)
  var lastHeight = 0
  service.subscribe(SubscribeRequest.of(1, Int.MaxValue)).foreach { se =>
    val currentHeight = se.getUpdate.height
    if (currentHeight > lastHeight + 1) {
      println(s"Error: $lastHeight -> $currentHeight")
      sys.exit(1)
    }
    println(currentHeight)
    lastHeight = currentHeight
  }
}

object Test2 extends App {
  import monix.execution.Scheduler.Implicits.global
  import monix.reactive.Observable
  import monix.reactive.subjects.{AsyncSubject, ConcurrentSubject}

  val obs = Observable.defer {
    println("Creating new")
    Observable.fromIterable(Seq(1, 2, 3))
  }

  val result = obs.publishSelector { obs =>
    val subj1 = AsyncSubject[Int]()
    val subj2 = ConcurrentSubject.publish[Int]
    obs.subscribe(subj1)
    obs.subscribe(subj2)
    subj2 ++ subj1.map(_ * 2)
  }
  result.foreach(println)
}
