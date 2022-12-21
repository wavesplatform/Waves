package com.wavesplatform.grpc

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.grpc.DefaultBlockchainApiTestSuite.EmptyChannel
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import io.grpc.{CallOptions, Channel, ClientCall, MethodDescriptor}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.exceptions.UpstreamTimeoutException
import monix.reactive.MulticastStrategy
import monix.reactive.subjects.ConcurrentSubject
import sttp.client3.testing.SttpBackendStub

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class DefaultBlockchainApiTestSuite extends BaseTestSuite with HasGrpc with ScorexLogging {
  "DefaultBlockchainApi" - {
    log.info("Before")
    "mkBlockchainUpdatesStream" - {
      "restart stream test" in {
        val s = ConcurrentSubject[Int](MulticastStrategy.Publish)
        val r = s
          .timeoutOnSlowUpstream(100.millis)
          .doOnNext { x =>
            Task(log.info(s"Got $x"))
          }
//          .doOnError {
//            case _: UpstreamTimeoutException => Task(log.info("Got timeout"))
//            case _                           => Task.unit
//          }
          .onErrorRecover { _ => 999 }
          .onErrorRestartIf {
            case _: UpstreamTimeoutException => true
            case _                           => false
          }
          .runAsyncGetLast

        s.onNext(1)

        Thread.sleep(150)
        s.onNext(2)

        Thread.sleep(10)
        s.onNext(3)

        Thread.sleep(150)
        s.onNext(4)

        Thread.sleep(10)
        s.onComplete()

        Await.result(r, 1.second)
      }

      "restart on slow upstream" in {
        val subscriptions = new AtomicInteger(0)
        val blockchainUpdatesGrpcService = new BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
          private val working = new AtomicBoolean(true)

          override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = if (working.get()) {
            val curr = new AtomicInteger(request.fromHeight)
            if (subscriptions.getAndIncrement() == 0) {
              // TODO Has Monix a fake timer?
              val t = Task {
                val x = curr.get()
                log.info(s"Sending to #${responseObserver.hashCode()}: $x")
                responseObserver.onNext(mkAppend(x))
              } *> Task {
                val x = curr.incrementAndGet()
                log.info(s"Sending to #${responseObserver.hashCode()}: $x")
                responseObserver.onNext(mkAppend(x))
              }.delayExecution(10.millis) *>
                Task {
                  val x = curr.incrementAndGet()
                  log.info(s"Sending to #${responseObserver.hashCode()}: $x")
                  responseObserver.onNext(mkAppend(x))
                }.delayExecution(1.second + 50.millis)

              t.runToFuture
            } else {
              val t = Task {
                val x = curr.get()
                log.info(s"Sending to #${responseObserver.hashCode()}: $x")
                responseObserver.onNext(mkAppend(x))
              }.delayExecution(10.millis) *> Task {
                val x = curr.incrementAndGet()
                log.info(s"Sending to #${responseObserver.hashCode()}: $x")
                responseObserver.onNext(mkAppend(x))
              }.delayExecution(10.millis) *> Task {
                if (working.compareAndSet(true, false)) {
                  log.info(s"Closing #${responseObserver.hashCode()}")
                  responseObserver.onCompleted()
                }
              }.delayExecution(10.millis)

              t.runToFuture
            }
          }

          override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = ???
          override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse]                   = ???
        }

        withGrpc(blockchainUpdatesGrpcService) { channel =>
          val upstreamTimeout = 1.second
          val blockchainApi = new DefaultBlockchainApi(
            DefaultBlockchainApi.Settings(
              "",
              DefaultBlockchainApi.GrpcApiSettings(None),
              DefaultBlockchainApi.BlockchainUpdatesApiSettings(upstreamTimeout, 2)
            ),
            EmptyChannel,
            channel,
            SttpBackendStub.synchronous
          )

          val stream        = blockchainApi.mkBlockchainUpdatesStream(global)
          val currentHeight = new AtomicInteger(0)
          val r = stream.downstream
            .doOnNext {
              case WrappedEvent.Next(x)                             => Task(currentHeight.set(x.getUpdate.height))
              case WrappedEvent.Closed                              => Task(stream.closeDownstream())
              case WrappedEvent.Failed(_: UpstreamTimeoutException) =>
                // Delay execution to test helps to test, that:
                // 1. We won't receive a message with height of 3 from a stale gRPC stream
                // 2. An upstream timeout during reconnecting doesn't affect whole behaviour
                Task(stream.start(currentHeight.get() - 1)).delayExecution(upstreamTimeout * 3 / 2)
            }
            .doOnComplete(Task(log.info("Done")))
            .doOnSubscriptionCancel(Task(log.info("Cancel")))
            .toListL
            .runToFuture

          stream.start(1)
          val xs = Await.result(r, 7.seconds)

          subscriptions.get() shouldBe 2

          val receivedHeights = xs.collect { case WrappedEvent.Next(x) => x.getUpdate.height }
          receivedHeights shouldBe List(1, 2, 1, 2)
        }
      }
    }
  }

  private def mkAppend(height: Int): SubscribeEvent =
    SubscribeEvent.defaultInstance.withUpdate(
      BlockchainUpdated.defaultInstance.withHeight(height)
    )
}

object DefaultBlockchainApiTestSuite {
  object EmptyChannel extends Channel {
    override def authority(): String = ???
    override def newCall[RequestT, ResponseT](
        methodDescriptor: MethodDescriptor[RequestT, ResponseT],
        callOptions: CallOptions
    ): ClientCall[RequestT, ResponseT] = ???
  }
}
