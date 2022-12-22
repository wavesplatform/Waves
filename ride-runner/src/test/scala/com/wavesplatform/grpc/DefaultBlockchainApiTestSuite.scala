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
import monix.execution.{ExecutionModel, UncaughtExceptionReporter}
import monix.execution.exceptions.UpstreamTimeoutException
import monix.execution.schedulers.TestScheduler
import monix.reactive.{MulticastStrategy, Observable}
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.ConcurrentSubject
import sttp.client3.testing.SttpBackendStub

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.chaining.scalaUtilChainingOps

class DefaultBlockchainApiTestSuite extends BaseTestSuite with HasGrpc with ScorexLogging {
  "DefaultBlockchainApi" - {
    log.info("Before")
    "mkBlockchainUpdatesStream" - {
      "restart stream test" in {
        implicit val testScheduler = TestScheduler()
        val s                      = ConcurrentSubject[Int](MulticastStrategy.Publish)
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
        val testScheduler = TestScheduler(ExecutionModel.AlwaysAsyncExecution)

        def dumpTasks(label: String): Unit = {
          val tasks    = testScheduler.state.tasks
          val currTime = testScheduler.state.clock
          val tasksStr = tasks
            .map { x =>
              val runIn = x.runsAt - currTime
              s"$x in $runIn"
            }
            .mkString("\n")
          log.info(s"$label: $tasksStr")
        }

        val subscriptions   = new AtomicInteger(0)
        val upstreamTimeout = 1.second

        val blockchainUpdatesGrpcService = new BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
          override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
            val currSub = subscriptions.incrementAndGet()
            log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] In, #${responseObserver.hashCode()}, subN=$currSub")
            val curr = new AtomicInteger(request.fromHeight)
            val tasks = if (currSub == 1) {
              Task {
                val x1 = curr.get()
                log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Sending to 1, #${responseObserver.hashCode()}: $x1")
                responseObserver.onNext(mkAppend(x1))
              }.delayExecution(1.millis) *> Task {
                val x2 = curr.incrementAndGet()
                log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Sending to 1, #${responseObserver.hashCode()}: $x2")
                responseObserver.onNext(mkAppend(x2))
              }.delayExecution(10.millis) *> Task {
                val x3 = curr.incrementAndGet()
                log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Sending to 1, #${responseObserver.hashCode()}: $x3")
                responseObserver.onNext(mkAppend(x3))
              }.delayExecution(upstreamTimeout + 50.millis) // After timeout, we should not receive this
            } else if (currSub == 2) {
              Task {
                val x1 = curr.get()
                log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Sending to 2, #${responseObserver.hashCode()}: $x1")
                responseObserver.onNext(mkAppend(x1))
              }.delayExecution(10.millis) *> Task {
                val x2 = curr.incrementAndGet()
                log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Sending to 2, #${responseObserver.hashCode()}: $x2")
                responseObserver.onNext(mkAppend(x2))
              }.delayExecution(10.millis) *> Task {
                log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Closing 2, #${responseObserver.hashCode()}")
                responseObserver.onCompleted()
              }.delayExecution(10.millis)
            } else Task.unit

            log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Out, #${responseObserver.hashCode()}")
            tasks.runToFuture(testScheduler) // Why don't we receive messages?
            dumpTasks("grpc")
          }

          override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = ???
          override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse]                   = ???
        }

        withGrpc(blockchainUpdatesGrpcService) { channel =>
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

          val stream        = blockchainApi.mkBlockchainUpdatesStream(testScheduler)
          val currentHeight = new AtomicInteger(0)
          val r = stream.downstream
            .doOnNext { x => Task { log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Before: $x") } }
            .doOnNext {
              case WrappedEvent.Next(x)                             => Task(currentHeight.set(x.getUpdate.height))
              case WrappedEvent.Closed                              => Task(stream.close()) // Task(stream.closeDownstream())
              case WrappedEvent.Failed(_: UpstreamTimeoutException) =>
                // Delay execution to test helps to test, that:
                // 1. We won't receive a message with height of 3 from a stale gRPC stream
                // 2. An upstream timeout during reconnecting doesn't affect whole behaviour
                Task {
                  stream.start(currentHeight.get() - 1)
                }.delayExecution(upstreamTimeout - 50.millis).runToFuture(testScheduler)
                Task.unit
            }
            .doOnNext(x => Task(log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] After: $x")))
            .takeWhile {
              case WrappedEvent.Closed =>
                log.info("WrappedEvent.Closed")
                false
              case _ => true
            }
//            .doOnComplete(Task(log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Done")))
//            .doOnSubscriptionCancel(Task(log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] Cancel")))
//            .doOnError(e => Task(log.error("Got", e)))
//            .doOnSubscribe(Task(log.info("Subscribe")))
//            .doOnEarlyStop(Task(log.info("Early stop")))
            .toListL
            .runToFuture(testScheduler)

          stream.start(1)

          testScheduler.tickOne()        // Subscribe
          testScheduler.tick(1.millis)   // Sending "1"
          testScheduler.tick(10.millis)  // Sending "2"
          testScheduler.tick(1.second)   // Timeout
          testScheduler.tick(50.millis)  // Sending "3" (1s + 50ms since sending "2")
          testScheduler.tick(900.millis) // Starting again
          testScheduler.tick(10.millis)  // Sending "1"
          testScheduler.tick(10.millis)  // Sending "2"
          testScheduler.tick(10.millis)  // Sending Complete

          log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] r: $r")
          withClue("completed") { r.isCompleted shouldBe true }
          val xs = r.value.value.success.value

          withClue("only two completions") { subscriptions.get() shouldBe 2 }

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
