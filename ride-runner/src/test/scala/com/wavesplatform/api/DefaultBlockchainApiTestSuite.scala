package com.wavesplatform.api

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.execution.ExecutionModel
import monix.execution.exceptions.UpstreamTimeoutException
import monix.execution.schedulers.TestScheduler

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class DefaultBlockchainApiTestSuite extends BaseTestSuite with HasGrpc with ScorexLogging {
  "DefaultBlockchainApi" - {
    "mkBlockchainUpdatesStream" - {
      "restart on slow upstream" in {
        implicit val testScheduler = TestScheduler()
        val subscriptions          = new AtomicInteger(0)
        val upstreamTimeout        = 1.second

        val blockchainUpdatesGrpcService = new BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
          override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
            val subscriptionId = subscriptions.incrementAndGet()
            val currHeight     = new AtomicInteger(request.fromHeight)
            val tasks = if (subscriptionId == 1) {
              Task {
                responseObserver.onNext(mkAppend(currHeight.get()))
              }.delayExecution(1.milli) *> Task {
                responseObserver.onNext(mkAppend(currHeight.incrementAndGet()))
              }.delayExecution(10.millis) *> Task {
                responseObserver.onNext(mkAppend(currHeight.incrementAndGet()))
              }.delayExecution(upstreamTimeout + 50.millis) // After timeout, we should not receive this
            } else if (subscriptionId == 2) {
              Task {
                responseObserver.onNext(mkAppend(currHeight.get()))
              }.delayExecution(10.millis) *> Task {
                responseObserver.onNext(mkAppend(currHeight.incrementAndGet()))
              }.delayExecution(10.millis) *> Task {
                responseObserver.onCompleted()
              }.delayExecution(10.millis)
            } else Task.unit

            tasks.runToFuture(testScheduler)
          }

          override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = ???
          override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse]                   = ???
        }

        withGrpc(blockchainUpdatesGrpcService) { channel =>
          val blockchainApi = new DefaultBlockchainApi(
            DefaultBlockchainApi.Settings(
              DefaultBlockchainApi.GrpcApiSettings(None),
              DefaultBlockchainApi.BlockchainUpdatesApiSettings(upstreamTimeout, 2)
            ),
            EmptyChannel,
            channel
          )

          val stream        = blockchainApi.mkBlockchainUpdatesStream(testScheduler)
          val currentHeight = new AtomicInteger(0)
          val r = stream.downstream
            .doOnNext {
              case WrappedEvent.Next(x)                             => Task(currentHeight.set(x.getUpdate.height))
              case WrappedEvent.Closed                              => Task(stream.close())
              case WrappedEvent.Failed(_: UpstreamTimeoutException) =>
                // Delay execution to test helps to test, that:
                // 1. We won't receive a message with height of 3 from a stale gRPC stream
                // 2. An upstream timeout during reconnecting doesn't affect whole behaviour
                Task {
                  stream.start(Height(currentHeight.get() - 1))
                }.delayExecution(upstreamTimeout - 50.millis).runToFuture(testScheduler)

                Task.unit // Otherwise the stream waits

              case x => fail(s"Unexpected message: $x")
            }
            .toListL
            .runToFuture(testScheduler)

          stream.start(Height(1))

          testScheduler.tickOne()        // Subscribe
          testScheduler.tick(1.milli)    // Sending "1"
          testScheduler.tick(10.millis)  // Sending "2"
          testScheduler.tick(1.second)   // Timeout
          testScheduler.tick(50.millis)  // Sending "3" (1s + 50ms since sending "2")
          testScheduler.tick(900.millis) // Starting again
          testScheduler.tick(10.millis)  // Sending "1"
          testScheduler.tick(10.millis)  // Sending "2"
          testScheduler.tick(10.millis)  // Sending Completed

          withClue("completed") { r.isCompleted shouldBe true }
          withClue("only two completions") { subscriptions.get() shouldBe 2 }

          val xs = r.value.value.success.value
          withClue("received heights") {
            val receivedHeights = xs.collect { case WrappedEvent.Next(x) => x.getUpdate.height }
            receivedHeights shouldBe List(1, 2, 1, 2)
          }

          withClue("one failed event") {
            xs.count {
              case WrappedEvent.Failed(_) => true
              case _                      => false
            } shouldBe 1
          }

          withClue("one closed event") {
            xs.count {
              case WrappedEvent.Closed => true
              case _                   => false
            } shouldBe 1
          }
        }
      }

      "restart on upstream completed" in {
        implicit val testScheduler = TestScheduler(ExecutionModel.AlwaysAsyncExecution)

        val subscriptions = new AtomicInteger(0)
        val blockchainUpdatesGrpcService = new BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
          override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
            val subscriptionId = subscriptions.incrementAndGet()
            val currHeight     = new AtomicInteger(request.fromHeight)
            val tasks = if (subscriptionId == 1) {
              Task {
                responseObserver.onNext(mkAppend(currHeight.get()))
              }.delayExecution(1.milli) *> Task {
                responseObserver.onNext(mkAppend(currHeight.incrementAndGet()))
              }.delayExecution(10.millis) *> Task {
                responseObserver.onCompleted()
              }.delayExecution(50.millis)
            } else if (subscriptionId == 2) {
              Task {
                responseObserver.onNext(mkAppend(currHeight.get()))
              }.delayExecution(10.millis) *> Task {
                responseObserver.onCompleted()
              }.delayExecution(10.millis)
            } else Task.unit

            tasks.runToFuture(testScheduler)
          }

          override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = ???
          override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse]                   = ???
        }

        withGrpc(blockchainUpdatesGrpcService) { channel =>
          val blockchainApi = new DefaultBlockchainApi(
            DefaultBlockchainApi.Settings(
              DefaultBlockchainApi.GrpcApiSettings(None),
              DefaultBlockchainApi.BlockchainUpdatesApiSettings(1.second, 2)
            ),
            EmptyChannel,
            channel
          )

          val stream        = blockchainApi.mkBlockchainUpdatesStream(testScheduler)
          val currentHeight = new AtomicInteger(0)
          val r = stream.downstream
            .doOnNext {
              case WrappedEvent.Next(x) => Task(currentHeight.set(x.getUpdate.height))
              case WrappedEvent.Closed =>
                if (subscriptions.get() == 2) Task(stream.close())
                else {
                  Task {
                    stream.start(Height(currentHeight.get() - 1))
                  }.delayExecution(900.millis).runToFuture
                  Task.unit
                }
              case x => fail(s"Unexpected message: $x")
            }
            .toListL
            .runToFuture

          stream.start(Height(1))

          testScheduler.tickOne()        // Subscribe
          testScheduler.tick(1.milli)    // Sending "1"
          testScheduler.tick(10.millis)  // Sending "2"
          testScheduler.tick(50.millis)  // Sending Completed
          testScheduler.tick(900.millis) // Starting again
          testScheduler.tick(10.millis)  // Sending "1"
          testScheduler.tick(10.millis)  // Sending Completed

          withClue("completed") { r.isCompleted shouldBe true }
          withClue("only two completions") { subscriptions.get() shouldBe 2 }

          val xs = r.value.value.success.value
          withClue("received heights") {
            val receivedHeights = xs.collect { case WrappedEvent.Next(x) => x.getUpdate.height }
            receivedHeights shouldBe List(1, 2, 1)
          }

          withClue("no failed events") {
            xs.count {
              case WrappedEvent.Failed(_) => true
              case _                      => false
            } shouldBe 0
          }

          withClue("two closed events") {
            xs.count {
              case WrappedEvent.Closed => true
              case _                   => false
            } shouldBe 2
          }
        }
      }

      "multiple restarts" in {
        implicit val testScheduler = TestScheduler()
        val subscriptions          = new AtomicInteger(0)
        val upstreamTimeout        = 1.second

        val blockchainUpdatesGrpcService = new BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
          override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
            val subscriptionId = subscriptions.incrementAndGet()
            val currHeight     = new AtomicInteger(request.fromHeight)

            val tasks =
              if (subscriptionId == 1)
                Task.sequence((1 to 4).map { _ =>
                  Task {
                    responseObserver.onNext(mkAppend(currHeight.getAndIncrement()))
                  }.delayExecution(1.milli)
                })
              else if (subscriptionId == 3)
                Task { responseObserver.onNext(mkAppend(currHeight.getAndIncrement())) } *>
                  Task { responseObserver.onCompleted() }.delayExecution(10.millis)
              else Task.unit

            tasks.runToFuture(testScheduler)
          }

          override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = ???

          override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse] = ???
        }

        withGrpc(blockchainUpdatesGrpcService) { channel =>
          val blockchainApi = new DefaultBlockchainApi(
            DefaultBlockchainApi.Settings(
              DefaultBlockchainApi.GrpcApiSettings(None),
              DefaultBlockchainApi.BlockchainUpdatesApiSettings(upstreamTimeout, 2)
            ),
            EmptyChannel,
            channel
          )

          val stream        = blockchainApi.mkBlockchainUpdatesStream(testScheduler)
          val currentHeight = new AtomicInteger(0)
          val r = stream.downstream
            .doOnNext {
              case WrappedEvent.Next(x)                             => Task(currentHeight.set(x.getUpdate.height))
              case WrappedEvent.Closed                              => Task(stream.close())
              case WrappedEvent.Failed(_: UpstreamTimeoutException) => Task { stream.start(Height(currentHeight.get() - 1)) }
              case x                                                => fail(s"Unexpected message: $x")
            }
            .toListL
            .runToFuture(testScheduler)

          stream.start(Height(1))

          testScheduler.tickOne()       // Subscribe
          testScheduler.tick(1.milli)   // Sending "1"
          testScheduler.tick(1.milli)   // Sending "2"
          testScheduler.tick(1.milli)   // Sending "3"
          testScheduler.tick(1.milli)   // Sending "4"
          testScheduler.tick(1.second)  // Timeout
          testScheduler.tick(1.second)  // Timeout
          testScheduler.tickOne()       // Sending "3"
          testScheduler.tick(10.millis) // Sending Completed

          withClue("completed") {
            r.isCompleted shouldBe true
          }
          withClue("only three completions") {
            subscriptions.get() shouldBe 3
          }

          val xs = r.value.value.success.value
          withClue("received heights") {
            val receivedHeights = xs.collect { case WrappedEvent.Next(x) => x.getUpdate.height }
            receivedHeights shouldBe List(1, 2, 3, 4, 3)
          }

          withClue("two failed events") {
            xs.count {
              case WrappedEvent.Failed(_) => true
              case _                      => false
            } shouldBe 2
          }

          withClue("one closed event") {
            xs.count {
              case WrappedEvent.Closed => true
              case _                   => false
            } shouldBe 1
          }
        }
      }
    }
  }

  private def mkAppend(height: Int): SubscribeEvent =
    SubscribeEvent.defaultInstance.withUpdate(
      BlockchainUpdated.defaultInstance.withHeight(height)
    )
}
