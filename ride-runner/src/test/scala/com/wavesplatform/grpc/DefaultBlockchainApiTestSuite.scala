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
          .doOnError {
            case _: UpstreamTimeoutException => Task(log.info("Got timeout"))
            case _                           => Task.unit
          }
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
              val x = curr.get()
              log.info(s"Sending to #${responseObserver.hashCode()}: $x")
              responseObserver.onNext(mkAppend(x))

              // TODO Has Monix a fake timer?
              val t = Task {
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
          val blockchainApi = new DefaultBlockchainApi(
            DefaultBlockchainApi.Settings(
              "",
              DefaultBlockchainApi.GrpcApiSettings(None),
              DefaultBlockchainApi.BlockchainUpdatesApiSettings(1.second, 2)
            ),
            EmptyChannel,
            channel,
            SttpBackendStub.synchronous
          )

          log.info("Before 1")
          val stream        = blockchainApi.mkBlockchainUpdatesStream(global)
          val currentHeight = new AtomicInteger(0)
          val r = stream.stream
            .doOnNext { x =>
              Task(log.info(s"onNext test: $x")) *> {
                x match {
                  case WrappedEvent.Next(x) => Task(currentHeight.set(x.getUpdate.height))
                  case WrappedEvent.Failed(_: UpstreamTimeoutException) =>
                    Task(stream.start(currentHeight.get() - 1)).delayExecution(500.millis)
                  case _ => Task.unit
                }
              }
            }
            .takeWhile {
              case WrappedEvent.Closed => false
              case _ => true
            }
            // A client part
//            .doOnError {
//              case _: UpstreamTimeoutException =>
//                Task(log.info(s"[stream] Got a timeout, restarting again from ${currentHeight.get() - 1}...")) *>
//                  Task(stream.start(currentHeight.get() - 1)).delayExecution(500.millis)
//
//              case _ => Task.unit
//            }
//            .onErrorRestartIf {
//              case _: UpstreamTimeoutException => true
//              case _ => false
//            }
            .runAsyncGetLast

          log.info("Before 2")
          stream.start(1)

          // TODO detect close upstream and restart
          Await.result(r, 5.seconds).value shouldBe a[WrappedEvent[SubscribeEvent]]
          subscriptions.get() shouldBe 2
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
