package com.wavesplatform.grpc

import com.wavesplatform.grpc.syntax.*
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.{ClientCalls, StreamObserver}
import io.grpc.{CallOptions, ManagedChannel, MethodDescriptor}

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.{Future, Promise}
import scala.jdk.CollectionConverters.CollectionHasAsScala

object ScalaGrpcApi extends ScorexLogging {

  def asyncUnaryCall[RequestT, ResponseT](
      channel: ManagedChannel,
      methodDescriptor: MethodDescriptor[RequestT, ResponseT],
      arg: RequestT
  ): Future[ResponseT] = {
    val call       = channel.newCall(methodDescriptor, CallOptions.DEFAULT)
    val p          = Promise[ResponseT]()
    val methodName = methodDescriptor.getFullMethodName.split('/').lastOption.getOrElse(methodDescriptor.getFullMethodName)
    ClientCalls.asyncUnaryCall(
      call,
      arg,
      new StreamObserver[ResponseT] {
        override def onNext(value: ResponseT): Unit = p.trySuccess(value)
        override def onError(t: Throwable): Unit = {
          log.error(s"$methodName[${call.hashCode()}] to ${call.socketAddressStr} failed")
          p.tryFailure(t)
        }
        override def onCompleted(): Unit =
          log.trace(s"$methodName[${call.hashCode()}] to ${call.socketAddressStr} completed")
      }
    )
    p.future
  }

  def asyncFiniteStreamingCall[RequestT, ResponseT](
      channel: ManagedChannel,
      methodDescriptor: MethodDescriptor[RequestT, ResponseT],
      arg: RequestT
  ): Future[List[ResponseT]] = {
    val call       = channel.newCall(methodDescriptor, CallOptions.DEFAULT)
    val p          = Promise[List[ResponseT]]()
    val methodName = methodDescriptor.getFullMethodName.split('/').lastOption.getOrElse(methodDescriptor.getFullMethodName)
    ClientCalls.asyncServerStreamingCall(
      call,
      arg,
      new StreamObserver[ResponseT] {
        private val responseList = new LinkedBlockingQueue[ResponseT]()

        override def onNext(value: ResponseT): Unit = responseList.put(value)
        override def onError(t: Throwable): Unit = {
          log.error(s"$methodName[${call.hashCode()}] to ${call.socketAddressStr} failed")
          p.tryFailure(t)
        }
        override def onCompleted(): Unit = {
          log.trace(s"$methodName[${call.hashCode()}] to ${call.socketAddressStr} completed")
          p.trySuccess(responseList.asScala.toList)
        }
      }
    )
    p.future
  }

}
