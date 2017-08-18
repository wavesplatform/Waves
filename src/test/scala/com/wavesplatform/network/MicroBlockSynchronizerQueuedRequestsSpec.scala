package com.wavesplatform.network

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.TransactionGen
import com.wavesplatform.network.MicroBlockSynchronizer.QueuedRequests
import com.wavesplatform.state2.ByteStr
import io.netty.channel._
import org.mockito.Mockito
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration.DurationInt
import scala.language.reflectiveCalls

class MicroBlockSynchronizerQueuedRequestsSpec extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TransactionGen {

  private val waitResponseTimeout = 500.millis
  private val completeWaitTime = 50.millis

  private def channelHandlerContextMock: ChannelHandlerContext = {
    val r = Mockito.mock(classOf[ChannelHandlerContext])
    val name = s"chc-${ThreadLocalRandom.current().nextInt()}"
    Mockito.doReturn(name).when(r).name()
    r
  }

  "should make a second request, when the first is timed out" in {
    val sig = ByteStr("foo".getBytes)
    val requests = new QueuedRequests(waitResponseTimeout) {
      var ownerCtxs: List[ChannelHandlerContext] = List.empty

      override protected def newRequest(ownerCtx: ChannelHandlerContext, sig: ByteStr): ChannelFuture = {
        ownerCtxs = ownerCtx :: ownerCtxs
        val r = Mockito.mock(classOf[ChannelFuture])
        Mockito.when(r.isDone).thenReturn(false)
        r
      }
    }

    val ownerCtx1 = channelHandlerContextMock
    val ownerCtx2 = channelHandlerContextMock

    requests.ask(ownerCtx1, sig)
    requests.ask(ownerCtx2, sig)
    Thread.sleep((1.7 * waitResponseTimeout).toMillis)

    requests.ownerCtxs shouldBe List(ownerCtx2, ownerCtx1)
  }

  "should ignore ask did twice from same owner" in {
    val sig = ByteStr("foo".getBytes)
    val requests = new QueuedRequests(waitResponseTimeout) {
      var ownerCtxs: List[ChannelHandlerContext] = List.empty

      override protected def newRequest(ownerCtx: ChannelHandlerContext, sig: ByteStr): ChannelFuture = {
        ownerCtxs = ownerCtx :: ownerCtxs
        val r = Mockito.mock(classOf[ChannelFuture])
        Mockito.when(r.isDone).thenReturn(false)
        r
      }
    }

    val ownerCtx = channelHandlerContextMock

    requests.ask(ownerCtx, sig)
    requests.ask(ownerCtx, sig)
    Thread.sleep((2.5 * waitResponseTimeout).toMillis)

    requests.ownerCtxs shouldBe List(ownerCtx)
  }

  "should not make a second request before the first is timed out" in {
    val sig = ByteStr("foo".getBytes)
    val requests = new QueuedRequests(waitResponseTimeout)

    val ownerCtx1 = channelHandlerContextMock
    val cf1 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf1).when(ownerCtx1).writeAndFlush(*)

    val ownerCtx2 = channelHandlerContextMock

    requests.ask(ownerCtx1, sig)
    requests.ask(ownerCtx2, sig)
    Thread.sleep((0.5 * waitResponseTimeout).toMillis)
    Mockito.verify(ownerCtx2, Mockito.times(0)).writeAndFlush(*)
  }

  "should immediately start the second request if it is added after the first one was timed out" in {
    val sig = ByteStr("foo".getBytes)
    val requests = new QueuedRequests(waitResponseTimeout)

    val ownerCtx1 = channelHandlerContextMock
    val cf1 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf1).when(ownerCtx1).writeAndFlush(*)
    Mockito.doReturn(false).when(cf1).isDone

    val ownerCtx2 = channelHandlerContextMock
    val cf2 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf2).when(ownerCtx2).writeAndFlush(*)

    requests.ask(ownerCtx1, sig)
    Thread.sleep((1.7 * waitResponseTimeout).toMillis)

    requests.ask(ownerCtx2, sig)
    Thread.sleep((0.5 * waitResponseTimeout).toMillis)

    Mockito.verify(ownerCtx2, Mockito.times(1)).writeAndFlush(*)
  }

  "complete should cancel requests" in {
    val sig = ByteStr("foo".getBytes)
    val requests = new QueuedRequests(waitResponseTimeout)

    val ownerCtx1 = channelHandlerContextMock
    val cf1 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf1).when(ownerCtx1).writeAndFlush(*)
    Mockito.doReturn(false).when(cf1).isDone
    Mockito.doReturn(true).when(cf1).cancel(false)

    val ownerCtx2 = channelHandlerContextMock
    val cf2 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf2).when(ownerCtx2).writeAndFlush(*)
    Mockito.doReturn(true).when(cf2).isDone

    requests.ask(ownerCtx1, sig)
    requests.ask(ownerCtx2, sig)
    Thread.sleep((2.5 * waitResponseTimeout).toMillis)

    requests.complete(sig)
    Thread.sleep(completeWaitTime.toMillis)

    Mockito.verify(cf1, Mockito.times(1)).cancel(false)
    Mockito.verify(cf2, Mockito.times(1)).cancel(false)
  }

}
