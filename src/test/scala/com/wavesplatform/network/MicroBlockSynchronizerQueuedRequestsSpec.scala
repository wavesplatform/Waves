package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.network.MircoBlockSynchronizer.QueuedRequests
import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelFuture, ChannelHandlerContext}
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

  private val waitResponseTimeout = 100.millis
  private val completeWaitTime = 50.millis

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

    val ownerCtx1 = Mockito.mock(classOf[ChannelHandlerContext])
    val ownerCtx2 = Mockito.mock(classOf[ChannelHandlerContext])

    requests.ask(ownerCtx1, sig)
    requests.ask(ownerCtx2, sig)
    Thread.sleep((1.7 * waitResponseTimeout).toMillis)

    requests.ownerCtxs shouldBe List(ownerCtx2, ownerCtx1)
  }

  "should not make a second request before time out" in {
    val sig = ByteStr("foo".getBytes)
    val requests = new QueuedRequests(waitResponseTimeout)

    val ownerCtx1 = Mockito.mock(classOf[ChannelHandlerContext])
    val cf1 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf1).when(ownerCtx1).writeAndFlush(*)

    val ownerCtx2 = Mockito.mock(classOf[ChannelHandlerContext])

    requests.ask(ownerCtx1, sig)
    requests.ask(ownerCtx2, sig)
    Thread.sleep((0.99 * waitResponseTimeout).toMillis)
    Mockito.verifyNoMoreInteractions(ownerCtx2)
  }

  "complete should cancel requests" in {
    val sig = ByteStr("foo".getBytes)
    val requests = new QueuedRequests(waitResponseTimeout)

    val ownerCtx1 = Mockito.mock(classOf[ChannelHandlerContext])
    val cf1 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf1).when(ownerCtx1).writeAndFlush(*)
    Mockito.doReturn(false).when(cf1).isDone
    Mockito.doReturn(true).when(cf1).cancel(false)

    val ownerCtx2 = Mockito.mock(classOf[ChannelHandlerContext])
    val cf2 = Mockito.mock(classOf[ChannelFuture])
    Mockito.doReturn(cf2).when(ownerCtx2).writeAndFlush(*)
    Mockito.doReturn(true).when(cf2).isDone

    requests.ask(ownerCtx1, sig)
    requests.ask(ownerCtx2, sig)
    Thread.sleep((2.5 * waitResponseTimeout).toMillis)

    requests.complete(sig)
    Thread.sleep(completeWaitTime.toMillis)

    Mockito.verify(cf1).isDone
    Mockito.verify(cf1).cancel(false)
    Mockito.verifyNoMoreInteractions(cf1)

    Mockito.verify(cf2).cancel(false)
    Mockito.verifyNoMoreInteractions(cf2)
  }

}
