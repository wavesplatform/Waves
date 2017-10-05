package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.concurrent.FutureSemaphore
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedException
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class RemoteScoreObserverSpec extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with Eventually
  with GeneratorDrivenPropertyChecks
  with TransactionGen {

  private implicit val pc: PatienceConfig = PatienceConfig(
    timeout = 1.second,
    interval = 50.millis
  )

  // This hack should be removed after the refactoring
  "should wait until block is downloaded and then pass a received score to the pipeline" in {
    val storage = new FutureSemaphore
    val processBlock = Promise[Unit]()

    val channel = new EmbeddedChannel(
      new RemoteScoreObserver(storage, 10.seconds, Seq.empty, 1),
      new ChannelInboundHandlerAdapter {
        override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = msg match {
          case _: Block => processBlock.future.onComplete { _ => storage.decrement() }
          case _ =>
        }
      }
    )

    def checkExtensionRequested(maxTime: FiniteDuration): Unit = {
      eventually(timeout(maxTime)) {
        Option(channel.readOutbound[LoadBlockchainExtension]()) shouldNot be(empty)
      }
    }

    channel.writeInbound(TestBlock.create(Seq.empty))
    channel.writeInbound(BigInt(2))
    channel.flushInbound()

    intercept[TestFailedException](checkExtensionRequested(300.millis))
    processBlock.success(())

    checkExtensionRequested(300.millis)
  }

}
