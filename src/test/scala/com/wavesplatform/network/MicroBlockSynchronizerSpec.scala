package com.wavesplatform.network

import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import monix.reactive.subjects.PublishSubject
import org.scalamock.scalatest.MockFactory
import org.scalatest._

import scala.concurrent.duration._

class MicroBlockSynchronizerSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with MockFactory with BlockGen {

  val defaultSettngs = MicroblockSynchronizerSettings(1.second, 1.minute, 1.minute)

  def buildMs(ms: MicroblockSynchronizerSettings = defaultSettngs) = {
    val peers = PeerDatabase.NoOp
    val lastBlockIds = PublishSubject[ByteStr]
    val microInvs = PublishSubject[(Channel, MicroBlockInv)]
    val microResponses = PublishSubject[(Channel, MicroBlockResponse)]
    val r = MicroBlockSynchronizer(ms, peers, lastBlockIds, microInvs, microResponses)
    (lastBlockIds, microInvs, microResponses, r)
  }

  "should request and propogate next microblock" in {
    val (lastBlockIds, microInvs, microResponses, r) = buildMs()
    val microblockDatas = newItems(r)
    val ch = new EmbeddedChannel()
    test(
      for {
        _ <- send(lastBlockIds)(byteStr(0))
        _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
        _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
        _ <- send(microResponses)((ch, MicroBlockResponse(microBlock(1, 0))))
        _ = microblockDatas().size shouldBe 1
      } yield ())
  }

  "should re-request next block if a previous one failed" in {
    val (lastBlockIds, microInvs, microResponses, r) = buildMs()
    val microblockDatas = newItems(r)
    val ch = new EmbeddedChannel()
    val ch2 = new EmbeddedChannel()
    test(
      for {
        _ <- send(lastBlockIds)(byteStr(0))
        _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
        _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
        _ <- send(microInvs)((ch2, MicroBlockInv(signer, byteStr(1), byteStr(0))))
        _ = Thread.sleep(2000)
        _ = ch2.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
        _ <- send(microResponses)((ch2, MicroBlockResponse(microBlock(1, 0))))
      } yield {
        val mbd = microblockDatas()
        mbd.size shouldBe 1
        mbd.head._1 shouldBe ch2
      })
  }

  "should not request the same micro if received before" in {
    val (lastBlockIds, microInvs, microResponses, r) = buildMs()
    val microblockDatas = newItems(r)
    val ch = new EmbeddedChannel()
    test(
      for {
        _ <- send(lastBlockIds)(byteStr(0))
        _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
        _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
        _ <- send(microResponses)((ch, MicroBlockResponse(microBlock(1, 0))))
        _ <- send(lastBlockIds)(byteStr(1))
        _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
      } yield Option(ch.readOutbound[MicroBlockRequest]) shouldBe None)
  }

  "should not request forked microblocks" in {
    val (lastBlockIds, microInvs, microResponses, r) = buildMs()
    val microblockDatas = newItems(r)
    val ch = new EmbeddedChannel()
    val ch2 = new EmbeddedChannel()
    test(
      for {
        _ <- send(lastBlockIds)(byteStr(0))
        _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
        _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
        _ <- send(microInvs)((ch2, MicroBlockInv(signer, byteStr(2), byteStr(0))))
      } yield Option(ch2.readOutbound[MicroBlockRequest]) shouldBe None)
  }

  "should remember inv to make a request later" in {
    val (lastBlockIds, microInvs, microResponses, r) = buildMs()
    val microblockDatas = newItems(r)
    val ch = new EmbeddedChannel()
    val ch2 = new EmbeddedChannel()
    test(
      for {
        _ <- send(lastBlockIds)(byteStr(0))
        _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
        _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
        _ <- send(microInvs)((ch2, MicroBlockInv(signer, byteStr(2), byteStr(1))))
        _ <- send(microResponses)((ch, MicroBlockResponse(microBlock(1, 0))))
        _ <- send(lastBlockIds)(byteStr(1))
      } yield ch2.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(2)))
  }

}

