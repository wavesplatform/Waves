package com.wavesplatform.network

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.{BlockGen, RxScheduler}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject as PS

import scala.concurrent.duration.*

class MicroBlockSynchronizerSpec extends FreeSpec with RxScheduler with BlockGen {
  override def testSchedulerName: String = "test-microblock-synchronizer"

  val defaultSettings = MicroblockSynchronizerSettings(1.second, 1.minute, 1.minute)

  private def withMs(
      f: (
          PS[ByteStr],
          PS[(Channel, MicroBlockInv)],
          PS[(Channel, MicroBlockResponse)],
          Observable[(Channel, MicroBlockSynchronizer.MicroblockData, Option[(Channel, MicroBlockSnapshot)])]
      ) => Any
  ) = {
    val peers          = PeerDatabase.NoOp
    val lastBlockIds   = PS[ByteStr]()
    val microInvs      = PS[(Channel, MicroBlockInv)]()
    val microResponses = PS[(Channel, MicroBlockResponse)]()
    val microSnapshots = PS[(Channel, MicroBlockSnapshot)]()
    val (r, _)         = MicroBlockSynchronizer(defaultSettings, false, peers, lastBlockIds, microInvs, microResponses, microSnapshots, testScheduler)
    try {
      f(lastBlockIds, microInvs, microResponses, r)
    } finally {
      lastBlockIds.onComplete()
      microInvs.onComplete()
      microResponses.onComplete()
      microSnapshots.onComplete()
    }
  }

  "should request and propagate next microblock" in withMs { (lastBlockIds, microInvs, microResponses, r) =>
    val microblockData = newItems(r)
    val ch             = new EmbeddedChannel()
    test(for {
      _ <- send(lastBlockIds)(byteStr(0))
      _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
      _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
      _ <- send(microResponses)((ch, MicroBlockResponse(microBlock(1, 0))))
      _ = microblockData().size shouldBe 1
    } yield ())
  }

  "should re-request next block if a previous one failed" in withMs { (lastBlockIds, microInvs, microResponses, r) =>
    val microblockDatas = newItems(r)
    val ch              = new EmbeddedChannel()
    val ch2             = new EmbeddedChannel()
    test(for {
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

  "should not request the same micro if received before" in withMs { (lastBlockIds, microInvs, microResponses, _) =>
    val ch = new EmbeddedChannel()
    test(for {
      _ <- send(lastBlockIds)(byteStr(0))
      _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
      _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
      _ <- send(microResponses)((ch, MicroBlockResponse(microBlock(1, 0))))
      _ <- send(lastBlockIds)(byteStr(1))
      _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
    } yield Option(ch.readOutbound[MicroBlockRequest]) shouldBe None)
  }

  "should not request forked microblocks" in withMs { (lastBlockIds, microInvs, _, _) =>
    val ch  = new EmbeddedChannel()
    val ch2 = new EmbeddedChannel()
    test(for {
      _ <- send(lastBlockIds)(byteStr(0))
      _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
      _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
      _ <- send(microInvs)((ch2, MicroBlockInv(signer, byteStr(2), byteStr(0))))
    } yield Option(ch2.readOutbound[MicroBlockRequest]) shouldBe None)
  }

  "should remember inv to make a request later" in withMs { (lastBlockIds, microInvs, microResponses, _) =>
    val ch  = new EmbeddedChannel()
    val ch2 = new EmbeddedChannel()
    test(for {
      _ <- send(lastBlockIds)(byteStr(0))
      _ <- send(microInvs)((ch, MicroBlockInv(signer, byteStr(1), byteStr(0))))
      _ = ch.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(1))
      _ <- send(microInvs)((ch2, MicroBlockInv(signer, byteStr(2), byteStr(1))))
      _ <- send(microResponses)((ch, MicroBlockResponse(microBlock(1, 0))))
      _ <- send(lastBlockIds)(byteStr(1))
    } yield ch2.readOutbound[MicroBlockRequest] shouldBe MicroBlockRequest(byteStr(2)))
  }

}
