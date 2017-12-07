package com.wavesplatform.network

import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalamock.scalatest.MockFactory
import org.scalatest._

import scala.concurrent.duration._

class MicroBlockSynchronizerSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with MockFactory with BlockGen {

  val defaultSettngs = MicroblockSynchronizerSettings(1.minute, 1.minute, 1.minute)

  def buildMicroblockSynchronizer():
  (TestHistory, PeerDatabase, PublishSubject[(Channel, MicroBlockInv)], PublishSubject[(Channel, MicroBlockResponse)], Observable[(Channel, MicroBlockSynchronizer.MicroblockData)]) = {
    val history = new TestHistory
    val op = PeerDatabase.NoOp

    val lastBlockIds = PublishSubject[ByteStr]
    val microInvs = PublishSubject[(Channel, MicroBlockInv)]
    val microResponses = PublishSubject[(Channel, MicroBlockResponse)]
    val r = MicroBlockSynchronizer(defaultSettngs, history, op)(lastBlockIds, microInvs, microResponses)
    (history, op, microInvs, microResponses, r)
  }

}
