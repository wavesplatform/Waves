package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.{ApplierState, ExtensionBlocks, LoaderState, State}
import com.wavesplatform.network.RxScoreObserver.ChannelClosedAndSyncWith
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import monix.eval.Task
import monix.reactive.subjects.PublishSubject
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError

import scala.concurrent.duration._

class RxExtensionLoaderSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with MockFactory with BlockGen {

  def bs(i: Int): ByteStr = ByteStr(Array(i.toByte))

  def genBlocks(amt: Int) = Gen.listOfN(amt, randomSignerBlockGen).sample.get

  val Idle = State(LoaderState.Idle, ApplierState.Idle)

  val MaxRollback = 10
  val simpleApplier: (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]] = (_, _) => Task(Right(Some(0)))

  def buildExtensionLoader(timeOut: FiniteDuration = 1.day) = {
    val blocks = PublishSubject[(Channel, Block)]
    val sigs = PublishSubject[(Channel, Signatures)]
    val channelClosed = PublishSubject[ChannelClosedAndSyncWith]
    val history = new TestHistory
    val op = PeerDatabase.NoOp
    val invBlockStorage = new InMemoryInvalidBlockStorage
    val singleBlocks = RxExtensionLoader(MaxRollback, timeOut, history, op, invBlockStorage, blocks, sigs, channelClosed)(simpleApplier)

    (history, invBlockStorage, blocks, sigs, channelClosed, singleBlocks)
  }
}
