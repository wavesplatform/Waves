package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import cats._
import cats.implicits._
import com.google.common.cache.CacheBuilder
import io.netty.channel._
import monix.execution.Scheduler
import monix.reactive.Observable
import scorex.transaction.History.BlockchainScore
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object RxScoreObserver extends ScorexLogging {

  case class BestChannel(channel: Channel, score: BlockchainScore) {
    override def toString: String = s"BestChannel(${id(channel)},score: $score)"
  }

  implicit val bestChannelEq = new Eq[BestChannel] {
    override def eqv(x: BestChannel, y: BestChannel) = x.channel == y.channel && x.score == y.score
  }

  type SyncWith = Option[BestChannel]

  case class ChannelClosedAndSyncWith(closed: Option[Channel], syncWith: SyncWith)

  implicit val channelClosedAndSyncWith = new Eq[ChannelClosedAndSyncWith] {
    override def eqv(x: ChannelClosedAndSyncWith, y: ChannelClosedAndSyncWith) = x.closed == y.closed && x.syncWith == y.syncWith
  }

  private def calcSyncWith(bestChannel: Option[Channel], localScore: BlockchainScore, scoreMap: scala.collection.Map[Channel, BlockchainScore]): SyncWith = {
    val (bestScore, bestScoreChannels) = scoreMap.foldLeft(BigInt(0) -> List.empty[Channel]) {
      case (r@(maxScore, maxScoreChannels), (currScoreChannel, currScore)) =>
        if (currScore > maxScore) currScore -> List(currScoreChannel)
        else if (currScore == maxScore) maxScore -> (currScoreChannel :: maxScoreChannels)
        else r
    }

    if (bestScore > localScore && bestScoreChannels.nonEmpty) bestChannel match {
      case Some(c) if bestScoreChannels.contains(c) => Some(BestChannel(c, bestScore))
      case _ =>
        val head = bestScoreChannels.head
        log.trace(s"${id(head)} Publishing new best channel with score=$bestScore > localScore $localScore")
        Some(BestChannel(head, bestScore))
    } else None
  }

  def apply(scoreTtl: FiniteDuration,
            initalLocalScore: BigInt,
            localScores: Observable[BlockchainScore],
            remoteScores: ChannelObservable[BlockchainScore],
            channelClosed: Observable[Channel]): Observable[ChannelClosedAndSyncWith] = {

    val scheduler = Scheduler.singleThread("rx-score-observer")

    var localScore: BlockchainScore = initalLocalScore
    var currentBestChannel: Option[Channel] = None
    val scores = CacheBuilder.newBuilder()
      .expireAfterWrite(scoreTtl.toMillis, TimeUnit.MILLISECONDS)
      .build[Channel, BlockchainScore]()

    def ls: Observable[Option[Channel]] = localScores
      .observeOn(scheduler)
      .distinctUntilChanged
      .map { x =>
        log.debug(s"New local score: $x, old: $localScore, Δ${x - localScore}")
        localScore = x
        None
      }

    // Make a stream of unique scores in each channel
    def rs: Observable[Option[Channel]] = remoteScores
      .observeOn(scheduler)
      .groupBy(_._1)
      .map(_.distinctUntilChanged)
      .merge
      .map { case ((ch, score)) =>
        scores.put(ch, score)
        log.trace(s"${id(ch)} New remote score $score")
        None
      }

    def cc: Observable[Option[Channel]] = channelClosed
      .observeOn(scheduler)
      .map { ch =>
        scores.invalidate(ch)
        if (currentBestChannel.contains(ch)) {
          log.debug(s"${id(ch)} Best channel has been closed")
          currentBestChannel = None
        }
        Option(ch)
      }

    Observable
      .merge(ls, rs, cc)
      .map { maybeClosedChannel =>
        val sw = calcSyncWith(currentBestChannel, localScore, scores.asMap().asScala)
        currentBestChannel = sw.map(_.channel)
        ChannelClosedAndSyncWith(maybeClosedChannel, sw)
      }
      .logErr
      .distinctUntilChanged
      .share(scheduler)
  }

}