package com.wavesplatform.network

import java.util.concurrent.TimeUnit
import cats.*
import cats.instances.bigInt.*
import cats.instances.tuple.*
import com.google.common.cache.CacheBuilder
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.*
import monix.eval.Coeval
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

case class BestChannel(channel: Channel, score: BigInt) {
  override def toString: String = s"BestChannel(${id(channel)},$score)"
}

object RxScoreObserver extends ScorexLogging {
  type SyncWith = Option[BestChannel]

  case class ChannelClosedAndSyncWith(closed: Option[Channel], syncWith: SyncWith)

  implicit val channelClosedAndSyncWith: Eq[ChannelClosedAndSyncWith] = { (x, y) =>
    x.closed == y.closed && x.syncWith == y.syncWith
  }

  private def calcSyncWith(bestChannel: Option[Channel], localScore: BigInt, scoreMap: scala.collection.Map[Channel, BigInt]): SyncWith = {
    val (bestScore, bestScoreChannels) = scoreMap.foldLeft(BigInt(0) -> List.empty[Channel]) {
      case (r @ (maxScore, maxScoreChannels), (currScoreChannel, currScore)) =>
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
    }
    else None
  }

  def apply(
      scoreTtl: FiniteDuration,
      remoteScoreDebounce: FiniteDuration,
      initalLocalScore: BigInt,
      localScores: Observable[BigInt],
      remoteScores: ChannelObservable[BigInt],
      channelClosed: Observable[Channel],
      channelTimeout: Observable[Channel],
      scheduler: Scheduler
  ): (Observable[ChannelClosedAndSyncWith], Coeval[Stats]) = {

    var localScore: BigInt                  = initalLocalScore
    var currentBestChannel: Option[Channel] = None
    val scores = CacheBuilder
      .newBuilder()
      .expireAfterWrite(scoreTtl.toMillis, TimeUnit.MILLISECONDS)
      .build[Channel, BigInt]()
    val statsReporter = Coeval.eval {
      Stats(localScore, currentBestChannel.toString, scores.size())
    }

    def ls: Observable[Option[Channel]] =
      localScores
        .observeOn(scheduler)
        .distinctUntilChanged
        .map { x =>
          log.debug(s"New local score: $x, old: $localScore, Δ${x - localScore}")
          localScore = x
          None
        }

    // Make a stream of unique scores in each channel
    def rs: Observable[Option[Channel]] =
      remoteScores
        .observeOn(scheduler)
        .groupBy(_._1)
        .map(
          _.distinctUntilChanged
            .debounce(remoteScoreDebounce)
        )
        .merge
        .collect {
          case (ch, score) if ch.isOpen =>
            scores.put(ch, score)
            log.trace(s"${id(ch)} New remote score $score")
            None
        }

    def cc: Observable[Option[Channel]] =
      Observable(channelClosed, channelTimeout).merge
        .observeOn(scheduler)
        .map { ch =>
          scores.invalidate(ch)
          if (currentBestChannel.contains(ch)) {
            log.debug(s"${id(ch)} Best channel has been closed")
            currentBestChannel = None
          }
          Option(ch)
        }

    val observable = Observable(ls, rs, cc).merge
      .map { maybeClosedChannel =>
        val sw: SyncWith = calcSyncWith(currentBestChannel.filterNot(maybeClosedChannel.contains), localScore, scores.asMap().asScala)
        currentBestChannel = sw.map(_.channel)
        ChannelClosedAndSyncWith(maybeClosedChannel, sw)
      }
      .logErr
      .distinctUntilChanged
      .share(scheduler)

    (observable, statsReporter)
  }

  case class Stats(localScore: BigInt, currentBestChannel: String, scoresCacheSize: Long)

}
