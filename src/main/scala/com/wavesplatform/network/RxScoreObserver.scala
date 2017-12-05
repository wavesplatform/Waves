package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import cats._
import cats.implicits._
import com.google.common.cache.CacheBuilder
import io.netty.channel._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
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
    val betterChannels = scoreMap.filter(_._2 > localScore)
    if (betterChannels.isEmpty) {
      log.debug(s"No better scores of remote peers, sync complete. Current local score = $localScore")
      None
    } else {
      val groupedByScore = betterChannels.toList.groupBy(_._2)
      val bestScore = groupedByScore.keySet.max
      val bestChannels = groupedByScore(bestScore).map(_._1)
      bestChannel match {
        case Some(c) if bestChannels contains c =>
          Some(BestChannel(c, bestScore))
        case _ =>
          val head = bestChannels.head
          log.trace(s"${id(head)} Publishing new best channel with score=$bestScore > localScore $localScore")
          Some(BestChannel(head, bestScore))
      }
    }
  }

  def apply(scoreTtl: FiniteDuration,
            initalLocalScore: BigInt,
            localScores: Observable[BlockchainScore],
            remoteScores: ChannelObservable[BlockchainScore],
            channelClosed: Observable[Channel]): Observable[ChannelClosedAndSyncWith] = {

    val scheduler: SchedulerService = Scheduler.singleThread("rx-score-observer")

    var localScore: BlockchainScore = initalLocalScore
    var currentBestChannel: Option[Channel] = None
    val scores = CacheBuilder.newBuilder()
      .expireAfterWrite(scoreTtl.toMillis, TimeUnit.MILLISECONDS)
      .build[Channel, BlockchainScore]()

    val ls: Observable[Option[Channel]] = localScores.mapTask(newLocalScore => Task {
      log.debug(s"New local score = $newLocalScore observed")
      localScore = newLocalScore
      None
    })

    val rs: Observable[Option[Channel]] = remoteScores.mapTask { case ((ch, score)) => Task {
      scores.put(ch, score)
      None
    }
    }

    val cc: Observable[Option[Channel]] = channelClosed.mapTask(ch => Task {
      scores.invalidate(ch)
      if (currentBestChannel.contains(ch)) {
        log.debug(s"${id(ch)} Best channel has been closed")
        currentBestChannel = None
      }
      Option(ch)
    })

    Observable.merge(ls, rs, cc).mapTask { maybeClosedChannel =>
      Task {
        val sw = calcSyncWith(currentBestChannel, localScore, scores.asMap().asScala)
        currentBestChannel = sw.map(_.channel)
        ChannelClosedAndSyncWith(maybeClosedChannel, sw)
      }
    }.executeOn(scheduler).logErr.distinctUntilChanged
  }
}