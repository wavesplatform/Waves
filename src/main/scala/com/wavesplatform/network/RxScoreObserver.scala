package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import cats.implicits._
import com.google.common.cache.CacheBuilder
import io.netty.channel._
import monix.eval.Task
import monix.reactive.Observable
import scorex.transaction.History.BlockchainScore
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object RxScoreObserver extends ScorexLogging {

  case class SyncWith(c: Channel, score: BlockchainScore)

  //  implicit val scheduler: SchedulerService = Scheduler.singleThread("rx-score-observer")

  def apply(scoreTtl: FiniteDuration, localScores: Observable[BlockchainScore],
            remoteScores: Observable[(Channel, BlockchainScore)],
            channelClosed: Observable[Channel]): Observable[SyncWith] = {

    var localScore: BlockchainScore = 0
    var pinned: Option[Channel] = None
    val scores = CacheBuilder.newBuilder()
      .expireAfterWrite(scoreTtl.toMillis, TimeUnit.MILLISECONDS)
      .build[Channel, BlockchainScore]()

    def newBestChannel(): Option[SyncWith] = {
      val betterChannels = scores.asMap().asScala.filter(_._2 > localScore)
      if (betterChannels.isEmpty) {
        log.debug("No better scores of remote peers, sync complete")
        None
      } else {
        val groupedByScore = betterChannels.toList.groupBy(_._2)
        val bestScore = groupedByScore.keySet.max
        val bestChannels = groupedByScore(bestScore).map(_._1)
        pinned match {
          case Some(c) if bestChannels contains c => None
          case _ =>
            val head = bestChannels.head
            pinned = Some(head)
            Some(SyncWith(head, bestScore))
        }
      }
    }

    val x = localScores.mapTask(newLocalScore => Task {
      localScore = newLocalScore
    })

    val y = channelClosed.mapTask(ch => Task {
      scores.invalidate(ch)
      if (pinned.contains(ch))
        pinned = None
    })

    val z = remoteScores.mapTask { case ((ch, score)) => Task {
      scores.put(ch, score)
      log.trace(s"${id(ch)} New score $scores")
    }
    }

    Observable.merge(x, y, z).map(_ => newBestChannel())
      .filter(_.isDefined)
      .map(_.get)
  }
}