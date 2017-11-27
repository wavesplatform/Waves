package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import cats._
import cats.implicits._
import com.google.common.cache.CacheBuilder
import io.netty.channel._
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import scorex.transaction.History.BlockchainScore
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object RxScoreObserver extends ScorexLogging {

  case class BestChannel(channel: Channel, score: BlockchainScore)

  implicit val bestChannelEq = new Eq[BestChannel] {
    override def eqv(x: BestChannel, y: BestChannel) = x.channel == y.channel && x.score == y.score
  }

  type SyncWith = Option[BestChannel]

  def apply(scoreTtl: FiniteDuration, initalLocalScore: BigInt,
            localScores: Observable[BlockchainScore],
            remoteScores: Observable[(Channel, BlockchainScore)],
            channelClosed: Observable[Channel]): Observable[SyncWith] = {

    val scheduler: SchedulerService = Scheduler.singleThread("rx-score-observer")

    var localScore: BlockchainScore = initalLocalScore
    var currentBestChannel: Option[Channel] = None
    val scores = CacheBuilder.newBuilder()
      .expireAfterWrite(scoreTtl.toMillis, TimeUnit.MILLISECONDS)
      .build[Channel, BlockchainScore]()

    def newBestChannel(): SyncWith = {
      val betterChannels = scores.asMap().asScala.filter(_._2 > localScore)
      if (betterChannels.isEmpty) {
        log.debug(s"No better scores of remote peers, sync complete. Current local score = $localScore")
        currentBestChannel = None
        None
      } else {
        val groupedByScore = betterChannels.toList.groupBy(_._2)
        val bestScore = groupedByScore.keySet.max
        val bestChannels = groupedByScore(bestScore).map(_._1)
        currentBestChannel match {
          case Some(c) if bestChannels contains c =>
            log.trace(s"${id(c)} Publishing same best channel with score $bestScore")
            Some(BestChannel(c, bestScore))
          case _ =>
            val head = bestChannels.head
            currentBestChannel = Some(head)
            log.trace(s"${id(head)} Publishing new best channel with score=$bestScore > localScore $localScore")
            Some(BestChannel(head, bestScore))
        }
      }
    }

    val x = localScores.observeOn(scheduler).map(newLocalScore => {
      log.debug(s"New local score = $newLocalScore observed")
      localScore = newLocalScore
    })

    val y = channelClosed.observeOn(scheduler).map(ch => {
      scores.invalidate(ch)
      if (currentBestChannel.contains(ch)) {
        log.debug(s"${id(ch)} Best channel has been closed")
        currentBestChannel = None
      }
    })

    val z = remoteScores.observeOn(scheduler).map { case ((ch, score)) =>
      scores.put(ch, score)
      log.trace(s"${id(ch)} New remote score $score")
    }

    Observable.merge(x, y, z).map(_ => newBestChannel()).distinctUntilChanged
  }

}