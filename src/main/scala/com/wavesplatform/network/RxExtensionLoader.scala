package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import cats.implicits._
import com.google.common.cache.CacheBuilder
import com.wavesplatform.network.RxScoreObserver.SyncWith
import io.netty.channel._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object RxExtensionLoader extends ScorexLogging {

  def filter(allBlocks: Observable[(Channel, Block)],


            ): (Observable[(Channel, Block)]) = ???


  def apply(history: NgHistory,
            syncRequests: Observable[SyncWith],
            blocks: Observable[(Channel, Block)],
            sigs: Observable[(Channel, Signatures)]): Observable[ExtensionBlocks] = {


    ???
  }


}