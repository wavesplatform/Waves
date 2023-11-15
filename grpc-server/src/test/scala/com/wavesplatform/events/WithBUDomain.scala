package com.wavesplatform.events

import com.google.common.util.concurrent.MoreExecutors
import com.wavesplatform.db.WithDomain
import FakeObserver.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdateResponse, GetBlockUpdatesRangeRequest, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.repo.LiquidState
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{Constants, WavesSettings}
import com.wavesplatform.transaction.TxHelpers
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.rocksdb.RocksDB
import monix.reactive.subjects.PublishToOneSubject
import org.scalatest.Suite
import org.scalatest.concurrent.ScalaFutures.convertScalaFuture

trait WithBUDomain extends WithDomain { _: Suite =>
  def withDomainAndRepo(settings: WavesSettings)(f: (Domain, Repo) => Unit, wrapDB: RocksDB => RocksDB = identity): Unit = {
    withDomain(settings) { d =>
      tempDb { rdb =>
        val repo = new Repo(wrapDB(rdb.db), d.blocksApi)
        d.triggers = Seq(repo)
        try f(d, repo)
        finally repo.shutdownHandlers()
      }
    }
  }

  def withManualHandle(settings: WavesSettings, setSendUpdate: (() => Unit) => Unit)(f: (Domain, Repo) => Unit): Unit =
    withDomain(settings) { d =>
      tempDb { rdb =>
        val repo = new Repo(rdb.db, d.blocksApi) {
          override def newHandler(
              id: String,
              maybeLiquidState: Option[LiquidState],
              subject: PublishToOneSubject[BlockchainUpdated],
              maxQueueSize: Int
          ): Handler =
            new Handler(id, maybeLiquidState, subject, maxQueueSize)(Scheduler(MoreExecutors.newDirectExecutorService())) {
              setSendUpdate(() => super.sendUpdate())
              override def sendUpdate(): Unit = ()
            }
        }
        d.triggers = Seq(repo)
        try f(d, repo)
        finally repo.shutdownHandlers()
      }
    }

  def withGenerateSubscription(
      request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue),
      settings: WavesSettings,
      balances: Seq[AddrWithBalance] = Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))
  )(generateBlocks: Domain => Unit)(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
    withDomainAndRepo(settings) { (d, repo) =>
      d.appendBlock(balances.map(awb => TxHelpers.genesis(awb.address, awb.balance))*)

      val subscription = repo.createFakeObserver(request)
      generateBlocks(d)

      val result = subscription.fetchAllEvents(d.blockchain, if (request.toHeight > 0) request.toHeight else Int.MaxValue)
      f(result.map(_.getUpdate))
    }
  }

  def withGenerateGetBlockUpdate(
      height: Int = 1,
      settings: WavesSettings,
      balances: Seq[AddrWithBalance] = Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))
  )(generateBlocks: Domain => Unit)(f: GetBlockUpdateResponse => Unit): Unit = {
    withDomainAndRepo(settings) { (d, repo) =>
      d.appendBlock(balances.map(awb => TxHelpers.genesis(awb.address, awb.balance))*)
      generateBlocks(d)
      val getBlockUpdate = repo.getBlockUpdate(height)
      f(getBlockUpdate)
    }
  }

  def withGenerateGetBlockUpdateRange(
      request: GetBlockUpdatesRangeRequest,
      settings: WavesSettings,
      balances: Seq[AddrWithBalance] = Seq(AddrWithBalance(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))
  )(generateBlocks: Domain => Unit)(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
    withDomainAndRepo(settings) { (d, repo) =>
      d.appendBlock(balances.map(awb => TxHelpers.genesis(awb.address, awb.balance))*)
      generateBlocks(d)
      val getBlockUpdateRange = repo.getBlockUpdatesRange(request).futureValue.updates
      f(getBlockUpdateRange)
    }
  }

  def withNEmptyBlocksSubscription(count: Int = 2, request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue), settings: WavesSettings)(
      f: Seq[PBBlockchainUpdated] => Unit
  ): Unit = withGenerateSubscription(request, settings)(d => for (_ <- 1 to count) d.appendBlock())(f)
}
