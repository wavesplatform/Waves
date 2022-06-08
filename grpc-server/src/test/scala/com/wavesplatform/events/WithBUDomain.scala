package com.wavesplatform.events

import com.wavesplatform.db.WithDomain
import com.wavesplatform.events.FakeObserver.*
import com.wavesplatform.events.api.grpc.protobuf.SubscribeRequest
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{Constants, WavesSettings}
import com.wavesplatform.transaction.TxHelpers
import monix.execution.Scheduler.Implicits.global
import org.iq80.leveldb.DB
import org.scalatest.Suite

trait WithBUDomain extends WithDomain { _: Suite =>
  def withDomainAndRepo(settings: WavesSettings)(f: (Domain, Repo) => Unit, wrapDB: DB => DB = identity): Unit = {
    withDomain(settings) { d =>
      tempDb { db =>
        val repo = new Repo(wrapDB(db), d.blocksApi)
        d.triggers = Seq(repo)
        try f(d, repo)
        finally repo.shutdownHandlers()
      }
    }
  }

  def withGenerateSubscription(request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue), settings: WavesSettings)(
      generateBlocks: Domain => Unit
  )(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
    withDomainAndRepo(settings) { (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))

      val subscription = repo.createFakeObserver(request)
      generateBlocks(d)

      val result = subscription.fetchAllEvents(d.blockchain, if (request.toHeight > 0) request.toHeight else Int.MaxValue)
      f(result.map(_.getUpdate))
    }
  }

  def withNEmptyBlocksSubscription(count: Int = 2, request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue), settings: WavesSettings)(
      f: Seq[PBBlockchainUpdated] => Unit
  ): Unit = withGenerateSubscription(request, settings)(d => for (_ <- 1 to count) d.appendBlock())(f)
}
