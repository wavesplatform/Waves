package com.wavesplatform.matcher

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import com.wavesplatform.UtxPool
import com.wavesplatform.db._
import com.wavesplatform.matcher.api.MatcherApiRoute
import com.wavesplatform.matcher.market.{MatcherActor, MatcherTransactionWriter, OrderHistoryActor}
import com.wavesplatform.settings.{BlockchainSettings, RestAPISettings}
import com.wavesplatform.state2.reader.SnapshotStateReader
import io.netty.channel.group.ChannelGroup
import scorex.api.http.CompositeHttpService
import scorex.transaction.History
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

class Matcher(actorSystem: ActorSystem,
              wallet: Wallet,
              utx: UtxPool,
              allChannels: ChannelGroup,
              stateReader: SnapshotStateReader,
              history: History,
              blockchainSettings: BlockchainSettings,
              restAPISettings: RestAPISettings, matcherSettings: MatcherSettings) extends ScorexLogging {
  lazy val matcherApiRoutes = Seq(
    MatcherApiRoute(wallet, matcher, orderHistory, txWriter, restAPISettings, matcherSettings)
  )

  lazy val matcherApiTypes = Seq(
    typeOf[MatcherApiRoute]
  )

  lazy val matcher: ActorRef = actorSystem.actorOf(MatcherActor.props(orderHistory, stateReader, wallet, utx, allChannels,
    matcherSettings, history, blockchainSettings.functionalitySettings), MatcherActor.name)

  lazy val db = openDB(matcherSettings.dataDir, matcherSettings.levelDbCacheSize)

  lazy val orderHistory: ActorRef = actorSystem.actorOf(OrderHistoryActor.props(db, matcherSettings, utx, wallet),
    OrderHistoryActor.name)

  lazy val txWriter: ActorRef = actorSystem.actorOf(MatcherTransactionWriter.props(db, matcherSettings),
    MatcherTransactionWriter.name)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdownMatcher(): Unit = {
    db.close()
    Await.result(matcherServerBinding.unbind(), 10.seconds)
  }

  private def checkDirectory(directory: File): Unit = if (!directory.exists()) {
    log.error(s"Failed to create directory '${directory.getPath}'")
    sys.exit(1)
  }

  def runMatcher() {
    val journalDir = new File(matcherSettings.journalDataDir)
    val snapshotDir = new File(matcherSettings.snapshotsDataDir)
    journalDir.mkdirs()
    snapshotDir.mkdirs()

    checkDirectory(journalDir)
    checkDirectory(snapshotDir)

    log.info(s"Starting matcher on: ${matcherSettings.bindAddress}:${matcherSettings.port} ...")

    implicit val as: ActorSystem = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, matcherSettings.bindAddress,
      matcherSettings.port), 5.seconds)

    log.info(s"Matcher bound to ${matcherServerBinding.localAddress} ")
  }

}
