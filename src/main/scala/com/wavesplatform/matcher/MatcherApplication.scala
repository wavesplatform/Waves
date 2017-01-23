package com.wavesplatform.matcher

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import com.wavesplatform.matcher.api.MatcherApiRoute
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.settings.WavesSettings
import scorex.api.http.CompositeHttpService
import scorex.app.Application
import scorex.settings.Settings
import scorex.transaction.BlockStorage
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet
import scorex.waves.transaction.WavesTransactionModule

import scala.concurrent.{Await, Future}
import scala.reflect.runtime.universe._
import scala.concurrent.duration._

trait MatcherApplication extends ScorexLogging {
  implicit def actorSystem: ActorSystem

  implicit def settings: WavesSettings

  def transactionModule: WavesTransactionModule

  def blockStorage: BlockStorage

  def wallet: Wallet

  def storedState: StoredState = blockStorage.state.asInstanceOf[StoredState]

  lazy val matcherApiRoutes = Seq(
    MatcherApiRoute(this.asInstanceOf[Application], matcher)
  )

  lazy val matcherApiTypes = Seq(
    typeOf[MatcherApiRoute]
  )

  lazy val matcher: ActorRef = actorSystem.actorOf(MatcherActor.props(storedState, wallet, settings,
    transactionModule), MatcherActor.name)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdownMatcher(): Unit = {
    Await.result(matcherServerBinding.unbind(), 10.seconds)
  }

  def runMatcher() {
    log.info(s"Starting matcher on: ${settings.matcherHost}:${settings.matcherPort} ...")

    implicit val materializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, settings.asInstanceOf[Settings]).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, settings.matcherHost, settings.matcherPort), 5.seconds)

    implicit val ec = actorSystem.dispatcher
    log.info(s"Matcher bound to ${matcherServerBinding.localAddress} ")
  }

}
