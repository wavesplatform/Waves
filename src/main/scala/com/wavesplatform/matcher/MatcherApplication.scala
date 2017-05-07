package com.wavesplatform.matcher

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import com.wavesplatform.matcher.api.MatcherApiRoute
import com.wavesplatform.matcher.market.MatcherActor
import scorex.api.http.CompositeHttpService
import scorex.app.Application
import scorex.transaction.NewTransactionHandler
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

trait MatcherApplication extends ScorexLogging {
  def actorSystem: ActorSystem

  def newTransactionHandler: NewTransactionHandler

  def wallet: Wallet

  private lazy val thisApplication = this.asInstanceOf[Application]

  val matcherSettings = thisApplication.settings.matcherSettings
  lazy val matcherApiRoutes = Seq(
    MatcherApiRoute(thisApplication.wallet,
      thisApplication.stateReader,
      matcher, thisApplication.settings.restAPISettings, matcherSettings)
  )

  lazy val matcherApiTypes = Seq(
    typeOf[MatcherApiRoute]
  )

  lazy val matcher: ActorRef = actorSystem.actorOf(MatcherActor.props(thisApplication.stateReader, wallet, matcherSettings,
    newTransactionHandler, thisApplication.time, thisApplication.history,
    thisApplication.settings.blockchainSettings.functionalitySettings), MatcherActor.name)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdownMatcher(): Unit = {
    Await.result(matcherServerBinding.unbind(), 10.seconds)
  }

  def runMatcher() {
    log.info(s"Starting matcher on: ${matcherSettings.bindAddress}:${matcherSettings.port} ...")

    implicit val as = actorSystem
    implicit val materializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, thisApplication.settings.restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, matcherSettings.bindAddress,
      matcherSettings.port), 5.seconds)

    implicit val ec = actorSystem.dispatcher
    log.info(s"Matcher bound to ${matcherServerBinding.localAddress} ")
  }

}
