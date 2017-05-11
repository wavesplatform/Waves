package com.wavesplatform.matcher

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import com.wavesplatform.Application
import com.wavesplatform.matcher.api.MatcherApiRoute
import com.wavesplatform.matcher.market.MatcherActor
import scorex.api.http.CompositeHttpService
import scorex.transaction.NewTransactionHandler
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

trait Matcher extends ScorexLogging {
  self: Application =>

  def actorSystem: ActorSystem

  def newTransactionHandler: NewTransactionHandler

  def wallet: Wallet

  val matcherSettings = settings.matcherSettings
  lazy val matcherApiRoutes = Seq(
    MatcherApiRoute(wallet,
      stateReader,
      matcher, settings.restAPISettings, matcherSettings)
  )

  lazy val matcherApiTypes = Seq(
    typeOf[MatcherApiRoute]
  )

  lazy val matcher: ActorRef = actorSystem.actorOf(MatcherActor.props(stateReader, wallet, matcherSettings,
    newTransactionHandler, time, history,
    settings.blockchainSettings.functionalitySettings), MatcherActor.name)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdownMatcher(): Unit = {
    Await.result(matcherServerBinding.unbind(), 10.seconds)
  }

  def runMatcher() {
    log.info(s"Starting matcher on: ${matcherSettings.bindAddress}:${matcherSettings.port} ...")

    implicit val as = actorSystem
    implicit val materializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, settings.restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, matcherSettings.bindAddress,
      matcherSettings.port), 5.seconds)

    implicit val ec = actorSystem.dispatcher
    log.info(s"Matcher bound to ${matcherServerBinding.localAddress} ")
  }

}
