package com.wavesplatform.matcher

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import com.wavesplatform.matcher.api.MatcherApiRoute
import com.wavesplatform.matcher.market.{MatcherActor, OrderHistoryActor}
import com.wavesplatform.settings.{BlockchainSettings, RestAPISettings}
import com.wavesplatform.state2.reader.StateReader
import scorex.api.http.CompositeHttpService
import scorex.transaction.{History, NewTransactionHandler}
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

class Matcher(actorSystem: ActorSystem,
              wallet: Wallet,
              newTransactionHandler: NewTransactionHandler,
              stateReader: StateReader,
              time: Time,
              history: History,
              blockchainSettings: BlockchainSettings,
              restAPISettings: RestAPISettings, matcherSettings: MatcherSettings) extends ScorexLogging {
  lazy val matcherApiRoutes = Seq(
    MatcherApiRoute(wallet,
      stateReader,
      matcher, orderHistory, restAPISettings, matcherSettings)
  )

  lazy val matcherApiTypes = Seq(
    typeOf[MatcherApiRoute]
  )

  lazy val matcher: ActorRef = actorSystem.actorOf(MatcherActor.props(orderHistory, stateReader, wallet, matcherSettings,
    newTransactionHandler, time, history,
    blockchainSettings.functionalitySettings), MatcherActor.name)

  lazy val orderHistory: ActorRef = actorSystem.actorOf(OrderHistoryActor.props(matcherSettings, stateReader, wallet),
    OrderHistoryActor.name)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdownMatcher(): Unit = {
    Await.result(matcherServerBinding.unbind(), 10.seconds)
  }

  def runMatcher() {
    log.info(s"Starting matcher on: ${matcherSettings.bindAddress}:${matcherSettings.port} ...")

    implicit val as = actorSystem
    implicit val materializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, matcherSettings.bindAddress,
      matcherSettings.port), 5.seconds)

    implicit val ec = actorSystem.dispatcher
    log.info(s"Matcher bound to ${matcherServerBinding.localAddress} ")
  }

}
