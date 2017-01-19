package com.wavesplatform.matcher

import akka.actor.ActorSystem
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

  lazy val matcher = actorSystem.actorOf(MatcherActor.props(storedState, wallet, settings,
    transactionModule), MatcherActor.name)

  @volatile var bindingFuture : Future[ServerBinding]= _

  def shutdownMatcher(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Await.result(bindingFuture.flatMap(_.unbind()), 10 seconds)
  }

  def runMatcher() {
    log.info(s"Starting matcher on: ${settings.matcherHost}:${settings.matcherPort} ...")

    implicit val materializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, settings.asInstanceOf[Settings]).compositeRoute
    bindingFuture = Http().bindAndHandle(combinedRoute, settings.matcherHost, settings.matcherPort)

    implicit val ec = actorSystem.dispatcher
    bindingFuture.map { serverBinding =>
      log.info(s"Matcher bound to ${serverBinding.localAddress} ")
    }.onFailure {
      case ex: Exception =>
        log.error(s"Failed to bind to${settings.matcherHost}:${settings.matcherPort}!", ex)
        actorSystem.terminate()
    }
  }

}
