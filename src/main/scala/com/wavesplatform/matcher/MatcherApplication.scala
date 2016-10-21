package com.wavesplatform.matcher

import scala.reflect.runtime.universe._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer

import com.wavesplatform.matcher.api.MatcherApiRoute
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.settings.WavesSettings
import scorex.api.http.CompositeHttpService
import scorex.settings.Settings
import scorex.utils.ScorexLogging

trait MatcherApplication extends ScorexLogging {

  implicit def actorSystem: ActorSystem
  implicit def settings: WavesSettings

  lazy val matcherApiRoutes = Seq(
    MatcherApiRoute(matcher)
  )

  lazy val matcherApiTypes = Seq(
    typeOf[MatcherApiRoute]
  )

  lazy val matcher = actorSystem.actorOf(MatcherActor.props(), MatcherActor.name)

  def runMatcher() {
    log.info(s"Starting matcher on: ${settings.matcherAddress}:${settings.matcherPort} ...")

    implicit val materializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, settings.asInstanceOf[Settings]).compositeRoute
    val bindingFuture = Http().bindAndHandle(combinedRoute, settings.matcherAddress, settings.matcherPort)

    implicit val ec = actorSystem.dispatcher
    bindingFuture.map { serverBinding =>
      log.info(s"Matcher bound to ${serverBinding.localAddress} ")
    }.onFailure {
      case ex: Exception =>
        log.error(s"Failed to bind to${settings.matcherAddress}:${settings.matcherPort}!", ex)
        actorSystem.terminate()
    }
  }

}
