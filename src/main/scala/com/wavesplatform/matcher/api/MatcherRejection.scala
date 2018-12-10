package com.wavesplatform.matcher.api
import akka.http.scaladsl.server.Rejection

sealed trait MatcherRejection extends Rejection

case class MessageRejection(message: String) extends MatcherRejection
