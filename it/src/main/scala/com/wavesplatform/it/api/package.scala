package com.wavesplatform.it

import java.nio.charset.StandardCharsets

import org.asynchttpclient.Response
import play.api.libs.json.Json.parse
import play.api.libs.json.Reads

import scala.concurrent.{ExecutionContext, Future}

package object api {
  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Reads](implicit ec: ExecutionContext): Future[A] = f.map(r => parse(r.getResponseBody(StandardCharsets.UTF_8)).as[A])(ec)
  }
}
