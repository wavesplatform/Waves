package com.wavesplatform.it

import org.asynchttpclient.Response
import play.api.libs.json.Format
import play.api.libs.json.Json.parse

import scala.concurrent.{ExecutionContext, Future}

package object api {
  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] = f.map(r => parse(r.getResponseBody).as[A])(ec)
  }
}
