package com.wavesplatform.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import cats.syntax.either._
import com.wavesplatform.api.http.{ApiError, ApiRoute, jsonParammedPost}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network._
import com.wavesplatform.transaction.Transaction
import play.api.libs.json._

trait BroadcastRoute { _: ApiRoute =>
  def utxPoolSynchronizer: UtxPoolSynchronizer

  def broadcast[A: Reads](f: A => Either[ValidationError, Transaction]): Route = extractScheduler { implicit sc =>
    jsonParammedPost[A] { (a, params) =>
      f(a).fold[ToResponseMarshallable](
        ApiError.fromValidationError,
        tx =>
          utxPoolSynchronizer
            .publish(tx)
            .map(
              _.transformE(
                _.bimap(
                  ApiError.fromValidationError,
                  _ =>
                    tx.json() ++ params
                      .get("trace")
                      .fold(Json.obj())(_ => Json.obj("trace" -> utxPoolSynchronizer.publish(tx).trace.map(_.loggedJson)))
                )
              )
            )
      )
    }
  }
}
