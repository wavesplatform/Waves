package com.wavesplatform.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import cats.syntax.either._
import com.wavesplatform.api.http.{ApiError, ApiRoute, jsonPost}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network._
import com.wavesplatform.transaction.Transaction
import play.api.libs.json.Reads

trait BroadcastRoute { _: ApiRoute =>
  def utxPoolSynchronizer: UtxPoolSynchronizer

  def broadcast[A: Reads](f: A => Either[ValidationError, Transaction]): Route = jsonPost[A] { a =>
    f(a).fold[ToResponseMarshallable](
      ApiError.fromValidationError,
      tx => utxPoolSynchronizer.publish(tx).transformE(_.bimap(ApiError.fromValidationError, _ => tx))
    )
  }
}
