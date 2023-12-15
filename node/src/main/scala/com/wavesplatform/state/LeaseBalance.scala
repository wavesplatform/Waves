package com.wavesplatform.state

import cats.Monad
import cats.implicits.{toFlatMapOps, toFunctorOps}
import play.api.libs.json.{Json, OFormat}

case class LeaseBalance(in: Long, out: Long) {
  def combineF[F[_]: Monad](that: LeaseBalance)(implicit s: Summarizer[F]): F[LeaseBalance] =
    for {
      in  <- s.sum(in, that.in, "Lease in")
      out <- s.sum(out, that.out, "Lease out")
    } yield LeaseBalance(in, out)
}

object LeaseBalance {
  val empty: LeaseBalance = LeaseBalance(0, 0)

  implicit val format: OFormat[LeaseBalance] = Json.format
}
