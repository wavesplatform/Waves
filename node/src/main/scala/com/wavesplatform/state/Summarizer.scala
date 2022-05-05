package com.wavesplatform.state

trait Summarizer[F[_]] {
  def sum(x: Long, y: Long, source: String): F[Long]
}
