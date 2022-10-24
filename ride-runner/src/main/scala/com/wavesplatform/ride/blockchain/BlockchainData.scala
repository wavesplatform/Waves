package com.wavesplatform.ride.blockchain

sealed trait BlockchainData[+ValueT] extends Product with Serializable {
  def mayBeValue: Option[ValueT]
}

object BlockchainData {
  case class Cached[T](value: T) extends BlockchainData[T] {
    override def mayBeValue: Option[T] = Some(value)
  }

  case object Absence extends BlockchainData[Nothing] {
    override val mayBeValue = None
  }

  def loaded[T](x: Option[T]): BlockchainData[T] = x match {
    case Some(x) => Cached(x)
    case None    => Absence
  }
}
