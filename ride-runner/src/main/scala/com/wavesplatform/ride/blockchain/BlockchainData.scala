package com.wavesplatform.ride.blockchain

sealed trait BlockchainData[+ValueT] extends Product with Serializable {
  def loaded: Boolean
  def mayBeValue: Option[ValueT]
}

object BlockchainData {
  case class Cached[T](value: T) extends BlockchainData[T] {
    override def loaded: Boolean       = true
    override def mayBeValue: Option[T] = Some(value)
  }

  case object Absence extends BlockchainData[Nothing] {
    override val loaded     = true
    override val mayBeValue = None
  }

  case object Unknown extends BlockchainData[Nothing] {
    override val loaded     = false
    override val mayBeValue = None
  }

  def loaded[T](x: Option[T]): BlockchainData[T] = x match {
    case Some(x) => Cached(x)
    case None    => Absence
  }

  implicit final class Ops[ValueT](val self: BlockchainData[ValueT]) extends AnyVal {
    def or(x: => BlockchainData[ValueT]): BlockchainData[ValueT] = if (self.loaded) self else x
  }
}
