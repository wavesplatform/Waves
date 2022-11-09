package com.wavesplatform.blockchain

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

  implicit final class Ops[A](val self: BlockchainData[A]) extends AnyVal {
    def or(x: => BlockchainData[A]): BlockchainData[A] = if (self.loaded) self else x

    def toFoundStr(f: A => Any = x => x): String =
      Ops.toFoundStr("", map(f))

    def toFoundStr(label: String, f: A => Any): String =
      Ops.toFoundStr(s"$label=", map(f))

    def map[B](f: A => B): BlockchainData[B] = self match {
      case Cached(a) => BlockchainData.Cached(f(a))
      case Absence   => Absence
      case Unknown   => Unknown
    }
  }

  object Ops {
    def toFoundStr(prefix: String, value: BlockchainData[Any]): String = value match {
      case Cached(value) => s"found $prefix$value"
      case Absence       => "found absence"
      case Unknown       => "not found"
    }
  }
}
