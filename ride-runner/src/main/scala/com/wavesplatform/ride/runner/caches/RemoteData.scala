package com.wavesplatform.ride.runner.caches

sealed trait RemoteData[+ValueT] extends Product with Serializable {
  def loaded: Boolean
  def mayBeValue: Option[ValueT]
}

object RemoteData {
  case class Cached[T](value: T) extends RemoteData[T] {
    override def loaded: Boolean       = true
    override def mayBeValue: Option[T] = Some(value)
  }

  case object Absence extends RemoteData[Nothing] {
    override val loaded     = true
    override val mayBeValue: Option[Nothing] = None
  }

  case object Unknown extends RemoteData[Nothing] {
    override val loaded     = false
    override val mayBeValue: Option[Nothing] = None
  }

  def apply[T](x: Option[Option[T]]): RemoteData[T] = x match {
    case None          => RemoteData.Unknown
    case Some(None)    => RemoteData.Absence
    case Some(Some(x)) => RemoteData.Cached(x)
  }

  def unknown[T]: RemoteData[T] = Unknown

  def loaded[T](x: T): RemoteData[T] = Cached(x)

  def loaded[T](x: Option[T]): RemoteData[T] = x match {
    case Some(x) => Cached(x)
    case None    => Absence
  }

  def cachedOrUnknown[T](x: Option[T]): RemoteData[T] = x match {
    case Some(x) => RemoteData.Cached(x)
    case None    => RemoteData.Unknown
  }

  implicit final class Ops[A](val self: RemoteData[A]) extends AnyVal {
    def orElse(x: => RemoteData[A]): RemoteData[A] = if (self.loaded) self else x
    def getOrElse(x: => A): A                      = self.mayBeValue.getOrElse(x)

    def toFoundStr(f: A => Any = x => x): String       = Ops.toFoundStr("", map(f))
    def toFoundStr(label: String, f: A => Any): String = Ops.toFoundStr(s"$label=", map(f))

    def map[B](f: A => B): RemoteData[B] = self match {
      case Cached(a) => Cached(f(a))
      case Absence   => Absence
      case Unknown   => Unknown
    }

    def flatMap[B](f: A => RemoteData[B]): RemoteData[B] = self match {
      case Cached(value) => f(value)
      case Absence       => Absence
      case Unknown       => Unknown
    }
  }

  object Ops {
    def toFoundStr(prefix: String, value: RemoteData[Any]): String = value match {
      case Cached(value) => s"found $prefix$value"
      case Absence       => "found absence"
      case Unknown       => "not found"
    }
  }
}
