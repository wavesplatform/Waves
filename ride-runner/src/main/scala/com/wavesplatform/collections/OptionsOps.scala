package com.wavesplatform.collections

trait OptionSyntax {
  @`inline` implicit final def optionSyntax[T](self: Option[T]): OptionOps[T] = new OptionOps(self)
}

final class OptionOps[T](private val self: Option[T]) extends AnyVal {
  def toFoundStr(f: T => Any = x => x): String       = OptionOps.toFoundStr("", self.map(f))
  def toFoundStr(label: String, f: T => Any): String = OptionOps.toFoundStr(s"$label=", self.map(f))
}

object OptionOps {
  def toFoundStr(prefix: String, value: Option[Any]): String = s"${value.fold("not found")(x => s"found $prefix$x")}"
}
