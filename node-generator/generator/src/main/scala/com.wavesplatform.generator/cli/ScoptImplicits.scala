package com.wavesplatform.generator.cli

import com.wavesplatform.generator.Mode
import scopt.Read

import scala.concurrent.duration.FiniteDuration

trait ScoptImplicits {
  implicit def scoptOptionReads[T](implicit r: Read[T]): Read[Option[T]] = Read.stringRead.map {
    case "null" => None
    case x      => Option(r.reads(x))
  }

  implicit val modeRead: Read[Mode.Value] = Read.reads(Mode withName _.toUpperCase)

  implicit val finiteDurationRead: Read[FiniteDuration] = Read.durationRead.map { x =>
    if (x.isFinite()) FiniteDuration(x.length, x.unit)
    else throw new IllegalArgumentException(s"Duration '$x' expected to be finite")
  }
}
