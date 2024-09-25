package com.wavesplatform.lang.contract.serialization

import cats.syntax.either.*
import com.wavesplatform.lang.contract.DApp

import scala.util.Try

trait ContractSerDe {
  def serialize(c: DApp): Either[String, Array[Byte]]
  def deserialize(arr: Array[Byte]): Either[String, DApp]

  protected def tryEi[A](f: => A): Either[String, A] = Try(f).toEither.leftMap { e =>
    if (e.getMessage != null) e.getMessage else e.toString
  }
}

object ContractSerDe {

  val CALL_ANNO: Int = 1
  val VER_ANNO: Int  = 3
}
