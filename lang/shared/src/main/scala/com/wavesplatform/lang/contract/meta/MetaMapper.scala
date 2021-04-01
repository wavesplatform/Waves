package com.wavesplatform.lang.contract.meta

import cats.implicits._
import cats.data.OptionT
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.protobuf.dapp.DAppMeta

object MetaMapper {
  def toProto[V <: MetaVersion](version: V)(data: List[List[FINAL]], nameMap: Map[String, String] = Map.empty): Either[String, DAppMeta] =
    version.strategy.toProto(data)

  def dicFromProto(dApp: DApp): Either[String, ParsedMeta] =
    extractMeta(dApp).value.map(opt => ParsedMeta(dApp.meta.version, opt))

  private def extractMeta(dApp: DApp) =
    for {
      version <- OptionT(resolveVersion(dApp.meta.version))
      data    <- OptionT.liftF(version.strategy.fromProto(dApp.meta))
    } yield data

  private def resolveVersion(version: Int): Either[String, Option[MetaVersion]] =
    version match {
      case 0          => Right(None)
      case 1          => Right(Some(V1))
      case 2          => Right(Some(V2))
      case n if n > 0 => Left(s"Unsupported meta version $n")
      case n          => Left(s"Illegal meta version $n, expected positive value")
    }
}
