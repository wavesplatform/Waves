package com.wavesplatform.lang.contract.meta

import cats.implicits._
import cats.data.OptionT
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.protobuf.dapp.DAppMeta

object MetaMapper {
  def toProto[V <: MetaVersion](version: V)(data: version.Self#Data): Either[String, DAppMeta] =
    version.strategy.toProto(data)

  def dicFromProto(dApp: DApp): Either[String, Dic] = {
    val versionEntry = Map("version" -> Single(dApp.meta.version.toString))
    extractMeta(dApp).value
      .map(meta => Dic(versionEntry ++ meta.map(_.m).getOrElse(Map())))
  }

  private def extractMeta(dApp: DApp) =
    for {
      version <- OptionT(resolveVersion(dApp.meta.version))
      data    <- OptionT.liftF(version.strategy.protoInfo(dApp))
    } yield data

  private def resolveVersion(version: Int): Either[String, Option[MetaVersion]] =
    version match {
      case 0          => Right(None)
      case 1          => Right(Some(V1))
      case n if n > 0 => Left(s"Unsupported meta version $n")
      case n          => Left(s"Illegal meta version $n, expected positive value")
    }
}
