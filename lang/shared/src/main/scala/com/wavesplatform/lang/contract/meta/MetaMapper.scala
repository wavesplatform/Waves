package com.wavesplatform.lang.contract.meta

import com.wavesplatform.protobuf.dapp.DAppMeta

object MetaMapper {
  def toProto[V <: MetaVersion](version: V)(data: version.Self#Data): Either[String, DAppMeta] =
    version.strategy.toProto(data)

  def dicFromProto(meta: DAppMeta): Either[String, Dic] =
    for {
      strategy <- resolveVersion(meta.version)
      data     <- strategy.textMapFromProto(meta)
    } yield data

  private def resolveVersion(version: Int): Either[String, MetaMapperStrategy[_]] = {
    version match {
      case 1          => Right(MetaMapperStrategyV1)
      case n if n > 0 => Left(s"Unsupported meta version $n")
      case n          => Left(s"Illegal meta version $n, expected positive value")
    }
  }
}
