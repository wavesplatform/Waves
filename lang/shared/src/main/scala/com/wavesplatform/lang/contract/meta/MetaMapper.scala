package com.wavesplatform.lang.contract.meta

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.protobuf.dapp.DAppMeta

object MetaMapper {
  def toProto[V <: MetaVersion](version: V)(data: version.Self#Data): Either[String, DAppMeta] =
    version.strategy.toProto(data)

  def dicFromProto(dApp: DApp): Either[String, Dic] =
    for {
      version <- resolveVersion(dApp.meta.version)
      data    <- version.strategy.protoInfo(dApp)
    } yield data

  private def resolveVersion(version: Int): Either[String, MetaVersion] = {
    version match {
      case 1          => Right(V1)
      case n if n > 0 => Left(s"Unsupported meta version $n")
      case n          => Left(s"Illegal meta version $n, expected positive value")
    }
  }
}
