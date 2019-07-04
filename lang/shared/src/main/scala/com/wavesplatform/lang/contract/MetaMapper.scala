package com.wavesplatform.lang.contract

import com.wavesplatform.protobuf.dapp.DAppMeta

object MetaMapper {
  def toProto(data: Any): Either[String, DAppMeta] =
    data match {
      case v1Data: MetaMapperStrategyV1.Data => MetaMapperStrategyV1.toProto(v1Data)
      case _ => Left(s"Unexpected meta type: ${data.getClass}")
    }

  def textMapFromProto(meta: DAppMeta): Either[String, Dic] = {
    for {
      strategy <- resolveStrategy(meta.version)
      data     <- strategy.textMapFromProto(meta)
    } yield data
  }

  private def resolveStrategy(version: Int): Either[String, MetaMapperStrategy] = {
    version match {
      case 1          => Right(MetaMapperStrategyV1)
      case n if n > 0 => Left(s"Unsupported meta version $n")
      case n          => Left(s"Illegal meta version $n, expected positive value")
    }
  }
}
