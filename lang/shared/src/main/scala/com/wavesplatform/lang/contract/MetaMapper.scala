package com.wavesplatform.lang.contract

import com.wavesplatform.lang.v1.compiler.Types
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.protobuf.dapp.DAppMeta
import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import shapeless.{HList, HMap, Nat}
import shapeless.Nat._

object MetaMapper {
  trait Serializable {
    def serialized: Map[String, String]
  }

  trait MetaVersion {
    type Data <: Serializable
    implicit val serializer: Data => Map[String, String]
  }
  case object V1 extends MetaVersion {
    type Data = List[(String, List[FINAL])]
    override implicit val serializer: List[(String, List[FINAL])] => Map[String, String] = _
  }
  case object V2 extends MetaVersion {
    type Data = List[(String, List[FINAL])]
    override implicit val serializer: List[(String, List[FINAL])] => Map[String, String] = _
  }

  def toProto(funcTypes: List[FuncArgTypes]): Either[String, DAppMeta] =
    funcTypes
      .traverse { case (funcName, types) => funcToProto(funcName, types) }
      .map(DAppMeta(_))

  def textMapFromProto(meta: DAppMeta): Either[String, Map[String, String]] = {
    for {
      version <- resolveVersion(meta.version)
      data    <- fromProto[version.type](meta)
    } yield data.serialized
  }

   */
  private def fromProto[V <: MetaVersion](meta: DAppMeta)(implicit s: MetaMapperStrategy[V]): Either[String, V#Data] =
    s.fromProto(meta)

  private def toProto[V <: MetaVersion](data: V#Data)(implicit s: MetaMapperStrategy[V]): Either[String, DAppMeta] =
    s.toProto(data)

  private def resolveVersion(version: Int): Either[String, MetaVersion] = {
    version match {
      case 1          => Right(V1)
      case n if n > 0 => Left(s"Unsupported meta version $n")
      case n          => Left(s"Illegal meta version $n, expected postive value")
    }
  }
}
