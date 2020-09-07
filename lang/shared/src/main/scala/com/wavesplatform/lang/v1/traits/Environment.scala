package com.wavesplatform.lang.v1.traits

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain._
import shapeless._

object Environment {
  import io.circe.{Decoder, HCursor}

  case class BalanceDetails(available: Long, regular: Long, generating: Long, effective: Long)

  implicit val BalanceDetailsDecoder: Decoder[BalanceDetails] = (c: HCursor) =>
    for {
      available <- c.downField("available").as[Long]
      regular <- c.downField("regular").as[Long]
      generating <- c.downField("generating").as[Long]
      effective <- c.downField("effective").as[Long]
    } yield BalanceDetails(available, regular, generating, effective)

  type InputEntity = Tx :+: Ord :+: PseudoTx :+: CNil

  case class AssetId(id: Array[Byte])
  type Tthis = Recipient.Address :+: AssetId :+: CNil
}

trait Environment[F[_]] {
  def chainId: Byte
  def inputEntity: Environment.InputEntity
  def tthis: Environment.Tthis
  def height: F[Long]
  def transactionById(id: Array[Byte]): F[Option[Tx]]
  def transferTransactionById(id: Array[Byte]): F[Option[Tx.Transfer]]
  def transactionHeightById(id: Array[Byte]): F[Option[Long]]
  def assetInfoById(id: Array[Byte]): F[Option[ScriptAssetInfo]]
  def lastBlockOpt(): F[Option[BlockInfo]]
  def blockInfoByHeight(height: Int): F[Option[BlockInfo]]
  def data(addressOrAlias: Recipient, key: String, dataType: DataType): F[Option[Any]]
  def resolveAlias(name: String): F[Either[String, Recipient.Address]]
  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): F[Either[String, Long]]
  def accountWavesBalanceOf(addressOrAlias: Recipient): F[Either[String, Environment.BalanceDetails]]
  def multiPaymentAllowed: Boolean
  def txId: ByteStr
  def transferTransactionFromProto(b: Array[Byte]): F[Option[Tx.Transfer]]
  def addressFromString(address: String): Either[String, Address]
  def dAppAlias: Boolean = false
}
