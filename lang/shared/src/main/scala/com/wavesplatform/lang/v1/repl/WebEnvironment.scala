package com.wavesplatform.lang.v1.repl

import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.repl.http.NodeClient
import com.wavesplatform.lang.v1.repl.http.NodeClient._
import com.wavesplatform.lang.v1.repl.model.{BalanceResponse, HeightResponse}
import com.wavesplatform.lang.v1.repl.model.tx.TransferTransaction
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import io.circe.generic.auto._

case class WebEnvironment(settings: NodeConnectionSettings) extends Environment {
  private val client = NodeClient(settings.url)

  override def chainId: Byte = settings.chainId
  override def tthis: Recipient.Address = Address(ByteStr.decodeBase58(settings.address).get)

  override def height: Long =
    client.get[Id, HeightResponse]("/blocks/height")
      .height

  override def transferTransactionById(id: Array[Byte]): Option[Tx] =
    client.get[Option, TransferTransaction](s"/transactions/info/${Base58.encode(id)}")
      .map(toDomainModel)

  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    client.get[Option, Long](s"/transactions/info/${Base58.encode(id)}")

  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] =
    client.get[Option, ScriptAssetInfo](s"/assets/details/${Base58.encode(id)}")

  override def lastBlockOpt(): Option[BlockInfo] =
    blockInfoByHeight(height.toInt)

  override def blockInfoByHeight(height: Int): Option[BlockInfo] =
    client.get[Option, BlockInfo](s"/blocks/at/$height")

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] =
    client.get[Option, String](s"/addresses/data/${address(recipient)}/$key")

  override def resolveAlias(name: String): Either[String, Address] =
    client.get[Either[String, ?], Address](s"/alias/by-alias/$name")

  override def accountBalanceOf(recipient: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] =
    client.get[Either[String, ?], BalanceResponse](s"/addresses/balance/${address(recipient)}")
      .map(_.balance)

  override def inputEntity: InputEntity                     = ???
  override def transactionById(id: Array[Byte]): Option[Tx] = ???

  private def address(addressOrAlias: Recipient) =
    addressOrAlias match {
      case Address(bytes) => bytes.base58
      case Alias(name)    => resolveAlias(name).explicitGet().bytes.base58
    }

  private def toDomainModel(tx: TransferTransaction): Tx =
    Tx.Transfer(
      proven(tx),
      feeAssetId = tx.feeAssetId.compatId,
      assetId = tx.assetId.compatId,
      amount = tx.amount,
      recipient = tx.recipient,
      attachment = ByteStr(tx.attachment)
    )
}
