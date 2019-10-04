package com.wavesplatform.lang.v1.repl.node

import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}

import scala.concurrent.Future

object ErrorMessageEnvironment extends Environment[Future] {
  lazy val unavailable = throw new BlockchainUnavailableException()
  override def chainId: Byte                                                                                           = 0
  override def height: Future[Long]                                                                                    = unavailable
  override def inputEntity: InputEntity                                                                                = unavailable
  override def tthis: Recipient.Address                                                                                = unavailable
  override def transactionById(id: Array[Byte]): Future[Option[Tx]]                                                    = unavailable
  override def transferTransactionById(id: Array[Byte]): Future[Option[Tx]]                                            = unavailable
  override def transactionHeightById(id: Array[Byte]): Future[Option[Long]]                                            = unavailable
  override def assetInfoById(d: Array[Byte]): Future[Option[ScriptAssetInfo]]                                          = unavailable
  override def lastBlockOpt(): Future[Option[BlockInfo]]                                                               = unavailable
  override def blockInfoByHeight(height: Int): Future[Option[BlockInfo]]                                               = unavailable
  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Future[Option[Any]]                   = unavailable
  override def resolveAlias(name: String): Future[Either[String, Recipient.Address]]                                   = unavailable
  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Future[Either[String, Long]] = unavailable
}

class BlockchainUnavailableException extends RuntimeException {
  override def toString: String = "Blockchain state is unavailable from REPL"
}

