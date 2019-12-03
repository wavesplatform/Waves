package com.wavesplatform.transaction

import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._

import scala.util.{Failure, Try}

object TransactionParsers {
  private[this] val old: Map[Byte, TransactionParser] = Seq[TransactionParser](
    GenesisTransaction,
    PaymentTransaction,
    IssueTransaction,
    ReissueTransaction,
    BurnTransaction,
    ExchangeTransaction,
    LeaseTransaction,
    LeaseCancelTransaction,
    CreateAliasTransaction,
    MassTransferTransaction,
    TransferTransaction
  ).map { x =>
    x.typeId -> x
  }.toMap

  private[this] val modern: Map[(Byte, Byte), TransactionParser] = Seq[TransactionParser](
    DataTransaction,
    SetScriptTransaction,
    IssueTransaction,
    CreateAliasTransaction,
    ReissueTransaction,
    BurnTransaction,
    ExchangeTransaction,
    LeaseTransaction,
    LeaseCancelTransaction,
    SponsorFeeTransaction,
    SetAssetScriptTransaction,
    InvokeScriptTransaction,
    TransferTransaction
  ).flatMap { x =>
    x.supportedVersions.map { version =>
      ((x.typeId, version), x)
    }
  }.toMap

  val all: Map[(Byte, Byte), TransactionParser] = old.flatMap {
    case (typeId, builder) =>
      builder.supportedVersions.map { version =>
        ((typeId, version), builder)
      }
  } ++ modern

  def by(typeId: Byte, version: TxVersion): Option[TransactionParser] = all.get((typeId, version))

  def parseBytes(bytes: Array[Byte]): Try[Transaction] = {
    def modernParseBytes: Try[Transaction] = {
      val typeId  = bytes(1)
      val version = bytes(2)
      modern.get((typeId, version)) match {
        case Some(parser) => parser.parseBytes(bytes)
        case None =>
          Failure[Transaction](new IllegalArgumentException(s"Unknown transaction type ($typeId) and version ($version) (modern encoding)"))
      }
    }
    def oldParseBytes: Try[Transaction] = {
      old.get(bytes(0)) match {
        case Some(parser) => parser.parseBytes(bytes)
        case None         => Failure[Transaction](new IllegalArgumentException(s"Unknown transaction type (old encoding): '${bytes(0)}'"))
      }
    }

    for {
      _  <- Either.cond(bytes.length > 2, (), new IllegalArgumentException("Buffer underflow while parsing transaction")).toTry
      tx <- if (bytes(0) == 0) modernParseBytes else oldParseBytes
    } yield tx
  }
}
