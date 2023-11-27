package com.wavesplatform.transaction

import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.*

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
    TransferTransaction,
    InvokeExpressionTransaction,
    UpdateAssetInfoTransaction
  ).flatMap { x =>
    x.supportedVersions.map { version =>
      ((x.typeId, version), x)
    }
  }.toMap

  val all: Map[(Byte, Byte), TransactionParser] = old.flatMap { case (typeId, builder) =>
    builder.supportedVersions.map { version =>
      ((typeId, version), builder)
    }
  } ++ modern

  def by(typeId: Byte, version: TxVersion): Option[TransactionParser] = all.get((typeId, version))

  def parseBytes(bytes: Array[Byte]): Try[Transaction] = {
    def validate(parser: TransactionParser)(tx: parser.TransactionT): Try[Transaction] = {
      import parser.*
      tx.validatedEither.left.map(ve => new RuntimeException(ve.toString)).toTry
    }
    def modernParseBytes: Try[Transaction] = {
      val typeId  = bytes(1)
      val version = bytes(2)
      modern.get((typeId, version)) match {
        case Some(parser) => parser.parseBytes(bytes).flatMap(validate(parser))
        case None         => Failure[Transaction](UnknownTypeAndVersion(typeId, version))
      }
    }
    def oldParseBytes: Try[Transaction] = {
      old.get(bytes(0)) match {
        case Some(parser) => parser.parseBytes(bytes).flatMap(validate(parser))
        case None         => Failure[Transaction](UnknownType(bytes(0)))
      }
    }

    for {
      _  <- Either.cond(bytes.length > 2, (), BufferUnderflow).toTry
      tx <- if (bytes(0) == 0) modernParseBytes else oldParseBytes
    } yield tx
  }

  def versionIsCorrect(tx: Transaction & VersionedTransaction): Boolean =
    TransactionParsers.all.contains((tx.tpe.id.toByte, tx.version))
}
