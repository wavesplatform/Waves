package com.wavesplatform.transaction

import com.wavesplatform.crypto._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransactionV1, ExchangeTransactionV2}
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.base58Length

import scala.util.{Failure, Try}

object TransactionParsers {

  val TimestampLength            = 8
  val AmountLength               = 8
  val TypeLength                 = 1
  val SignatureStringLength: Int = base58Length(SignatureLength)

  private val old: Map[Byte, TransactionParserLite] = Seq[TransactionParserLite](
    GenesisTransaction,
    PaymentTransaction,
    IssueTransactionV1,
    ReissueTransactionV1,
    BurnTransactionV1,
    ExchangeTransactionV1,
    LeaseTransaction,
    LeaseCancelTransactionV1,
    CreateAliasTransaction,
    MassTransferTransaction,
    TransferTransaction
  ).map { x =>
    x.typeId -> x
  }(collection.breakOut)

  private val modern: Map[(Byte, Byte), TransactionParserLite] = Seq[TransactionParserLite](
    DataTransaction,
    SetScriptTransaction,
    IssueTransactionV2,
    CreateAliasTransaction,
    ReissueTransactionV2,
    BurnTransactionV2,
    ExchangeTransactionV2,
    LeaseCancelTransactionV2,
    SponsorFeeTransaction,
    SetAssetScriptTransaction,
    InvokeScriptTransaction,
    TransferTransaction
  ).flatMap { x =>
    x.supportedVersions.map { version =>
      ((x.typeId, version), x)
    }
  }(collection.breakOut)

  val all: Map[(Byte, Byte), TransactionParserLite] = old.flatMap {
    case (typeId, builder) =>
      builder.supportedVersions.map { version =>
        ((typeId, version), builder)
      }
  } ++ modern

  val byName: Map[String, TransactionParserLite] = (old ++ modern).map {
    case (_, builder) => builder.classTag.runtimeClass.getSimpleName -> builder
  }

  def by(name: String): Option[TransactionParserLite]                     = byName.get(name)
  def by(typeId: Byte, version: TxVersion): Option[TransactionParserLite] = all.get((typeId, version))

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
