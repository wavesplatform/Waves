package com.wavesplatform.transaction

import com.wavesplatform.crypto._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransactionV1, ExchangeTransactionV2}
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransactionV1, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.base58Length

import scala.util.Try

object TransactionParsers {

  val TimestampLength            = 8
  val AmountLength               = 8
  val TypeLength                 = 1
  val SignatureStringLength: Int = base58Length(SignatureLength)

  private val old: Map[Byte, TransactionParserLite] = Seq[TransactionParser](
    GenesisTransaction,
    PaymentTransaction,
    IssueTransactionV1,
    ReissueTransactionV1,
    BurnTransactionV1,
    ExchangeTransactionV1,
    LeaseTransactionV1,
    LeaseCancelTransactionV1,
    CreateAliasTransactionV1,
    MassTransferTransaction
  ).map { x =>
    x.typeId -> x
  }(collection.breakOut)

  private val modern: Map[(Byte, Byte), TransactionParserLite] = Seq[TransactionParser](
    DataTransaction,
    SetScriptTransaction,
    IssueTransactionV2,
    CreateAliasTransactionV2,
    ReissueTransactionV2,
    BurnTransactionV2,
    ExchangeTransactionV2,
    LeaseTransactionV2,
    LeaseCancelTransactionV2,
    SponsorFeeTransaction,
    SetAssetScriptTransaction,
    InvokeScriptTransaction
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

  def by(name: String): Option[TransactionParserLite]                = byName.get(name)
  def by(typeId: Byte, version: Byte): Option[TransactionParserLite] = all.get((typeId, version))

  def parseBytes(bytes: Array[Byte]): Try[Transaction] = {
    require(bytes.length > 2, "Buffer underflow while parsing transaction")
    val parser = if (bytes(0) == 0) {
      val typeId = bytes(1)
      val version = bytes(2)
      modern.getOrElse(
        (typeId, version),
        throw new IllegalArgumentException(s"Unknown transaction type ($typeId) and version ($version) (modern encoding)")
      )

    } else {
      old.getOrElse(bytes(0), throw new IllegalArgumentException(s"Unknown transaction type (old encoding): '${bytes(0)}'"))
    }
    parser.parseBytes(bytes)
  }

  // todo: (NODE-1915) Used in tests
  def forTypeSet(types: Set[Byte]): Set[TransactionParserLite] =
    (all.values.toList :+ TransferTransaction.transactionParserStub).filter(tp => types.contains(tp.typeId)).toSet

  def allVersions(parsers: TransactionParser*): Set[TransactionParserLite] =
    forTypeSet(parsers.map(_.typeId).toSet)
}
