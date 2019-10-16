package com.wavesplatform.transaction

import com.wavesplatform.transaction.transfer.TransferTransaction

import scala.util.{Failure, Success, Try}

// todo: (NODE-1915) Relevant name
object NewTransactionParsers {

  private val parsers: Map[TransactionType, NewTransactionParser[_ <: Transaction]] = Map(
    TransferTransaction.transactionType -> TransferTransaction
  )

  def parseBytes(data: Array[Byte]): Try[Transaction] =
    for {
      txType <- parseTypeId(data)
      tx: Transaction <- parsers.get(txType) match {
        case Some(parser) => parser.parseBytes(data)
        case None         => Failure(new UnsupportedOperationException(s"Unknown transaction type ($txType)"))
      }
    } yield tx

  def parseTypeId(bytes: Array[Byte]): Try[TransactionType] =
    bytes match {
      case modernTypeId(typeId) => Success(typeId)
      case oldTypeId(typeId)    => Success(typeId)
      case data if data.isEmpty => Failure(new IllegalArgumentException("Can't find the significant byte: the buffer is empty"))
      case data                 => Failure(new IllegalArgumentException(s"Can't determine the type of transaction: the buffer has ${data.length} bytes"))
    }

  private[this] object modernTypeId {
    def unapply(bytes: Array[Byte]): Option[TransactionType] =
      bytes match {
        case data if data.length >= 2 && data.head == 0 => Some(TransactionType(data(1)))
        case _                                          => None
      }
  }

  private[this] object oldTypeId {
    def unapply(bytes: Array[Byte]): Option[TransactionType] =
      bytes match {
        case data if data.nonEmpty => Some(TransactionType(data.head))
        case _                     => None
      }
  }
}
