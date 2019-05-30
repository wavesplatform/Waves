package com.wavesplatform.protobuf.transaction.dsl

import cats.data.{Validated, ValidatedNel}
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.PBTransaction.{Data => TXD}
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.state.DataEntry

object PBTransactionsDSL {
  import com.wavesplatform.protobuf.utils.PBInternalImplicits._

  final case class BaseFields(sender: PublicKey = PublicKey.empty,
                              fee: Amount = Amount.defaultInstance,
                              timestamp: Long = 0,
                              proofs: Seq[ByteStr] = Nil)

  final case class Builder(baseFields: BaseFields = BaseFields(), txData: TXD = TXD.Empty, version: Int = 1) {
    type Repr = Builder
    protected def withBaseFieldsObj(bf: BaseFields): Repr = copy(baseFields)

    def withBaseFields(sender: PublicKey = PublicKey.empty,
                       fee: Amount = Amount.defaultInstance,
                       timestamp: Long = 0,
                       proofs: Seq[ByteStr] = Nil): Repr =
      withBaseFieldsObj(BaseFields(sender, fee, timestamp, proofs))

    def withVersion(version: Int): Repr = copy(version = version)

    def withData(dataEntries: DataEntry[_]*): Repr =
      copy(txData = TXD.DataTransaction(DataTransactionData(dataEntries.map(PBTransactions.toPBDataEntry))))

    def withTransfer(recipient: Recipient, amount: Amount, attachment: ByteStr): Repr =
      copy(txData = TXD.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)))

    // TODO: Other types

    def result(): PBCachedTransaction =
      PBSignedTransaction(
        Some(PBTransaction(AddressScheme.current.chainId, baseFields.sender, Some(baseFields.fee), baseFields.timestamp, version)),
        baseFields.proofs.map(ByteString.copyFrom(_))
      )
  }

  object Matchers {
    sealed trait TxMatcher[T] {
      type R = T
      def unapply(arg: PBSignedTransaction): Option[R]
      final def unapply(arg: PBCachedTransaction): Option[R] = this.unapply(arg.transaction)
    }

    object Data extends TxMatcher[(BaseFields, Seq[DataEntry[_]])] {
      def unapply(arg: PBSignedTransaction): Option[R] = arg.getTransaction.data match {
        case TXD.DataTransaction(value) => Some((extractBF(arg), value.data.map(PBTransactions.toVanillaDataEntry)))
        case _                          => None
      }
    }

    object Transfer extends TxMatcher[(BaseFields, Recipient, Amount, ByteStr)] {
      def unapply(arg: PBSignedTransaction): Option[R] = arg.getTransaction.data match {
        case TXD.Transfer(value) => Some((extractBF(arg), value.getRecipient, value.getAmount, value.attachment))
        case _                   => None
      }
    }

    private[this] def extractBF(signedTx: PBSignedTransaction): BaseFields = {
      val tx = signedTx.getTransaction
      BaseFields(tx.senderPublicKey.publicKeyAccount, tx.getFee, tx.timestamp, signedTx.proofs.map(_.byteStr))
    }
  }

  object Implicits {
    implicit class PBCachedTransactionExt(private val tx: PBCachedTransaction) extends AnyVal {
      def validated(implicit verifier: TxValidator): ValidatedNel[ValidationError, PBCachedTransaction] = verifier.validate(tx).map(_ => tx)
    }
  }

  def newBuilder: Builder = Builder()

  import Implicits._

  // Example
  val tx = PBTransactionsDSL.newBuilder
    .withBaseFields(timestamp = 123)
    .withTransfer(PBRecipients.create(Address.fromString("123").right.get), PBAmounts.waves(123), Base58.decode("123"))
    .result()

  tx.validated match {
    case Validated.Valid(Matchers.Data(_, de)) =>
      println(de)

    case Validated.Valid(Matchers.Transfer(_, recipient, amount, _)) =>
      println(recipient + " " + amount)
  }
}
