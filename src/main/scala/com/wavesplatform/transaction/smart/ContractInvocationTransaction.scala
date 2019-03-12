package com.wavesplatform.transaction.smart

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, REF}
import com.wavesplatform.lang.v1.{ContractLimits, Serde}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset._
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.smart.ContractInvocationTransaction.Payment
import com.wavesplatform.utils.byteStrWrites
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class ContractInvocationTransaction private (chainId: Byte,
                                                  sender: PublicKeyAccount,
                                                  contractAddress: Address,
                                                  fc: Terms.FUNCTION_CALL,
                                                  payment: Option[Payment],
                                                  fee: Long,
                                                  timestamp: Long,
                                                  proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  import ContractInvocationTransaction.paymentPartFormat
  import play.api.libs.json.Json

  override val builder: TransactionParser = ContractInvocationTransaction

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        sender.publicKey,
        contractAddress.bytes.arr,
        Serde.serialize(fc),
        Deser.serializeOption(payment)(pmt => Longs.toByteArray(pmt.amount) ++ Deser.serializeOption(pmt.assetId.compatId)(_.arr)),
        Longs.toByteArray(fee),
        Longs.toByteArray(timestamp)
      )
    )

  override val assetFee: (Asset, Long) = (Waves, fee)
  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      jsonBase()
        ++ Json.obj(
          "version"         -> version,
          "contractAddress" -> contractAddress.bytes,
          "call"            -> ContractInvocationTransaction.functionCallToJson(fc),
          "payment"         -> payment
        )
    )

  override def checkedAssets(): Seq[Asset] = payment.toSeq.map(_.assetId)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 1
}

object ContractInvocationTransaction extends TransactionParserFor[ContractInvocationTransaction] with TransactionParser.MultipleVersions {

  import play.api.libs.json.{Json, _}

  case class Payment(amount: Long, assetId: Asset)

  implicit val paymentPartFormat: Format[ContractInvocationTransaction.Payment] = Json.format

  def functionCallToJson(fc: Terms.FUNCTION_CALL): JsObject = {
    Json.obj(
      "function" -> JsString(fc.function.asInstanceOf[com.wavesplatform.lang.v1.FunctionHeader.User].name),
      "args" -> JsArray(
        fc.args.map {
          case Terms.CONST_LONG(l)    => Json.obj("type" -> "integer", "value" -> l)
          case Terms.CONST_BOOLEAN(l) => Json.obj("type" -> "boolean", "value" -> l)
          case Terms.CONST_BYTESTR(l) => Json.obj("type" -> "binary", "value" -> l.base64)
          case Terms.CONST_STRING(l)  => Json.obj("type" -> "string", "value" -> l)
          case _                      => ???
        }
      )
    )
  }

  override val typeId: Byte                 = 16
  override val supportedVersions: Set[Byte] = Set(1)

  private def currentChainId: Byte = AddressScheme.current.chainId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == currentChainId, (), GenericError(s"Wrong chainId ${tx.chainId.toInt}"))
        .flatMap(_ => Either.cond(tx.fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: ${tx.fee}")))
        .flatMap(_ =>
          tx.payment match {
            case Some(Payment(amt, token)) => Either.cond(amt > 0, (), ValidationError.NegativeAmount(0, token.toString))
            case _                         => Right(())
        })
        .flatMap(_ =>
          Either.cond(tx.fc.args.forall(x => x.isInstanceOf[EVALUATED] || x == REF("unit")),
                      (),
                      GenericError("all arguments of contractInvocation must be EVALUATED")))
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKeyAccount,
             contractAddress: Address,
             fc: Terms.FUNCTION_CALL,
             p: Option[Payment],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: $fee"))
      _ <- Either.cond(
        fc.args.size <= ContractLimits.MaxContractInvocationArgs,
        (),
        ValidationError.GenericError(s"ContractInvocation can't have more than ${ContractLimits.MaxContractInvocationArgs} arguments")
      )
      _ <- p match {
        case Some(Payment(amt, token)) => Either.cond(amt > 0, (), ValidationError.NegativeAmount(0, token.toString))
        case _                         => Right(())
      }

      _ <- Either.cond(fc.args.forall(x => x.isInstanceOf[EVALUATED] || x == REF("unit")),
                       (),
                       GenericError("all arguments of contractInvocation must be EVALUATED"))
      tx   = new ContractInvocationTransaction(currentChainId, sender, contractAddress, fc, p, fee, timestamp, proofs)
      size = tx.bytes().length
      _ <- Either.cond(size <= ContractLimits.MaxContractInvocationSizeInBytes, (), ValidationError.TooBigArray)
    } yield tx
  }

  def signed(sender: PublicKeyAccount,
             contractAddress: Address,
             fc: Terms.FUNCTION_CALL,
             p: Option[Payment],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, contractAddress, fc, p, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 contractAddress: Address,
                 fc: Terms.FUNCTION_CALL,
                 p: Option[Payment],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, contractAddress, fc, p, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[ContractInvocationTransaction] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyAccountBytes(tailIndex(2), "Sender's public key"),
      AddressBytes(tailIndex(3), "Contract address"),
      FunctionCallBytes(tailIndex(4), "Function call"),
      OptionBytes(tailIndex(5), "Payment", PaymentBytes(tailIndex(5), "Payment")),
      LongBytes(tailIndex(6), "Fee"),
      LongBytes(tailIndex(7), "Timestamp"),
      ProofsBytes(tailIndex(8))
    ) mapN ContractInvocationTransaction.apply
  }
}
