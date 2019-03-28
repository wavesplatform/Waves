package com.wavesplatform.transaction.smart

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, REF}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, Serde}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset._
import com.wavesplatform.transaction.ValidationError.{GenericError, NonPositiveAmount}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.utils.byteStrWrites
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class InvokeScriptTransaction private (chainId: Byte,
                                            sender: PublicKeyAccount,
                                            dappAddress: Address,
                                            fc: Terms.FUNCTION_CALL,
                                            payment: Seq[Payment],
                                            fee: Long,
                                            feeAssetId: Asset,
                                            timestamp: Long,
                                            proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  import InvokeScriptTransaction.paymentPartFormat
  import play.api.libs.json.Json

  override val builder: TransactionParser = InvokeScriptTransaction

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        sender.publicKey,
        dappAddress.bytes.arr,
        Serde.serialize(fc),
        Deser.serializeArrays(payment.map(pmt => Longs.toByteArray(pmt.amount) ++ Deser.serializeOption(pmt.assetId.compatId)(_.arr))),
        Longs.toByteArray(fee),
        feeAssetId.byteRepr,
        Longs.toByteArray(timestamp)
      )
    )

  override val assetFee: (Asset, Long) = (feeAssetId, fee)
  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      jsonBase()
        ++ Json.obj(
          "version"     -> version,
          "dappAddress" -> dappAddress.bytes,
          "call"        -> InvokeScriptTransaction.functionCallToJson(fc),
          "payment"     -> payment
        )
    )

  override def checkedAssets(): Seq[Asset] = payment.toSeq.map(_.assetId)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 1
}

object InvokeScriptTransaction extends TransactionParserFor[InvokeScriptTransaction] with TransactionParser.MultipleVersions {

  import play.api.libs.json.{Json, _}

  case class Payment(amount: Long, assetId: Asset)

  implicit val paymentPartFormat: Format[InvokeScriptTransaction.Payment] = Json.format

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
          Either.cond(tx.payment.forall(_.amount > 0),
                      (),
                      ValidationError.NonPositiveAmount(0, tx.payment.find(_.amount <= 0).get.assetId.fold("Waves")(_.toString))))
        .flatMap(_ =>
          Either.cond(tx.fc.args.forall(x => x.isInstanceOf[EVALUATED] || x == REF("unit")),
                      (),
                      GenericError("all arguments of invokeScript must be EVALUATED")))
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKeyAccount,
             dappAddress: Address,
             fc: Terms.FUNCTION_CALL,
             p: Seq[Payment],
             fee: Long,
             feeAssetId: Asset,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: $fee"))
      _ <- Either.cond(
        fc.args.size <= ContractLimits.MaxInvokeScriptArgs,
        (),
        ValidationError.GenericError(s"InvokeScript can't have more than ${ContractLimits.MaxInvokeScriptArgs} arguments")
      )
      _ <- Either.cond(
        fc.function match {
          case FunctionHeader.User(name) => name.getBytes.length <= ContractLimits.MaxCallableFunctionNameInBytes
          case _                         => true
        },
        (),
        ValidationError.GenericError(s"Callable function name size in bytes must be less than ${ContractLimits.MaxCallableFunctionNameInBytes} bytes")
      )
      _ <- checkAmounts(p)
      _ <- Either.cond(p.length <= 1, (), ValidationError.GenericError("Multiple payment isn't allowed now"))
      _ <- Either.cond(p.map(_.assetId).distinct.length == p.length, (), ValidationError.GenericError("duplicate payments"))

      _ <- Either.cond(fc.args.forall(x => x.isInstanceOf[EVALUATED] || x == REF("unit")),
                       (),
                       GenericError("all arguments of invokeScript must be EVALUATED"))
      tx   = new InvokeScriptTransaction(currentChainId, sender, dappAddress, fc, p, fee, feeAssetId, timestamp, proofs)
      size = tx.bytes().length
      _ <- Either.cond(size <= ContractLimits.MaxInvokeScriptSizeInBytes, (), ValidationError.TooBigArray)
    } yield tx
  }

  private def checkAmounts(payments: Seq[Payment]): Either[NonPositiveAmount, Unit] =
    payments
      .find(_.amount <= 0)
      .fold(().asRight[NonPositiveAmount])(
        p =>
          ValidationError
            .NonPositiveAmount(
              p.amount,
              p.assetId.fold("Waves")(_.toString)
            )
            .asLeft[Unit])

  def signed(sender: PublicKeyAccount,
             dappAddress: Address,
             fc: Terms.FUNCTION_CALL,
             p: Seq[Payment],
             fee: Long,
             feeAssetId: Asset,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    for {
      tx     <- create(sender, dappAddress, fc, p, fee, feeAssetId, timestamp, Proofs.empty)
      proofs <- Proofs.create(Seq(ByteStr(crypto.sign(signer, tx.bodyBytes()))))
    } yield tx.copy(proofs = proofs)

  def selfSigned(sender: PrivateKeyAccount,
                 dappAddress: Address,
                 fc: Terms.FUNCTION_CALL,
                 p: Seq[Payment],
                 fee: Long,
                 feeAssetId: Asset,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, dappAddress, fc, p, fee, feeAssetId, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[InvokeScriptTransaction] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyAccountBytes(tailIndex(2), "Sender's public key"),
      AddressBytes(tailIndex(3), "Contract address"),
      FunctionCallBytes(tailIndex(4), "Function call"),
      SeqBytes(tailIndex(5), "Payment", PaymentBytes(tailIndex(5), "Payment")),
      LongBytes(tailIndex(6), "Fee"),
      OptionBytes(tailIndex(7), "Fee's asset ID", AssetIdBytes(tailIndex(7), "Fee's asset ID"), "flag (1 - asset, 0 - Waves)")
        .map(_.getOrElse(Waves)),
      LongBytes(tailIndex(8), "Timestamp"),
      ProofsBytes(tailIndex(9))
    ) mapN InvokeScriptTransaction.apply
  }
}
