package com.wavesplatform.transaction.smart

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CaseObj, EVALUATED, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, Serde}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset._
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class InvokeScriptTransaction private (chainId: Byte,
                                            sender: PublicKey,
                                            dAppAddressOrAlias: AddressOrAlias,
                                            funcCallOpt: Option[Terms.FUNCTION_CALL],
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

  val funcCall = funcCallOpt.getOrElse(FUNCTION_CALL(FunctionHeader.User(ContractEvaluator.DEFAULT_FUNC_NAME), List.empty))

  override val builder: TransactionParser = InvokeScriptTransaction

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        sender,
        dAppAddressOrAlias.bytes.arr,
        Deser.serializeOption(funcCallOpt)(Serde.serialize(_)),
        Deser.serializeArrays(payment.map(pmt => Longs.toByteArray(pmt.amount) ++ pmt.assetId.byteRepr)),
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
          "version" -> version,
          "dApp"    -> dAppAddressOrAlias.stringRepr,
          "payment" -> payment
        )
        ++ (funcCallOpt match {
          case Some(fc) => Json.obj("call" -> InvokeScriptTransaction.functionCallToJson(fc))
          case None     => JsObject.empty
        })
    )

  override def checkedAssets(): Seq[IssuedAsset] = payment.toSeq collect { case Payment(_, assetId: IssuedAsset) => assetId }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 1
}

object InvokeScriptTransaction extends TransactionParserFor[InvokeScriptTransaction] with TransactionParser.MultipleVersions {

  import play.api.libs.json.{Json, _}

  case class Payment(amount: Long, assetId: Asset)

  implicit val paymentPartFormat: Format[InvokeScriptTransaction.Payment] = Json.format

  def functionCallToJson(fc: Terms.FUNCTION_CALL): JsObject = {
    Json.obj(
      "function" -> JsString(fc.function.asInstanceOf[com.wavesplatform.lang.v1.FunctionHeader.User].internalName),
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
      validate(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKey,
             dappAddress: AddressOrAlias,
             fc: Option[Terms.FUNCTION_CALL],
             p: Seq[Payment],
             fee: Long,
             feeAssetId: Asset,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- validate(fc, p, fee)
      tx   = new InvokeScriptTransaction(currentChainId, sender, dappAddress, fc, p, fee, feeAssetId, timestamp, proofs)
      size = tx.bytes().length
      _ <- Either.cond(size <= ContractLimits.MaxInvokeScriptSizeInBytes, (), TooBigArray)
    } yield tx
  }

  private def checkAmounts(payments: Seq[Payment]): Either[NonPositiveAmount, Unit] =
    payments
      .find(_.amount <= 0)
      .fold(().asRight[NonPositiveAmount])(
        p =>
          NonPositiveAmount(
            p.amount,
            p.assetId.fold("Waves")(_.toString)
          ).asLeft[Unit])

  def signed(sender: PublicKey,
             dappAddress: AddressOrAlias,
             fc: Option[Terms.FUNCTION_CALL],
             p: Seq[Payment],
             fee: Long,
             feeAssetId: Asset,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] =
    for {
      tx     <- create(sender, dappAddress, fc, p, fee, feeAssetId, timestamp, Proofs.empty)
      proofs <- Proofs.create(Seq(ByteStr(crypto.sign(signer, tx.bodyBytes()))))
    } yield tx.copy(proofs = proofs)

  def selfSigned(sender: KeyPair,
                 dappAddress: AddressOrAlias,
                 fc: Option[Terms.FUNCTION_CALL],
                 p: Seq[Payment],
                 fee: Long,
                 feeAssetId: Asset,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, dappAddress, fc, p, fee, feeAssetId, timestamp, sender)
  }

  def validate(tx: InvokeScriptTransaction): Either[ValidationError, Unit] = {
    for {
      _ <- Either.cond(tx.chainId == currentChainId, (), GenericError(s"Wrong chainId ${tx.chainId.toInt}"))
      _ <- validate(tx.funcCallOpt, tx.payment, tx.fee)
      _ <- Either.cond(tx.bytes().length <= ContractLimits.MaxInvokeScriptSizeInBytes, (), TooBigArray)
    } yield ()
  }

  def validate(fc: Option[Terms.FUNCTION_CALL], p: Seq[Payment], fee: Long): Either[ValidationError, Unit] = {
    for {
      _ <- Either.cond(fee > 0, (), InsufficientFee(s"insufficient fee: $fee"))
      _ <- Either.cond(
        fc.isEmpty || fc.get.args.size <= ContractLimits.MaxInvokeScriptArgs,
        (),
        GenericError(s"InvokeScript can't have more than ${ContractLimits.MaxInvokeScriptArgs} arguments")
      )
      _ <- Either.cond(
        fc.isEmpty || (fc.get.function match {
          case FunctionHeader.User(internalName, _) =>
            internalName.getBytes("UTF-8").length <= ContractLimits.MaxAnnotatedFunctionNameInBytes
          case _ => true
        }),
        (),
        GenericError(s"Callable function name size in bytes must be less than ${ContractLimits.MaxAnnotatedFunctionNameInBytes} bytes")
      )
      _ <- checkAmounts(p)
      _ <- Either.cond(p.length <= 1, (), GenericError("Multiple payment isn't allowed now"))
      _ <- Either.cond(p.map(_.assetId).distinct.length == p.length, (), GenericError("duplicate payments"))
      _ <- Either.cond(
        fc.isEmpty || fc.get.args.forall(x => x.isInstanceOf[EVALUATED] && !x.isInstanceOf[CaseObj] && !x.isInstanceOf[ARR]),
        (),
        GenericError("All arguments of invokeScript must be one of the types: Int, ByteVector, String, Boolean")
      )
    } yield ()
  }

  val byteTailDescription: ByteEntity[InvokeScriptTransaction] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyBytes(tailIndex(2), "Sender's public key"),
      AddressOrAliasBytes(tailIndex(3), "Contract address or alias"),
      OptionBytes(tailIndex(4), "Function call", FunctionCallBytes(tailIndex(4), "Function call")),
      SeqBytes(tailIndex(5), "Payments", PaymentBytes(tailIndex(5), "Payment")),
      LongBytes(tailIndex(6), "Fee"),
      OptionBytes(tailIndex(7), "Fee's asset ID", AssetIdBytes(tailIndex(7), "Fee's asset ID"), "flag (1 - asset, 0 - Waves)")
        .map(_.getOrElse(Waves)),
      LongBytes(tailIndex(8), "Timestamp"),
      ProofsBytes(tailIndex(9))
    ) mapN InvokeScriptTransaction.apply
  }
}
