package com.wavesplatform.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.crypto
import com.wavesplatform.crypto.KeyLength
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FUNCTION_CALL}
import com.wavesplatform.state.{ByteStr, _}
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import monix.eval.Coeval
import play.api.libs.json.Json

import scala.util.{Failure, Success, Try}

case class ContractInvocationTransaction private (version: Byte,
                                                  chainId: Byte,
                                                  sender: PublicKeyAccount,
                                                  contractAddress: Address,
                                                  fc: Terms.FUNCTION_CALL,
                                                  fee: Long,
                                                  timestamp: Long,
                                                  proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = ContractInvocationTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      contractAddress.bytes.arr,
      Serde.serialize(fc),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val json = Coeval.evalOnce(
    jsonBase()
      ++ Json.obj("version" -> version, "contractAddress" -> contractAddress.bytes) ++ ContractInvocationTransaction.functionCallToJson(fc))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object ContractInvocationTransaction extends TransactionParserFor[ContractInvocationTransaction] with TransactionParser.MultipleVersions {

  import play.api.libs.json._
  def functionCallToJson(fc: Terms.FUNCTION_CALL) = Json.obj(
    "function" -> JsString(fc.function.asInstanceOf[com.wavesplatform.lang.v1.FunctionHeader.User].name),
    "args" -> JsArray(fc.args.map {
      case Terms.CONST_LONG(l)       => Json.obj("key" -> "", "type" -> "integer", "value" -> l)
      case Terms.CONST_BOOLEAN(l)    => Json.obj("key" -> "", "type" -> "boolean", "value" -> l)
      case Terms.CONST_BYTEVECTOR(l) => Json.obj("key" -> "", "type" -> "binary", "value" -> ByteStr(l.toArray).base64)
      case Terms.CONST_STRING(l)     => Json.obj("key" -> "", "type" -> "string", "value" -> l)
      case _                         => ???
    })
  )

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1)

  private def networkByte = AddressScheme.current.chainId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val chainId         = bytes(0)
      val sender          = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
      val contractAddress = Address.fromBytes(bytes.drop(KeyLength + 1).take(Address.AddressLength)).explicitGet()
      val fcStart         = KeyLength + 1 + Address.AddressLength
      val rest            = bytes.drop(fcStart)
      val (fc, remaining) = Serde.deserialize(rest, all = false).explicitGet()
      val feeTsProofs     = rest.takeRight(remaining)
      val fee             = Longs.fromByteArray(feeTsProofs.slice(0, 8))
      val timestamp       = Longs.fromByteArray(feeTsProofs.slice(8, 16))
      (for {
        _      <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
        proofs <- Proofs.fromBytes(feeTsProofs.drop(16))
        tx     <- create(version, sender, contractAddress, fc.asInstanceOf[FUNCTION_CALL], fee, timestamp, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             contractAddress: Address,
             fc: Terms.FUNCTION_CALL,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: $fee"))
      _ <- Either.cond(fc.args.forall(_.isInstanceOf[EVALUATED]), (), GenericError("all arguments of contractInvocation must be EVALUATED"))
    } yield new ContractInvocationTransaction(version, networkByte, sender, contractAddress, fc, fee, timestamp, proofs)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             contractAddress: Address,
             fc: Terms.FUNCTION_CALL,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, sender, contractAddress, fc, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 contractAddress: Address,
                 fc: Terms.FUNCTION_CALL,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(version, sender, contractAddress, fc, fee, timestamp, sender)
}
