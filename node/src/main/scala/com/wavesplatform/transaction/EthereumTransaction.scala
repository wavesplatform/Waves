package com.wavesplatform.transaction

import java.math.BigInteger

import scala.reflect.ClassTag
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.TransactionType.TransactionType
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.serialization.impl.BaseTxJson
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.validation.{TxConstraints, TxValidator, ValidatedV}
import com.wavesplatform.utils.EthEncoding
import monix.eval.Coeval
import org.web3j.abi.TypeDecoder
import org.web3j.abi.datatypes.{Address => EthAddress}
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.crypto._
import org.web3j.crypto.Sign.SignatureData
import org.web3j.utils.Convert
import play.api.libs.json._

final case class EthereumTransaction(
    payload: EthereumTransaction.Payload,
    underlying: RawTransaction,
    signatureData: SignatureData,
    override val chainId: Byte
) extends Transaction(TransactionType.Ethereum)
    with Authorized
    with VersionedTransaction.ConstV1
    with PBSince.V1 {
  import EthereumTransaction._

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(encodeTransaction(underlying, signatureData))

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(TransactionEncoder.encode(underlying, chainId.toLong))

  override val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(Hash.sha3(bodyBytes())))

  override def assetFee: (Asset, Long) = Asset.Waves -> underlying.getGasLimit.longValueExact()

  override val timestamp: TxTimestamp = underlying.getNonce.longValueExact()

  val signerPublicKey: Coeval[PublicKey] = Coeval.evalOnce {
    require(signatureData != null, "empty signature data")
    val v          = BigInt(1, signatureData.getV)
    val recoveryId = if (v > 28) v - chainId * 2 - 35 else v - 27
    val sig        = new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS))

    PublicKey(
      ByteStr(
        Sign
          .recoverFromSignature(recoveryId.intValue, sig, id().arr)
          .toByteArray
          .takeRight(EthereumKeyLength)
      )
    )
  }

  val senderAddress: Coeval[Address] = Coeval.evalOnce(signerPublicKey().toAddress(chainId))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    BaseTxJson.toJson(this) ++ Json.obj(
      "bytes"           -> EthEncoding.toHexString(bytes()),
      "sender"          -> senderAddress().toString,
      "senderPublicKey" -> signerPublicKey()
    )
  )

  override val sender: PublicKey = signerPublicKey()
}

object EthereumTransaction {
  sealed trait Payload

  case class Transfer(tokenAddress: Option[ERC20Address], amount: Long, recipient: Address) extends Payload {
    def tryResolveAsset(blockchain: Blockchain): Either[ValidationError, Asset] =
      tokenAddress
        .fold[Either[ValidationError, Asset]](
          Right(Waves)
        )(a => blockchain.resolveERC20Address(a).toRight(GenericError(s"Can't resolve ERC20 address $a")))
  }

  case class Invocation(dApp: Address, hexCallData: String) extends Payload {
    def toInvokeScriptLike(tx: EthereumTransaction, blockchain: Blockchain): Either[ValidationError, InvokeScriptTransactionLike] =
      for {
        scriptInfo <- blockchain.accountScript(dApp).toRight(GenericError(s"No script at address $dApp"))
        (extractedCall, extractedPayments) = ABIConverter(scriptInfo.script).decodeFunctionCall(hexCallData)
      } yield
        new InvokeScriptTransactionLike {
          override def funcCall: Terms.FUNCTION_CALL                  = extractedCall
          override def payments: Seq[InvokeScriptTransaction.Payment] = extractedPayments
          override def id: Coeval[ByteStr]                            = tx.id
          override def dApp: AddressOrAlias                           = Invocation.this.dApp
          override val sender: PublicKey                              = tx.signerPublicKey()
          override def root: InvokeScriptTransactionLike              = this
          override def assetFee: (Asset, TxTimestamp)                 = tx.assetFee
          override def timestamp: TxTimestamp                         = tx.timestamp
          override def chainId: TxVersion                             = tx.chainId
          override def checkedAssets: Seq[Asset.IssuedAsset]          = this.paymentAssets
          override val tpe: TransactionType                           = TransactionType.InvokeScript
        }
  }

  implicit object EthereumTransactionValidator extends TxValidator[EthereumTransaction] {
    override def validate(tx: EthereumTransaction): ValidatedV[EthereumTransaction] = TxConstraints.seq(tx)(
      TxConstraints.fee(tx.underlying.getGasLimit.longValueExact()),
      TxConstraints
        .positiveOrZeroAmount((BigInt(tx.underlying.getValue) / AmountMultiplier).bigInteger.longValueExact(), "waves"),
      TxConstraints.cond(tx.underlying.getGasPrice == GasPrice, GenericError("Gas price must be 10 Gwei")),
      TxConstraints.cond(
        tx.underlying.getValue != BigInteger.ZERO || EthEncoding.cleanHexPrefix(tx.underlying.getData).nonEmpty,
        GenericError("Transaction cancellation is not supported")
      ),
      TxConstraints
        .cond(tx.underlying.getData.isEmpty || BigInt(tx.underlying.getValue) == 0, GenericError("Transaction should have either data or value")),
      tx.payload match {
        case Transfer(tokenAddress, amount, _) =>
          TxConstraints.positiveAmount(amount, tokenAddress.fold("waves")(erc20 => EthEncoding.toHexString(erc20.arr)))
        case Invocation(_, _) => TxConstraints.seq(tx)()
      }
    )
  }

  val GasPrice: BigInteger = Convert.toWei("10", Convert.Unit.GWEI).toBigInteger

  val AmountMultiplier = 10000000000L

  private val decodeMethod = {
    val m = classOf[TypeDecoder].getDeclaredMethod("decode", classOf[String], classOf[Int], classOf[Class[_]])
    m.setAccessible(true)
    m
  }

  private def decode[A](source: String, offset: Int)(implicit ct: ClassTag[A]): A =
    decodeMethod.invoke(null, source, offset, ct.runtimeClass.asInstanceOf[Class[A]]).asInstanceOf[A]

  private val encodeMethod = {
    val m = classOf[TransactionEncoder].getDeclaredMethod("encode", classOf[RawTransaction], classOf[SignatureData])
    m.setAccessible(true)
    m
  }

  private def encodeTransaction(tx: RawTransaction, signatureData: SignatureData): Array[Byte] =
    encodeMethod.invoke(null, tx, signatureData).asInstanceOf[Array[Byte]]

  def apply(bytes: Array[Byte]): Either[ValidationError, EthereumTransaction] =
    apply(TransactionDecoder.decode(EthEncoding.toHexString(bytes)).asInstanceOf[SignedRawTransaction])

  val ERC20TransferPrefix: String = "a9059cbb"

  def extractPayload(underlying: RawTransaction): Payload = {
    val hexData          = EthEncoding.cleanHexPrefix(underlying.getData)
    val recipientAddress = ByteStr(EthEncoding.toBytes(underlying.getTo))
    if (hexData.isEmpty) {
      Transfer(
        None,
        (BigInt(underlying.getValue) / AmountMultiplier).bigInteger.longValueExact(),
        Address(recipientAddress.arr)
      )
    } else if (hexData.startsWith(ERC20TransferPrefix)) {
      val recipient = decode[EthAddress](hexData, 8)
      val amount    = decode[Uint256](hexData, 72)
      Transfer(
        Some(ERC20Address(recipientAddress)),
        amount.getValue.longValueExact(),
        Address(EthEncoding.toBytes(recipient.toString))
      )
    } else Invocation(Address(recipientAddress.arr), hexData)
  }

  def apply(underlying: RawTransaction): Either[ValidationError, EthereumTransaction] =
    new EthereumTransaction(
      extractPayload(underlying),
      underlying,
      new SignatureData(Array.emptyByteArray, Array.emptyByteArray, Array.emptyByteArray),
      AddressScheme.current.chainId
    ).validatedEither

  def apply(underlying: SignedRawTransaction): Either[ValidationError, EthereumTransaction] =
    new EthereumTransaction(
      extractPayload(underlying),
      underlying,
      underlying.getSignatureData,
      Option(underlying.getChainId).fold(AddressScheme.current.chainId)(_.toByte)
    ).validatedEither
}