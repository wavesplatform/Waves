package com.wavesplatform.transaction

import java.math.BigInteger

import scala.reflect.ClassTag

import com.wavesplatform.account.{Address, AddressScheme, EthereumAddress, PublicKey, Recipient}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.{Height, TxNum}
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import monix.eval.Coeval
import org.bouncycastle.util.encoders.Hex
import org.web3j.abi.TypeDecoder
import org.web3j.abi.datatypes.{Address => EthAddress}
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.crypto._
import org.web3j.rlp.{RlpEncoder, RlpList}
import org.web3j.utils.Numeric._
import play.api.libs.json._

sealed abstract class EthereumTransaction(final val underlying: SignedRawTransaction) extends Transaction(TransactionType.Ethereum) {
  private final val signatureData: Sign.SignatureData = underlying.getSignatureData
  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(RlpEncoder.encode(new RlpList(TransactionEncoder.asRlpValues(underlying, signatureData))))

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    underlying.getEncodedTransaction(AddressScheme.current.chainId.toLong)
  }

  override val id: Coeval[ByteStr] = bytes.map(bs => ByteStr(Hash.sha3(bs)))

  override def assetFee: (Asset, Long) = Asset.Waves -> underlying.getGasLimit.longValueExact()

  override val timestamp: TxTimestamp = underlying.getNonce.longValueExact()

  override val protoSize: Coeval[Int] = bytes.map(_.length)

  override val chainId: Byte = underlying.getChainId.byteValue()

  val signerPublicKey: Coeval[Array[Byte]] = bodyBytes.map { bs =>
    Sign
      .recoverFromSignature(
        1,
        new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS)),
        Hash.sha3(bs)
      )
      .toByteArray
  }

  val signatureValid: Coeval[Boolean] = signerPublicKey.map(_ => true)

  val baseJson: Coeval[JsObject] = for {
    idValue <- id
  } yield Json.obj(
    "id"                  -> idValue.toString,
    "type"                -> tpe.id,
    "ethereumTransaction" -> ethereumJson(None, None, None)
  )

  def ethereumJson(blockId: Option[BlockId], height: Option[Height], num: Option[TxNum]): JsObject = Json.obj(
    "blockHash"        -> blockId.map(id => toHexString(id.arr)),
    "blockNumber"      -> height.map(h => toHexStringWithPrefix(BigInteger.valueOf(h))),
    "from"             -> EthereumAddress(PublicKey(signerPublicKey())).toString,
    "gas"              -> toHexStringWithPrefix(underlying.getGasLimit),
    "gasPrice"         -> toHexStringWithPrefix(underlying.getGasPrice),
    "hash"             -> toHexString(id().arr),
    "input"            -> underlying.getData,
    "nonce"            -> toHexStringWithPrefix(underlying.getNonce),
    "to"               -> underlying.getTo,
    "transactionIndex" -> num.map(n => toHexStringWithPrefix(BigInteger.valueOf(n))),
    "value"            -> toHexStringWithPrefix(underlying.getValue),
    "v"                -> toHexString(underlying.getSignatureData.getV),
    "r"                -> toHexString(underlying.getSignatureData.getR),
    "s"                -> toHexString(underlying.getSignatureData.getS)
  )
}

object EthereumTransaction {
  val AmountMultiplier = 10000000000L

  private val decodeMethod = {
    val m = classOf[TypeDecoder].getDeclaredMethod("decode", classOf[String], classOf[Int], classOf[Class[_]])
    m.setAccessible(true)
    m
  }

  private def decode[A](source: String, offset: Int)(implicit ct: ClassTag[A]): A =
    decodeMethod.invoke(null, source, offset, ct.runtimeClass.asInstanceOf[Class[A]]).asInstanceOf[A]

  class Transfer(
      val sender: Address,
      val asset: Either[Asset.Waves.type, ERC20Address],
      val amount: TxAmount,
      val recipient: EthereumAddress,
      underlying: SignedRawTransaction
  ) extends EthereumTransaction(underlying) {
    override val json: Coeval[JsObject] = baseJson.map(
      _ ++ Json.obj(
        "transfer" -> Json.obj(
          "sender"    -> sender.asWaves.toString,
          "recipient" -> recipient.toString,
          "amount"    -> amount,
          "asset" -> (asset match {
            case Left(_)      => JsNull
            case Right(erc20) => toHexString(erc20.arr)
          })
        )
      )
    )
  }

  class InvokeScript(
      val senderAddress: Address,
      val dApp: Recipient,
      val callData: ByteStr,
      underlying: SignedRawTransaction
  ) extends EthereumTransaction(underlying) {
    private[this] def hexCallData: String = Hex.toHexString(callData.arr)

    final class Invokable(script: Script) extends InvokeScriptTransactionLike {
      lazy val (funcCall, payments)             = ABIConverter(script).decodeFunctionCall(hexCallData)
      def dApp: Recipient                       = InvokeScript.this.dApp
      def root: Option[InvokeScriptTransaction] = None
      def senderAddress: Address                = InvokeScript.this.senderAddress
      def sender: PublicKey                     = PublicKey(signerPublicKey())
      def id: Coeval[BlockId]                   = InvokeScript.this.id
      val (feeAssetId, fee)                     = InvokeScript.this.assetFee
      def checkedAssets: Seq[Asset.IssuedAsset] = payments.collect { case InvokeScriptTransaction.Payment(_, asset: IssuedAsset) => asset }
      def transaction: InvokeScript             = InvokeScript.this
    }

    def toInvokable(script: Script): Invokable = new Invokable(script)

    override val json: Coeval[JsObject] = baseJson.map(
      _ ++ Json.obj("invokeScript" -> Json.obj("sender" -> senderAddress.asWaves.toString, "dApp" -> dApp.toString, "callData" -> hexCallData))
    )
  }

  def apply(bytes: Array[Byte]): EthereumTransaction =
    apply(TransactionDecoder.decode(toHexString(bytes)).asInstanceOf[SignedRawTransaction])

  val ERC20TransferPrefix: String = "a9059cbb"

  def apply(underlying: SignedRawTransaction): EthereumTransaction = {
    val hexData          = cleanHexPrefix(underlying.getData)
    val senderAddress    = PBRecipients.toAddress(hexStringToByteArray(underlying.getFrom), underlying.getChainId.toByte).explicitGet()
    val recipientAddress = ByteStr(hexStringToByteArray(underlying.getTo))
    if (hexData.isEmpty) {
      new Transfer(
        senderAddress,
        Left(Asset.Waves),
        underlying.getValue.divide(BigInt(AmountMultiplier).bigInteger).longValueExact(),
        new EthereumAddress(recipientAddress.arr),
        underlying
      )
    } else if (hexData.startsWith(ERC20TransferPrefix)) {
      val recipient = decode[EthAddress](hexData, 8)
      val amount    = decode[Uint256](hexData, 72)
      new Transfer(
        senderAddress,
        Right(ERC20Address(recipientAddress)),
        amount.getValue.longValueExact(),
        new EthereumAddress(hexStringToByteArray(recipient.toString)),
        underlying
      )
    } else new InvokeScript(senderAddress, new EthereumAddress(recipientAddress.arr), ByteStr(Hex.decode(hexData)), underlying)
  }
}
