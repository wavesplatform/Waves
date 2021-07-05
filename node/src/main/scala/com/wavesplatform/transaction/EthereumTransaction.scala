package com.wavesplatform.transaction

import java.math.BigInteger

import com.wavesplatform.account.{Address, AddressScheme, EthereumAddress}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.{Height, TxNum}
import monix.eval.Coeval
import org.web3j.abi.TypeDecoder
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.abi.datatypes.{Address => EthAddress}
import org.web3j.crypto._
import org.web3j.rlp.{RlpEncoder, RlpList}
import org.web3j.utils.Numeric._
import play.api.libs.json._

import scala.reflect.ClassTag

abstract class EthereumTransaction(final val underlying: SignedRawTransaction) extends Transaction(TransactionType.Ethereum) {
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

  val senderAddress: Coeval[EthereumAddress] = signerPublicKey.map { pk =>
    new EthereumAddress(Keys.getAddress(pk))
  }

  val signatureValid: Coeval[Boolean] = senderAddress.map { _ =>
    true
  }

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
    "from"             -> senderAddress().toString,
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

  def apply(bytes: Array[Byte]): EthereumTransaction =
    apply(TransactionDecoder.decode(toHexString(bytes)).asInstanceOf[SignedRawTransaction])

  val ERC20TransferPrefix: String = "a9059cbb"

  def apply(underlying: SignedRawTransaction): EthereumTransaction = {
    val hexData       = cleanHexPrefix(underlying.getData)
    val senderAddress = PBRecipients.toAddress(hexStringToByteArray(underlying.getFrom), underlying.getChainId.toByte).explicitGet()
    if (hexData.isEmpty) {
      new Transfer(
        senderAddress,
        Left(Asset.Waves),
        underlying.getValue.divide(BigInt(AmountMultiplier).bigInteger).longValueExact(),
        new EthereumAddress(hexStringToByteArray(underlying.getTo)),
        underlying
      )
    } else if (hexData.startsWith(ERC20TransferPrefix)) {
      val amount    = decode[Uint256](hexData, 72)
      val recipient = decode[EthAddress](hexData, 8)
      new Transfer(
        senderAddress,
        Right(ERC20Address(ByteStr(hexStringToByteArray(underlying.getTo)))),
        amount.getValue.longValueExact(),
        new EthereumAddress(hexStringToByteArray(recipient.toString)),
        underlying
      )
    } else throw new UnsupportedOperationException
  }
}
