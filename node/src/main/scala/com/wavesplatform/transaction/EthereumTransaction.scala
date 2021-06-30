package com.wavesplatform.transaction

import com.wavesplatform.account.{Address, EthereumAddress}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.PBRecipients
import monix.eval.Coeval
import org.web3j.abi.TypeDecoder
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.abi.datatypes.{Address => EthAddress}
import org.web3j.crypto.{SignedRawTransaction, TransactionEncoder}
import org.web3j.rlp.{RlpEncoder, RlpList}
import org.web3j.utils.Numeric._
import play.api.libs.json._
import scorex.crypto.hash.Keccak256

import scala.reflect.ClassTag

abstract class EthereumTransaction(underlying: SignedRawTransaction) extends Transaction(TransactionType.Ethereum) {

  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(RlpEncoder.encode(new RlpList(TransactionEncoder.asRlpValues(underlying, underlying.getSignatureData))))
  override val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(Keccak256.hash(bytes())))

  override def assetFee: (Asset, Long) = Asset.Waves -> underlying.getGasLimit.longValueExact()

  override val timestamp: TxTimestamp = underlying.getNonce.longValueExact()

  override val chainId: Byte = underlying.getChainId.byteValue()

  override val json: Coeval[JsObject] = Coeval.evalOnce(Json.obj())
}

object EthereumTransaction {
  private val decodeMethod = {
    val m = classOf[TypeDecoder].getDeclaredMethod("decode", classOf[String], classOf[Int], classOf[Class[_]])
    m.setAccessible(true)
    m
  }

  private def decode[A](source: String, offset: Int)(implicit ct: ClassTag[A]): A =
    decodeMethod.invoke(null, source, offset, ct.runtimeClass.asInstanceOf[Class[A]]).asInstanceOf[A]

  class Transfer(val sender: Address, val asset: Asset, val amount: TxAmount, val recipient: EthereumAddress, underlying: SignedRawTransaction)
      extends EthereumTransaction(underlying) {}

  def apply(underlying: SignedRawTransaction): EthereumTransaction = {
    val hexData       = cleanHexPrefix(underlying.getData)
    val senderAddress = PBRecipients.toAddress(hexStringToByteArray(underlying.getFrom), underlying.getChainId.toByte).explicitGet()
    if (hexData.isEmpty) {
      new Transfer(
        senderAddress,
        Asset.Waves,
        underlying.getValue.longValueExact(),
        new EthereumAddress(hexStringToByteArray(underlying.getTo)),
        underlying
      )
    } else if (hexData.startsWith("a9059cbb")) {
      val amount    = decode[Uint256](hexData, 72)
      val recipient = decode[EthAddress](hexData, 8)
      new Transfer(
        senderAddress,
        Asset.IssuedAsset(ByteStr(hexStringToByteArray(underlying.getTo))),
        amount.getValue.longValueExact(),
        new EthereumAddress(hexStringToByteArray(recipient.toString)),
        underlying
      )
    } else throw new UnsupportedOperationException
  }
}
