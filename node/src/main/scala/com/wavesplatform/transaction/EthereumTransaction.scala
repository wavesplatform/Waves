package com.wavesplatform.transaction

import java.math.BigInteger

import com.wavesplatform.account._
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.state.{Blockchain, Height, TxNum}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.EthEncoding
import monix.eval.Coeval
import org.web3j.abi.TypeDecoder
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.abi.datatypes.{Address => EthAddress}
import org.web3j.crypto.Sign.SignatureData
import org.web3j.crypto._
import org.web3j.rlp.{RlpEncoder, RlpList, RlpString}
import play.api.libs.json._

import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

class EthereumTransaction(
    val payload: EthereumTransaction.Payload,
    val underlying: RawTransaction,
    val signatureData: SignatureData,
    override val chainId: Byte
) extends Transaction(TransactionType.Ethereum) {
  import EthereumTransaction._

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(encode(signatureData))

  private def encode(signatureData: SignatureData) = {
    underlying.getTransaction match {
      case _: org.web3j.crypto.transaction.`type`.Transaction1559 =>
        encodeTransaction(underlying, signatureData)
      case lt: org.web3j.crypto.transaction.`type`.LegacyTransaction =>
        val list = lt.asRlpValues(signatureData)
        if (signatureData == null) {
          list.addAll(Seq(RlpString.create(chainId.toLong), RlpString.create(0L), RlpString.create(0L)).asJava)
        }
        RlpEncoder.encode(new RlpList(list))

      case _ => ???
    }
  }

  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(encode(null))

  override val id: Coeval[ByteStr] = Coeval.evalOnce {
    ByteStr(Hash.sha3(bodyBytes()))
  }

  override def assetFee: (Asset, Long) = Asset.Waves -> underlying.getGasLimit.longValueExact()

  override val timestamp: TxTimestamp = underlying.getNonce.longValueExact()

  override val protoSize: Coeval[Int] = bytes.map(_.length)

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

  val senderAddress: Coeval[Address] = signerPublicKey.map(_.toAddress(chainId))

  val baseJson: Coeval[JsObject] = for {
    idValue <- id
  } yield Json.obj(
    "id"                  -> idValue.toString,
    "type"                -> tpe.id,
    "bytes" -> EthEncoding.toHexString(bytes()),
    "from" -> EthEncoding.toHexString(senderAddress().publicKeyHash)
  )

  def ethereumJson(blockId: Option[BlockId], height: Option[Height], num: Option[TxNum]): JsObject = Json.obj()

  override val json: Coeval[JsObject] = Coeval.evalOnce(baseJson())

  override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = payload.smartAssets(blockchain)
}

object EthereumTransaction {
  sealed trait Payload {
    def smartAssets(blockchain: Blockchain): Seq[IssuedAsset]
  }

  case class Transfer(asset: Either[Asset.Waves.type, ERC20Address], amount: Long, recipient: Address) extends Payload {
    override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = asset match {
      case Right(erc20) => blockchain.resolveERC20Address(erc20).filter(blockchain.hasAssetScript).toSeq
      case _            => Seq.empty
    }

    def json(senderAddress: Address): JsObject =
      Json.obj(
        "transfer" -> Json.obj(
          "sender"    -> senderAddress.toString,
          "recipient" -> recipient.toString,
          "amount"    -> amount,
          "asset" -> (asset match {
            case Left(_)      => JsNull
            case Right(erc20) => EthEncoding.toHexString(erc20.arr)
          })
        )
      )
  }

  case class Invocation(dApp: Address, hexCallData: String) extends Payload {
    override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = Seq.empty

    def toInvokeScriptLike(tx: EthereumTransaction, script: Script): InvokeScriptTransactionLike = new InvokeScriptTransactionLike {
      lazy val (funcCall, payments)                                      = ABIConverter(script).decodeFunctionCall(hexCallData)
      override def id: Coeval[ByteStr]                                   = tx.id
      override def dApp: AddressOrAlias                                  = Invocation.this.dApp
      override def sender: PublicKey                                     = tx.signerPublicKey()
      override def root: InvokeScriptTransactionLike                     = this
      override def assetFee: (Asset, TxTimestamp)                        = tx.assetFee
      override def timestamp: TxTimestamp                                = tx.timestamp
      override def chainId: TxVersion                                    = tx.chainId
      override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = Invocation.this.smartAssets(blockchain)
    }

    def json(senderAddress: Address): JsObject =
      Json.obj("invokeScript" -> Json.obj("sender" -> senderAddress.toString, "dApp" -> dApp.toString))
  }

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

  def apply(bytes: Array[Byte]): EthereumTransaction =
    apply(TransactionDecoder.decode(EthEncoding.toHexString(bytes)).asInstanceOf[SignedRawTransaction])

  val ERC20TransferPrefix: String = "a9059cbb"

  private def extractPayload(underlying: RawTransaction): Payload = {
    val hexData          = EthEncoding.cleanHexPrefix(underlying.getData)
    val recipientAddress = ByteStr(EthEncoding.toBytes(underlying.getTo))
    if (hexData.isEmpty) {
      Transfer(
        Left(Asset.Waves),
        underlying.getValue.divide(BigInt(AmountMultiplier).bigInteger).longValueExact(),
        Address(recipientAddress.arr)
      )
    } else if (hexData.startsWith(ERC20TransferPrefix)) {
      val recipient = decode[EthAddress](hexData, 8)
      val amount    = decode[Uint256](hexData, 72)
      Transfer(
        Right(ERC20Address(recipientAddress)),
        amount.getValue.longValueExact(),
        Address(EthEncoding.toBytes(recipient.toString))
      )
    } else Invocation(Address(recipientAddress.arr), hexData)
  }

  def apply(underlying: RawTransaction): EthereumTransaction =
    new EthereumTransaction(extractPayload(underlying), underlying, null, AddressScheme.current.chainId)

  def apply(underlying: SignedRawTransaction): EthereumTransaction =
    new EthereumTransaction(
      extractPayload(underlying),
      underlying,
      underlying.getSignatureData,
      underlying.getChainId.toByte
    )
}
