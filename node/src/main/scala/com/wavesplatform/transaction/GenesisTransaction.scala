package com.wavesplatform.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.serialization.impl.GenesisTxSerializer
import com.wavesplatform.transaction.validation.{TxConstraints, TxValidator}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class GenesisTransaction private (recipient: Address, amount: TxNonNegativeAmount, timestamp: TxTimestamp, signature: ByteStr, chainId: Byte)
    extends Transaction {
  override val builder                 = GenesisTransaction
  override val assetFee: (Asset, Long) = (Waves, 0)
  override val id: Coeval[ByteStr]     = Coeval.evalOnce(signature)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val bytes: Coeval[Array[Byte]]     = bodyBytes
  override val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))
}

object GenesisTransaction extends TransactionParser {
  type TransactionT = GenesisTransaction

  override val typeId: TxType                    = 1: Byte
  override val supportedVersions: Set[TxVersion] = Set(1)

  val serializer = GenesisTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[GenesisTransaction] =
    serializer.parseBytes(bytes)

  implicit val validator: TxValidator[GenesisTransaction] =
    tx =>
      TxConstraints.seq(tx)(
        TxConstraints.addressChainId(tx.recipient, tx.chainId)
      )

  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {
    val payload = Bytes.concat(Ints.toByteArray(typeId), Longs.toByteArray(timestamp), recipient.bytes, Longs.toByteArray(amount))
    val hash    = crypto.fastHash(payload)
    Bytes.concat(hash, hash)
  }

  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))

    for {
      amount <- TxNonNegativeAmount(amount)(TxValidationError.NegativeAmount(amount, "waves"))
      tx     <- GenesisTransaction(recipient, amount, timestamp, signature, recipient.chainId).validatedEither
    } yield tx
  }
}
