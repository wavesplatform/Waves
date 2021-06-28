package com.wavesplatform.transaction

import cats.data.Validated
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

case class GenesisTransaction private (recipient: Address, amount: TxAmount, timestamp: TxTimestamp, signature: ByteStr, chainId: Byte)
    extends Transaction(TransactionType.Genesis) {
  override val assetFee: (Asset, Long) = (Waves, 0)
  override val id: Coeval[ByteStr]     = Coeval.evalOnce(signature)

  val bodyBytes: Coeval[Array[Byte]]      = Coeval.evalOnce(GenesisTxSerializer.toBytes(this))
  override val bytes: Coeval[Array[Byte]] = bodyBytes
  override val json: Coeval[JsObject]     = Coeval.evalOnce(GenesisTxSerializer.toJson(this))
}

object GenesisTransaction extends TransactionParser {
  type TransactionT = GenesisTransaction

  override val typeId: TxType                    = 1: Byte
  override val supportedVersions: Set[TxVersion] = Set(1)

  override def parseBytes(bytes: Array[TxVersion]): Try[GenesisTransaction] =
    GenesisTxSerializer.parseBytes(bytes)

  implicit val validator: TxValidator[GenesisTransaction] =
    tx =>
      TxConstraints.seq(tx)(
        Validated.condNel(tx.amount >= 0, tx, TxValidationError.NegativeAmount(tx.amount, "waves")),
        TxConstraints.addressChainId(tx.recipient, tx.chainId)
      )

  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {
    val payload = Bytes.concat(Ints.toByteArray(typeId), Longs.toByteArray(timestamp), recipient.bytes, Longs.toByteArray(amount))
    val hash    = crypto.fastHash(payload)
    Bytes.concat(hash, hash)
  }

  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))
    GenesisTransaction(recipient, amount, timestamp, signature, recipient.chainId).validatedEither
  }
}
