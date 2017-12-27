package scorex.transaction.smart

import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.account._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.{ProvenTransaction, Transaction}
import scorex.transaction.TransactionParser.TransactionType

case class SetScriptTransaction private(sender: PublicKeyAccount,
                                        script: Script,
                                        fee: Long,
                                        timestamp: Long,
                                        proof: ByteStr) extends Transaction with ProvenTransaction {

  override val id = Coeval.evalOnce(ByteStr(FastCryptographicHash(this.toString)))
  override val transactionType = TransactionType.SetScriptTransaction
  override val assetFee = (None, fee)
  override val json = Coeval.evalOnce(Json.obj("type" -> transactionType.id,
    "id" -> id().base58,
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
    "script" -> script.text,
    "proof" -> proof.base58)
  )
  val toSign: Coeval[Array[Byte]] = ???
  override val bytes = ???
}
