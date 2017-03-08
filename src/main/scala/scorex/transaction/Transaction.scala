package scorex.transaction

import com.google.common.primitives.Ints
import scorex.account.Account
import scorex.serialization.JsonSerializable
import scorex.transaction.TransactionParser.TransactionType


/**
  * A transaction is an atomic state modifier
  */
trait Transaction extends StateChangeReason with JsonSerializable {

  val transactionType: TransactionType.Value
  val assetFee: (Option[AssetId], Long)
  val timestamp: Long

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id.sameElements(tx.id)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(id.takeRight(4))

}

case class BalanceChange(assetAcc: AssetAcc, delta: Long)
case class EffectiveBalanceChange(account: Account, amount: Long)
