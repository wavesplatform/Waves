package scorex.transaction

import java.math.BigInteger
import java.util.Arrays

import com.google.common.primitives.Bytes
import scorex.account.Account
import scorex.crypto.Base58
import scorex.transaction.Transaction._

abstract class PreTransaction(val recipient: Account, val amount: BigDecimal) {
  def involvedAmount(account: Account): BigDecimal
}

object PreTransaction {
  def fromBytes(data: Array[Byte]): PreTransaction = {
    val txType = data.head

    txType match {
      case i: Byte if i == PreTransactionType.FEE_PRETRANSACTION.id =>
        FeeTransaction.Parse(data.tail)

      case _ => Transaction.fromBytes(data)
    }
  }

  object PreTransactionType extends Enumeration {
    type PreTransactiontype = Value

    val FEE_PRETRANSACTION = Value(0)
  }

}


//virtual transaction type not stated explicitly, used for storing block rewards into the database
case class FeeTransaction(override val recipient: Account, override val amount: BigDecimal)
  extends PreTransaction(recipient, amount) {

  def involvedAmount(account: Account): BigDecimal = account.address == recipient.address match {
    case true => amount
    case false => 0
  }

  def toBytes(): Array[Byte] = {
    val amountBytes = amount.bigDecimal.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)
    Bytes.concat(amountFill, amountBytes)
  }
}

object FeeTransaction {
  def Parse(data: Array[Byte]): FeeTransaction = {
    var position = 0
    val recipientBytes = Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH)
    val recipient = new Account(Base58.encode(recipientBytes))

    position += RECIPIENT_LENGTH
    val amountBytes = Arrays.copyOfRange(data, 0, AMOUNT_LENGTH)
    val amount = BigDecimal(new BigInteger(amountBytes), 8)

    FeeTransaction(recipient, amount)
  }
}