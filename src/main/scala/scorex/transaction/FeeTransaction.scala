package scorex.transaction

import com.google.common.primitives.Bytes
import scorex.account.Account
import scorex.transaction.Transaction._


//virtual transaction type not stated explicitly, used for storing block rewards into the database
case class FeeTransaction(account: Account, amount:BigDecimal){
  def toBytes():Array[Byte] = {
    val amountBytes = amount.bigDecimal.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)
    Bytes.concat(amountFill, amountBytes)
  }
}


