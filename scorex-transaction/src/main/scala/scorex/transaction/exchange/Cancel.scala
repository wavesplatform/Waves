package scorex.transaction.exchange

import scorex.account.PublicKeyAccount

/**
  * Cancel transaciton to be sent to matcher
  */
case class Cancel(spendAddress: PublicKeyAccount, orderId: Array[Byte], signature: Array[Byte])
