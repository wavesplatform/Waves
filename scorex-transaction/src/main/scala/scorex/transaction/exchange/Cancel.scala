package scorex.transaction.exchange

import scorex.account.PublicKeyAccount

case class Cancel(spendAddress: PublicKeyAccount, orderId: Array[Byte], signature: Array[Byte])
