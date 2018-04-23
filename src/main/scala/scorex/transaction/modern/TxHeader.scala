package scorex.transaction.modern

import scorex.account.PublicKeyAccount

final case class TxHeader(`type`: Byte,
                          version: Byte,
                          sender: PublicKeyAccount,
                          fee: Long,
                          timestamp: Long)