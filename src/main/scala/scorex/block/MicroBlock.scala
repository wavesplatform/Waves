package scorex.block

import com.wavesplatform.state2.ByteStr
import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.transaction.{Signed, Transaction}

case class MicroBlock(generator: PublicKeyAccount, transactions: Seq[Transaction], prevResBlockSig: BlockId,
                      totalResBlockSig: BlockId, signature: ByteStr) extends Signed {
  override protected lazy val signatureValid: Boolean = true
}

