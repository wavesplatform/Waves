package scorex.block

import com.wavesplatform.state2.ByteStr
import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{Signed, Transaction, ValidationError}

case class MicroBlock private(version: Byte, generator: PublicKeyAccount, transactionData: Seq[Transaction], prevResBlockSig: BlockId,
                              totalResBlockSig: BlockId, signature: ByteStr) extends Signed {
  override protected lazy val signatureValid: Boolean = true
}

object MicroBlock {
  def apply(generator: PublicKeyAccount, transactionData: Seq[Transaction], prevResBlockSig:
  BlockId, totalResBlockSig: BlockId, signature: ByteStr): Either[ValidationError, MicroBlock] = {
    if (transactionData.isEmpty)
      Left(GenericError("cannot create empty MicroBlock"))
    else
      Right(new MicroBlock(version = 1: Byte, generator, transactionData, prevResBlockSig, totalResBlockSig, signature))
  }
}

