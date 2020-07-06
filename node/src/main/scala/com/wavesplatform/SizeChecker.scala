package com.wavesplatform
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.transfer.TransferTransaction

object SizeChecker extends App {
  val maxProofs = Proofs(
    Seq.fill(Proofs.MaxProofs)(ByteStr.fill(Proofs.MaxProofSize)(1))
  )

  TransferTransaction(, , , , , , , , , Proofs.MaxProofs, 'W')
}
