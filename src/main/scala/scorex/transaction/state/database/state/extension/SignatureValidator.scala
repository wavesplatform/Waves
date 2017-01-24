package scorex.transaction.state.database.state.extension

import scorex.crypto.EllipticCurveImpl
import scorex.transaction.{SignedTransaction, Transaction}

class SignatureValidator extends StateExtension {


  override def isValid(tx: Transaction, height: Int): Boolean = tx match {
    case tx: SignedTransaction => EllipticCurveImpl.verify(tx.signature, tx.toSign, tx.sender.publicKey)
    case _ => false
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {}
}