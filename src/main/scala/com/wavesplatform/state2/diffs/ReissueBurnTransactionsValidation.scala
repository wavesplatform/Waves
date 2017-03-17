package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{EqByteArray, StateReader}
import scorex.transaction.{SignedTransaction, StateValidationError, Transaction}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}

import scala.util.{Left, Right}

object ReissueBurnTransactionsValidation {
  def apply(state: StateReader)(tx: Transaction): Either[StateValidationError, Transaction] = {
    tx match {
      case tx: ReissueTransaction =>
        val assetId = EqByteArray(tx.assetId)
        state.assetInfo(assetId) match {
          case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
          case Some(ai) =>
            if (!ai.isReissuable)
              Left(TransactionValidationError(tx, "Asset is not reissuable"))
            else if (!state.findTransaction[IssueTransaction](tx.assetId).exists(_.sender equals tx.sender))
              Left(TransactionValidationError(tx, "Asset was issued by other address"))
            else
              Right(tx)
        }
      case tx: BurnTransaction =>
        if (!state.findTransaction[IssueTransaction](tx.assetId).exists(_.sender equals tx.sender))
          Left(TransactionValidationError(tx, "Asset was issued by other address"))
        else
          Right(tx)
      case _ => Right(tx)
    }
  }
}
