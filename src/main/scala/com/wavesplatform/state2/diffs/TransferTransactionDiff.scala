package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, EqByteArray, Portfolio}
import scorex.account.{Account, Alias}
import scorex.transaction.StateValidationError
import scorex.transaction.assets.TransferTransaction

import scala.util.Right

object TransferTransactionDiff {
  def apply(state: StateReader, height: Int)(tx: TransferTransaction): Either[StateValidationError, Diff] = {
    val sender = Account.fromPublicKey(tx.sender.publicKey)
    val recipient = tx.recipient match {
      case a: Account => a
      case a: Alias => ???
    }

    val portfolios = (
      tx.assetId match {
        case None => Map(sender -> Portfolio(-tx.amount, -tx.amount, Map.empty)).combine(
          Map(recipient -> Portfolio(tx.amount, tx.amount, Map.empty))
        )
        case Some(aid) =>
          val assetId = EqByteArray(aid)
          Map(sender -> Portfolio(0, 0, Map(assetId -> -tx.amount))).combine(
            Map(recipient -> Portfolio(0, 0, Map(assetId -> tx.amount)))
          )
      }).combine(
      tx.feeAssetId match {
        case None => Map(sender -> Portfolio(-tx.fee, -tx.fee, Map.empty))
        case Some(aid) =>
          Map(sender -> Portfolio(0, 0, Map(EqByteArray(aid) -> tx.fee)))
      }
    )

    Right(Diff(height = height,
      tx = tx,
      portfolios = portfolios,
      assetInfos = Map.empty
    ))
  }
}
