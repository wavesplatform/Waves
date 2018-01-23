package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.ScriptTransferTransaction

import scala.util.Right

object ScriptTransferTransactionDiff {
  def apply(state: SnapshotStateReader, height: Int)(tx: ScriptTransferTransaction): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.sender.publicKey)
    for {
      recipient <- state.resolveAliasEi(tx.recipient)
      portfolios = (tx.assetId match {
        case None =>
          Map(sender -> Portfolio(-tx.amount, LeaseInfo.empty, Map.empty)).combine(
            Map(recipient -> Portfolio(tx.amount, LeaseInfo.empty, Map.empty))
          )
        case Some(aid) =>
          Map(sender -> Portfolio(0, LeaseInfo.empty, Map(aid -> -tx.amount))).combine(
            Map(recipient -> Portfolio(0, LeaseInfo.empty, Map(aid -> tx.amount)))
          )
      }).combine(Map(sender -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)))
      assetIssued = tx.assetId match {
        case None => true
        case Some(aid) => state.assetInfo(aid).isDefined
      }
      _ <- Either.cond(assetIssued, (), GenericError(s"Unissued assets are not allowed"))
    } yield Diff(height, tx, portfolios)
  }
}
