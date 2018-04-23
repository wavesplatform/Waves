package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.state._
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.VersionedTransferTransaction

object ScriptTransferTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: VersionedTransferTransaction): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.sender.publicKey)
    for {
      recipient <- blockchain.resolveAliasEi(tx.recipient)
      portfolios = (tx.assetId match {
        case None =>
          Map(sender -> Portfolio(-tx.amount, LeaseBalance.empty, Map.empty)).combine(
            Map(recipient -> Portfolio(tx.amount, LeaseBalance.empty, Map.empty))
          )
        case Some(aid) =>
          Map(sender -> Portfolio(0, LeaseBalance.empty, Map(aid -> -tx.amount))).combine(
            Map(recipient -> Portfolio(0, LeaseBalance.empty, Map(aid -> tx.amount)))
          )
      }).combine(Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)))
      assetIssued = tx.assetId match {
        case None      => true
        case Some(aid) => blockchain.assetDescription(aid).isDefined
      }
      _ <- Either.cond(assetIssued, (), GenericError(s"Unissued assets are not allowed"))
    } yield Diff(height, tx, portfolios)
  }
}
