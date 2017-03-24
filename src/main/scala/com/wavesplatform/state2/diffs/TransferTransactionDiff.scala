package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, EqByteArray, Portfolio}
import scorex.account.{Account, Alias}
import scorex.transaction.StateValidationError
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.TransferTransaction

import scala.util.Right

object TransferTransactionDiff {
  def apply(state: StateReader, s: FunctionalitySettings, blockTime: Long, height: Int)(tx: TransferTransaction): Either[StateValidationError, Diff] = {
    lazy val sender = Account.fromPublicKey(tx.sender.publicKey)
    lazy val recipient = tx.recipient match {
      case a: Account => a
      case a: Alias => ???
    }

    lazy val portfolios = (
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
          Map(sender -> Portfolio(0, 0, Map(EqByteArray(aid) -> -tx.fee)))
      }
    )

    lazy val a1 = tx.assetId match {
      case None => true
      case Some(aid) => state.assetInfo(EqByteArray(aid)).isDefined
    }

    lazy val a2 = tx.feeAssetId match {
      case None => true
      case Some(aid) => state.assetInfo(EqByteArray(aid)).isDefined
    }

    if (blockTime > s.allowUnissuedAssetsUntil && !(a1 && a2))
      Left(TransactionValidationError(tx, s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${s.allowUnissuedAssetsUntil}"))
    else
      Right(Diff(height = height,
        tx = tx,
        portfolios = portfolios,
        assetInfos = Map.empty
      ))
  }
}
