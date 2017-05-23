package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import scorex.account.Account
import scorex.transaction.StateValidationError
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.TransferTransaction

import scala.util.Right

object TransferTransactionDiff {
  def apply(state: StateReader, s: FunctionalitySettings, blockTime: Long, height: Int)(tx: TransferTransaction): Either[StateValidationError, Diff] = {
    val sender = Account.fromPublicKey(tx.sender.publicKey)

    val isInvalidEi = for {
      recipient <- state.resolveAliasEi(tx.recipient)
      portfolios = (
        tx.assetId match {
          case None => Map(sender -> Portfolio(-tx.amount, LeaseInfo.empty, Map.empty)).combine(
            Map(recipient -> Portfolio(tx.amount, LeaseInfo.empty, Map.empty))
          )
          case Some(aid) =>
            val assetId = EqByteArray(aid)
            Map(sender -> Portfolio(0, LeaseInfo.empty, Map(assetId -> -tx.amount))).combine(
              Map(recipient -> Portfolio(0, LeaseInfo.empty, Map(assetId -> tx.amount)))
            )
        }).combine(
        tx.feeAssetId match {
          case None => Map(sender -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty))
          case Some(aid) =>
            Map(sender -> Portfolio(0, LeaseInfo.empty, Map(EqByteArray(aid) -> -tx.fee)))
        }
      )
      assetIssued = tx.assetId match {
        case None => true
        case Some(aid) => state.assetInfo(EqByteArray(aid)).isDefined
      }
      feeAssetIssued = tx.feeAssetId match {
        case None => true
        case Some(aid) => state.assetInfo(EqByteArray(aid)).isDefined
      }
    } yield (portfolios, blockTime > s.allowUnissuedAssetsUntil && !(assetIssued && feeAssetIssued))

    isInvalidEi match {
      case Left(e) => Left(e)
      case Right((portfolios, invalid)) =>
        if (invalid)
          Left(TransactionValidationError(tx, s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${s.allowUnissuedAssetsUntil}"))
        else
          Right(Diff(height, tx, portfolios))
    }
  }
}