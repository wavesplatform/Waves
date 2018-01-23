package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.MassTransferTransaction

import scala.util.Right

object MassTransferTransactionDiff {

  def apply(state: SnapshotStateReader, s: FunctionalitySettings, blockTime: Long, height: Int)(tx: MassTransferTransaction): Either[ValidationError, Diff] = {
    val portfoliosEi = MassTransferTransaction.processRecipientsWith(tx.transfers) { (recipient, amount) =>
      for {
        recipientAddr <- state.resolveAliasEi(recipient)
        portfolio = tx.assetId match {
          case None => Map(recipientAddr -> Portfolio(amount, LeaseInfo.empty, Map.empty))
          case Some(aid) => Map(recipientAddr -> Portfolio(0, LeaseInfo.empty, Map(aid -> amount)))
        }
      } yield (portfolio, amount)
    }

    portfoliosEi match {///map
      case Left(e) => Left(e)
      case Right(list) =>
        val sender = Address.fromPublicKey(tx.sender.publicKey)
        val (recipientPortfolios, totalAmount) = list.reduceLeft((u, v) => (u._1 combine v._1, u._2 + v._2))
        val portfolios = recipientPortfolios.combine(
          Map(sender -> Portfolio(-(totalAmount + tx.fee), LeaseInfo.empty, Map.empty)))
        val assetIssued = tx.assetId match {
          case None => true
          case Some(aid) => state.assetInfo(aid).isDefined
        }
        ///what to validate here?
        val invalid = blockTime > s.allowUnissuedAssetsUntil && !assetIssued
        Either.cond(!invalid,
          Diff(height, tx, portfolios),
          GenericError(s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${s.allowUnissuedAssetsUntil}"))
    }
  }
}