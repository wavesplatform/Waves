package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.MassTransferTransaction

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

    portfoliosEi.flatMap { list: List[(Map[Address, Portfolio], Long)] =>
      val sender = Address.fromPublicKey(tx.sender.publicKey)
      val foldInit = (Map(sender -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)), 0L)
      val (recipientPortfolios, totalAmount) = list.fold(foldInit) { (u, v) => (u._1 combine v._1, u._2 + v._2) }
      val completePortfolio = recipientPortfolios.combine(
        tx.assetId match {
          case None => Map(sender -> Portfolio(-totalAmount, LeaseInfo.empty, Map.empty))
          case Some(aid) => Map(sender -> Portfolio(0, LeaseInfo.empty, Map(aid -> -totalAmount)))
        })

      val assetIssued = tx.assetId match {
        case None => true
        case Some(aid) => state.assetInfo(aid).isDefined
      }
      Either.cond(blockTime <= s.allowUnissuedAssetsUntil || assetIssued,
        Diff(height, tx, completePortfolio),
        GenericError(s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${s.allowUnissuedAssetsUntil}"))
    }
  }
}