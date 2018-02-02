package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.{Address, AddressOrAlias}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.{GenericError, Validation}
import scorex.transaction.assets.MassTransferTransaction

object MassTransferTransactionDiff {

  def apply(state: SnapshotStateReader, blockTime: Long, height: Int)(tx: MassTransferTransaction): Either[ValidationError, Diff] = {
    def parseTransfer(recipient: AddressOrAlias, amount: Long): Validation[(Map[Address, Portfolio], Long)] = {
      for {
        recipientAddr <- state.resolveAliasEi(recipient)
        portfolio = tx.assetId match {
          case None => Map(recipientAddr -> Portfolio(amount, LeaseInfo.empty, Map.empty))
          case Some(aid) => Map(recipientAddr -> Portfolio(0, LeaseInfo.empty, Map(aid -> amount)))
        }
      } yield (portfolio, amount)
    }
    val portfoliosEi = tx.transfers.traverse(Function.tupled(parseTransfer _))

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
      Either.cond(assetIssued,
        Diff(height, tx, completePortfolio),
        GenericError(s"Attempt to transfer a nonexistent asset"))
    }
  }
}