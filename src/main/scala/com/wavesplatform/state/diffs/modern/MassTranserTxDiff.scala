package com.wavesplatform.state.diffs.modern

import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import scorex.account.Address
import cats.implicits._
import scorex.transaction.assets.MassTransferTransaction.ParsedTransfer
import scorex.transaction.base.MassTransferTxBase
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.{GenericError, Validation}

object MassTranserTxDiff {
  def apply(blockchain: Blockchain, blockTime: Long, height: Int)(tx: MassTransferTxBase): Either[ValidationError, Diff] = {
    def parseTransfer(xfer: ParsedTransfer): Validation[(Map[Address, Portfolio], Long)] = {
      for {
        recipientAddr <- blockchain.resolveAliasEi(xfer.address)
        portfolio = tx.assetId match {
          case None      => Map(recipientAddr -> Portfolio(xfer.amount, LeaseBalance.empty, Map.empty))
          case Some(aid) => Map(recipientAddr -> Portfolio(0, LeaseBalance.empty, Map(aid -> xfer.amount)))
        }
      } yield (portfolio, xfer.amount)
    }
    val portfoliosEi = tx.transfers.traverse(parseTransfer)

    portfoliosEi.flatMap { list: List[(Map[Address, Portfolio], Long)] =>
      val sender   = Address.fromPublicKey(tx.sender.publicKey)
      val foldInit = (Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)), 0L)
      val (recipientPortfolios, totalAmount) = list.fold(foldInit) { (u, v) =>
        (u._1 combine v._1, u._2 + v._2)
      }
      val completePortfolio = recipientPortfolios.combine(tx.assetId match {
        case None      => Map(sender -> Portfolio(-totalAmount, LeaseBalance.empty, Map.empty))
        case Some(aid) => Map(sender -> Portfolio(0, LeaseBalance.empty, Map(aid -> -totalAmount)))
      })

      val assetIssued = tx.assetId.forall(blockchain.assetDescription(_).isDefined)

      Either.cond(assetIssued, Diff(height, tx, completePortfolio), GenericError(s"Attempt to transfer a nonexistent asset"))
    }
  }
}
