package com.wavesplatform.state.diffs

import cats.implicits.{toBifunctorOps, toFoldableOps}
import cats.instances.list._
import cats.syntax.traverse._
import com.wavesplatform.account.Address
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, Validation}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._

object MassTransferTransactionDiff {

  def apply(blockchain: Blockchain, blockTime: Long)(tx: MassTransferTransaction): Either[ValidationError, Diff] = {
    def parseTransfer(xfer: ParsedTransfer): Validation[(Map[Address, Portfolio], Long)] = {
      for {
        recipientAddr <- blockchain.resolveAlias(xfer.address)
        portfolio = tx.assetId
          .fold(Map(recipientAddr -> Portfolio(xfer.amount.value, LeaseBalance.empty, Map.empty))) { asset =>
            Map(recipientAddr -> Portfolio(0, LeaseBalance.empty, Map(asset -> xfer.amount.value)))
          }
      } yield (portfolio, xfer.amount.value)
    }
    val portfoliosEi = tx.transfers.toList.traverse(parseTransfer)

    portfoliosEi.flatMap { list: List[(Map[Address, Portfolio], Long)] =>
      val sender   = Address.fromPublicKey(tx.sender)
      val foldInit = (Map(sender -> Portfolio(-tx.fee.value, LeaseBalance.empty, Map.empty)), 0L)
      list
        .foldM(foldInit) { case ((totalPortfolios, totalTransferAmount), (portfolios, transferAmount)) =>
          Diff.combine(totalPortfolios, portfolios).map((_, totalTransferAmount + transferAmount))
        }
        .flatMap {
          case (recipientPortfolios, totalAmount) =>
            Diff.combine(
              recipientPortfolios,
              tx.assetId
                .fold(Map(sender -> Portfolio(-totalAmount, LeaseBalance.empty, Map.empty))) { asset =>
                  Map(sender -> Portfolio(0, LeaseBalance.empty, Map(asset -> -totalAmount)))
                }
            )
        }
        .leftMap(GenericError(_))
        .flatMap { completePortfolio =>
          val assetIssued =
            tx.assetId match {
              case Waves                  => true
              case asset @ IssuedAsset(_) => blockchain.assetDescription(asset).isDefined
            }
          Either.cond(
            assetIssued,
            Diff(portfolios = completePortfolio, scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)),
            GenericError(s"Attempt to transfer a nonexistent asset")
          )
        }
    }
  }
}
