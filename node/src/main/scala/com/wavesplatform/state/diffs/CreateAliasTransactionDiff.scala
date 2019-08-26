package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
          aliases = Map(tx.alias -> tx.sender.toAddress),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
        )
      )
}
