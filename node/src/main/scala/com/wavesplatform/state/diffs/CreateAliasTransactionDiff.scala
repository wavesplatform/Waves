package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain)(tx: CreateAliasTransaction): Either[ValidationError, Diff] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, blockchain.height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else if (blockchain.height > blockchain.settings.functionalitySettings.allowMultipleProofsInCreateAliasUntil && tx.proofs.size > 1)
      Left(GenericError("Invalid proofs size"))
    else
      Right(
        Diff(
          portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee.value, LeaseBalance.empty, Map.empty)),
          aliases = Map(tx.alias               -> tx.sender.toAddress),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
        )
      )
}
