package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Portfolio, StateSnapshot}
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain)(tx: CreateAliasTransaction): Either[ValidationError, StateSnapshot] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, blockchain.height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else if (tx.proofs.size > 1 && !blockchain.isFeatureActivated(BlockchainFeatures.RideV6))
      Left(GenericError("Invalid proofs size"))
    else
      StateSnapshot.build(
        blockchain,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee.value)),
        aliases = Map(tx.alias -> tx.sender.toAddress)
      )
}
