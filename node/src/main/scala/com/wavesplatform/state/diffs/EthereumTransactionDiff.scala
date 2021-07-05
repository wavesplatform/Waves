package com.wavesplatform.state.diffs

import cats.syntax.either._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.TxValidationError.InvalidAssetId
import com.wavesplatform.transaction.{Asset, EthereumTransaction}

object EthereumTransactionDiff {
  def apply(blockchain: Blockchain, e: EthereumTransaction): Either[ValidationError, Diff] = e match {
    case et: EthereumTransaction.Transfer =>
      for {
        asset <- et.asset.map[Either[ValidationError, Asset]](c => blockchain.resolveERC20Address(c).toRight(InvalidAssetId)).valueOr(Right(_))
        diff  <- TransferDiff(blockchain)(et.sender, et.recipient, et.amount, asset, et.assetFee._2, et.assetFee._1)
      } yield diff
  }
}
