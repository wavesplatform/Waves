package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.smart.SetScriptTransaction

import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.lang.v1.DenyDuplicateVarNames
import com.wavesplatform.utils.varNames

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    val scriptOpt = tx.script
    for {
      _ <- scriptOpt.fold(Right(()): Either[ValidationError, Unit]) { script =>
        if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, height)) {
          Right(())
        } else {
          println(s"SmartAccountTrading ${blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, height)} at $height")
          val version = script.version
          DenyDuplicateVarNames(version, varNames(version), script.expr).left.map(GenericError.apply)
        }
      }
    } yield {
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> scriptOpt)
      )
    }
  }
}
