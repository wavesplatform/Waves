package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.Version
import com.wavesplatform.lang.v1.DenyDuplicateVarNames
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.smart.script.v1.ScriptV2
import com.wavesplatform.transaction.smart.{ContractInvokationTransaction, SetScriptTransaction}
import com.wavesplatform.utils.varNames

import scala.util.Right

object ContractInvokationTransactionDiff {
//  def apply(blockchain: Blockchain, height: Int)(tx: ContractInvokationTransaction): Either[ValidationError, Diff] = {
//    val sc = blockchain.accountScript(tx.contractAddress)
//    sc match  {
//      case Some(ScriptV2(_, contract)) =>
//        contract.
//
//      case _ => Left(GenericError(s"No contract at address ${tx.contractAddress}"))
//    }
//    for {
//      _ <- if(sc)
//    } yield {
//      Diff(
//        height = height,
//        tx = tx,
//        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
//        scripts = Map(tx.sender.toAddress    -> scriptOpt)
//      )
//    }
//  }
}
