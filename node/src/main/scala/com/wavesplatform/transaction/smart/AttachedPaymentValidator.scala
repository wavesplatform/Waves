package com.wavesplatform.transaction.smart

import com.wavesplatform.features.MultiPaymentPolicyProvider._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Asset, DApp, Expression, V4}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.traits.domain.Payments
import com.wavesplatform.state.Blockchain

object AttachedPaymentValidator {
  def extractPayments(
    tx: InvokeScriptTransaction,
    ds: DirectiveSet,
    blockchain: Blockchain
  ): Either[ExecutionError, Payments] =
    if (blockchain.multiPaymentAllowed)
      if (ds.stdLibVersion < V4)
        Left(scriptErrorMessage(ds))
      else if (tx.payments.size > ContractLimits.MaxAttachedPaymentAmount)
        Left(s"Script payment amount=${tx.payments.size} should not exceed ${ContractLimits.MaxAttachedPaymentAmount}")
      else
        Right(Payments.Multi(tx.payments.map(p => (p.amount, p.assetId.compatId))))
    else if (tx.payments.size > 1)
      Left("Multiple payments isn't allowed now")
    else
      Right(Payments.Single(tx.payments.headOption.map(p => (p.amount, p.assetId.compatId))))

  private def scriptErrorMessage(ds: DirectiveSet): String = {
    val name = (ds.scriptType, ds.contentType) match {
      case (Account, DApp)       => "DApp"
      case (Account, Expression) => "Invoker script"
      case (Asset,   Expression) => "Attached asset script"
      case _ => ???
    }
    s"$name version ${ds.stdLibVersion.id} < 4 doesn't support multiple payment attachment"
  }
}
