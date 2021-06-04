package com.wavesplatform.transaction.smart

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs.invoke.InvokeScriptLike
import com.wavesplatform.features.MultiPaymentPolicyProvider._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments._
import com.wavesplatform.state.Blockchain

object AttachedPaymentExtractor {
  def extractPayments(
    tx:           InvokeScriptLike,
    version:      StdLibVersion,
    blockchain:   Blockchain,
    targetScript: AttachedPaymentTarget
  ): Either[ExecutionError, AttachedPayments] =
    if (tx.payments.size <= 1)
      if (version.supportsMultiPayment)
        multiple(tx)
      else
        single(tx)
    else
      if (!blockchain.allowsMultiPayment)
        Left("Multiple payments isn't allowed now")
      else if (!version.supportsMultiPayment)
        Left(scriptErrorMessage(targetScript, version))
      else if (tx.payments.size > version.maxPayments)
        Left(s"Script payment amount=${tx.payments.size} should not exceed ${version.maxPayments}")
      else
        multiple(tx)

  private def single(tx: InvokeScriptLike) =
    Right(AttachedPayments.Single(tx.payments.headOption.map(p => (p.amount, p.assetId.compatId))))

  private def multiple(tx: InvokeScriptLike) =
    Right(AttachedPayments.Multi(tx.payments.map(p => (p.amount, p.assetId.compatId))))

  private def scriptErrorMessage(apt: AttachedPaymentTarget, version: StdLibVersion): String = {
    val name = apt match {
      case DApp            => "DApp"
      case InvokerScript   => "Invoker script"
      case AssetScript(id) => s"Attached asset script id=$id"
    }
    s"$name version ${version.id} < ${MultiPaymentSupportedVersion.id} doesn't support multiple payment attachment"
  }
}

trait AttachedPaymentTarget
case object DApp                     extends AttachedPaymentTarget
case object InvokerScript            extends AttachedPaymentTarget
case class  AssetScript(id: ByteStr) extends AttachedPaymentTarget
