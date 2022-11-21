package com.wavesplatform.transaction.smart

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.MultiPaymentPolicyProvider.*
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments.*
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.invoke.InvokeScriptLike
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

object AttachedPaymentExtractor {
  def extractPayments(
      tx: InvokeScriptLike,
      version: StdLibVersion,
      blockchain: Blockchain,
      targetScript: AttachedPaymentTarget
  ): Either[String, AttachedPayments] =
    extractPayments(tx.payments, version, blockchain.allowsMultiPayment, targetScript)

  def extractPayments(
      payments: Seq[Payment],
      version: StdLibVersion,
      blockchainAllowsMultiPayment: Boolean,
      targetScript: AttachedPaymentTarget
  ): Either[String, AttachedPayments] =
    if (payments.size <= 1)
      if (version.supportsMultiPayment)
        multiple(payments)
      else
        single(payments)
    else if (!blockchainAllowsMultiPayment)
      Left("Multiple payments isn't allowed now")
    else if (!version.supportsMultiPayment)
      Left(scriptErrorMessage(targetScript, version))
    else if (payments.size > version.maxPayments)
      Left(s"Script payment amount=${payments.size} should not exceed ${version.maxPayments}")
    else
      multiple(payments)

  private def single(payments: Seq[Payment]) =
    Right(AttachedPayments.Single(payments.headOption.map(p => (p.amount, p.assetId.compatId))))

  private def multiple(payments: Seq[Payment]) =
    Right(AttachedPayments.Multi(payments.map(p => (p.amount, p.assetId.compatId))))

  private def scriptErrorMessage(apt: AttachedPaymentTarget, version: StdLibVersion): String = {
    val name = (apt: @unchecked) match {
      case DApp            => "DApp"
      case InvokerScript   => "Invoker script"
      case AssetScript(id) => s"Attached asset script id=$id"
    }
    s"$name version ${version.id} < ${MultiPaymentSupportedVersion.id} doesn't support multiple payment attachment"
  }
}

trait AttachedPaymentTarget
case object DApp                    extends AttachedPaymentTarget
case object InvokerScript           extends AttachedPaymentTarget
case class AssetScript(id: ByteStr) extends AttachedPaymentTarget
