package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5}
import com.wavesplatform.lang.v1.ContractLimits

sealed trait AttachedPayments {
  def payments: Seq[AttachedPayments.Payment]
}

object AttachedPayments {
  type Payment = (Long, Option[ByteStr])

  case class Single(p: Option[Payment]) extends AttachedPayments {
    def payments: Seq[AttachedPayments.Payment] = p.toSeq
  }
  case class Multi(payments: Seq[Payment]) extends AttachedPayments

  val MultiPaymentSupportedVersion: StdLibVersion = V4

  implicit class StdLibVersionMultiPaymentOps(version: StdLibVersion) {
    def supportsMultiPayment: Boolean = version >= MultiPaymentSupportedVersion
    def maxPayments: Int = {
      if (!supportsMultiPayment) {
        1
      } else if (version < V5) {
        ContractLimits.MaxAttachedPaymentAmount
      } else {
        ContractLimits.MaxAttachedPaymentAmountV5
      }
    }
  }
}
