package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.TxValidationError.NegativeMinFee
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object SponsorFeeTxValidator extends TxValidator[SponsorFeeTransaction] {
  override def validate(tx: SponsorFeeTransaction): ValidatedV[SponsorFeeTransaction] = {
    import tx._
    V.seq(tx)(
      V.cond(minSponsoredAssetFee.forall(_ > 0), NegativeMinFee(minSponsoredAssetFee.get, "asset")),
      V.fee(fee)
    )
  }
}
