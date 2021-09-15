package com.wavesplatform.transaction.validation.impl

import cats.syntax.either._
import com.wavesplatform.transaction.TxAmount
import com.wavesplatform.transaction.TxValidationError.NegativeMinFee
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object SponsorFeeTxValidator extends TxValidator[SponsorFeeTransaction] {
  override def validate(tx: SponsorFeeTransaction): ValidatedV[SponsorFeeTransaction] = {
    import tx._
    V.seq(tx)(
      checkMinSponsoredAssetFee(minSponsoredAssetFee).toValidatedNel,
      V.fee(fee)
    )
  }

  def checkMinSponsoredAssetFee(minSponsoredAssetFee: Option[TxAmount]): Either[NegativeMinFee, Unit] =
    Either.cond(minSponsoredAssetFee.forall(_ > 0), (), NegativeMinFee(minSponsoredAssetFee.get, "asset"))
}
