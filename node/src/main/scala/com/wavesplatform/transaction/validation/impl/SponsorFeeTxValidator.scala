package com.wavesplatform.transaction.validation.impl

import cats.syntax.validated.*
import com.wavesplatform.transaction.TxValidationError.NegativeMinFee
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object SponsorFeeTxValidator extends TxValidator[SponsorFeeTransaction] {
  override def validate(tx: SponsorFeeTransaction): ValidatedV[SponsorFeeTransaction] = tx.validNel

  def checkMinSponsoredAssetFee(minSponsoredAssetFee: Option[Long]): Either[NegativeMinFee, Unit] =
    Either.cond(minSponsoredAssetFee.forall(_ > 0), (), NegativeMinFee(minSponsoredAssetFee.get, "asset"))
}
