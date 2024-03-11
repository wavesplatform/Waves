package com.wavesplatform.state

import cats.kernel.Monoid
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, Transaction}
import play.api.libs.json.{JsNumber, Writes}

sealed abstract class Sponsorship
case class SponsorshipValue(minFee: Long) extends Sponsorship
case object SponsorshipNoInfo             extends Sponsorship

object Sponsorship {
  implicit val writesValue: Writes[SponsorshipValue] = Writes[SponsorshipValue](v => JsNumber(v.minFee))

  implicit val sponsorshipMonoid: Monoid[Sponsorship] = new Monoid[Sponsorship] {
    override def empty: Sponsorship = SponsorshipNoInfo

    override def combine(x: Sponsorship, y: Sponsorship): Sponsorship = y match {
      case SponsorshipNoInfo => x
      case _                 => y
    }
  }

  def calcWavesFeeAmount(tx: Transaction, getSponsorship: IssuedAsset => Option[Long]): Long = tx.assetFee match {
    case (asset @ IssuedAsset(_), amountInAsset) =>
      val sponsorship = getSponsorship(asset).getOrElse(0L)
      Sponsorship.toWaves(amountInAsset, sponsorship)

    case (Asset.Waves, amountInWaves) =>
      amountInWaves
  }

  def sponsoredFeesSwitchHeight(blockchain: Blockchain): Int =
    blockchain
      .featureActivationHeight(BlockchainFeatures.FeeSponsorship.id)
      .map(h => h + blockchain.settings.functionalitySettings.activationWindowSize(h))
      .getOrElse(Int.MaxValue)

  def toWaves(assetFee: Long, sponsorship: Long): Long =
    if (sponsorship == 0) Long.MaxValue
    else {
      val waves = BigInt(assetFee) * FeeValidation.FeeUnit / sponsorship
      waves.bigInteger.longValueExact()
    }

  def fromWaves(wavesFee: Long, sponsorship: Long): Long =
    if (wavesFee == 0 || sponsorship == 0) 0
    else {
      val assetFee = BigInt(wavesFee) * sponsorship / FeeValidation.FeeUnit
      assetFee.bigInteger.longValueExact()
    }
}
