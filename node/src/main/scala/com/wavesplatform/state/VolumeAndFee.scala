package com.wavesplatform.state
import play.api.libs.json.{Json, Writes}

case class VolumeAndFee(volume: Long, fee: Long) {
  def combineE(that: VolumeAndFee): Either[String, VolumeAndFee] =
    for {
      volume <- safeSum(this.volume, that.volume, "Order volume")
      fee    <- safeSum(this.fee, that.fee, "Order fee")
    } yield VolumeAndFee(volume, fee)
}

object VolumeAndFee {
  val empty: VolumeAndFee = VolumeAndFee(0, 0)

  implicit val writes: Writes[VolumeAndFee] = Json.writes
}
