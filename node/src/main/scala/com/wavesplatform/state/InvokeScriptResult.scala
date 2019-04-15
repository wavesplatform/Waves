package com.wavesplatform.state
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import play.api.libs.json.Json

final case class InvokeScriptResult(dataEntries: Seq[DataEntry[_]] = Nil, payments: Seq[InvokeScriptResult.Payment] = Nil)

object InvokeScriptResult {
  final case class Payment(address: Address, asset: Asset, amount: Long)
  object Payment {
    implicit val jsonFormat = Json.format[Payment]
  }

  implicit val jsonFormat = Json.format[InvokeScriptResult]

  implicit val monoid = new Monoid[InvokeScriptResult] {
    override val empty: InvokeScriptResult =
      InvokeScriptResult()

    override def combine(x: InvokeScriptResult, y: InvokeScriptResult): InvokeScriptResult =
      InvokeScriptResult(x.dataEntries ++ y.dataEntries, x.payments ++ y.payments)
  }

  // TODO: Replace to binary format
  def toBytes(invokeScriptResult: InvokeScriptResult): ByteStr = {
    Json.toBytes(Json.toJson(invokeScriptResult))
  }

  def fromBytes(bs: ByteStr): InvokeScriptResult = {
    Json.parse(bs).as[InvokeScriptResult]
  }
}
