package com.wavesplatform.state
import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils._
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBTransactions, InvokeScriptResult => PBInvokeScriptResult}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.transaction.Asset
import play.api.libs.json.Json

final case class InvokeScriptResult(dataEntries: Seq[DataEntry[_]] = Nil, payments: Seq[InvokeScriptResult.Payment] = Nil)

//noinspection TypeAnnotation
object InvokeScriptResult {
  val empty = InvokeScriptResult()

  final case class Payment(address: Address, asset: Asset, amount: Long)
  object Payment {
    implicit val jsonFormat = Json.format[Payment]
  }

  implicit val jsonFormat = Json.format[InvokeScriptResult]

  implicit val monoid = new Monoid[InvokeScriptResult] {
    override val empty: InvokeScriptResult =
      InvokeScriptResult.this.empty

    override def combine(x: InvokeScriptResult, y: InvokeScriptResult): InvokeScriptResult =
      InvokeScriptResult(x.dataEntries ++ y.dataEntries, x.payments ++ y.payments)
  }

  def toBytes(isr: InvokeScriptResult): Array[Byte] = {
    val pbValue = this.toPB(isr)
    PBUtils.encodeDeterministic(pbValue)
  }

  def fromBytes(bs: Array[Byte]): InvokeScriptResult = {
    val pbValue = PBInvokeScriptResult.parseFrom(bs)
    fromPB(pbValue)
  }

  def toPB(isr: InvokeScriptResult): PBInvokeScriptResult = {
    PBInvokeScriptResult(
      isr.dataEntries.map(PBTransactions.toPBDataEntry),
      isr.payments.map(
        payment =>
          PBInvokeScriptResult.Payment(
            ByteString.copyFrom(payment.address.bytes),
            Some(PBAmounts.fromAssetAndAmount(payment.asset, payment.amount))
          ))
    )
  }

  def fromPB(pbValue: PBInvokeScriptResult): InvokeScriptResult = {
    InvokeScriptResult(
      pbValue.dataEntries.map(PBTransactions.toVanillaDataEntry),
      pbValue.payments.map { p =>
        val (asset, amount) = PBAmounts.toAssetAndAmount(p.getAmount)
        InvokeScriptResult.Payment(Address.fromBytes(p.address.toByteArray).explicitGet(), asset, amount)
      }
    )
  }
}
