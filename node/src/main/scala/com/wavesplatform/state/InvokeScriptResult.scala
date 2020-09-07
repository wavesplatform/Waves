package com.wavesplatform.state

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.traits.domain.{Burn, Issue, Reissue, SponsorFee}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBRecipients, PBTransactions, InvokeScriptResult => PBInvokeScriptResult}
import com.wavesplatform.protobuf.utils.PBImplicitConversions._
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.utils._
import play.api.libs.json._

final case class InvokeScriptResult(
    data: Seq[DataEntry[_]] = Nil,
    transfers: Seq[InvokeScriptResult.Payment] = Nil,
    issues: Seq[Issue] = Nil,
    reissues: Seq[Reissue] = Nil,
    burns: Seq[Burn] = Nil,
    sponsorFees: Seq[SponsorFee] = Nil,
    error: Option[ErrorMessage] = None
)

//noinspection TypeAnnotation
object InvokeScriptResult {
  val empty = InvokeScriptResult()

  final case class Payment(address: Address, asset: Asset, amount: Long)
  object Payment {
    implicit val jsonWrites = Json.writes[Payment]
  }

  def paymentsFromPortfolio(addr: Address, portfolio: Portfolio): Seq[Payment] = {
    val waves  = InvokeScriptResult.Payment(addr, Waves, portfolio.balance)
    val assets = portfolio.assets.map { case (assetId, amount) => InvokeScriptResult.Payment(addr, assetId, amount) }
    (assets.toVector ++ Some(waves)).filter(_.amount != 0)
  }

  implicit val issueFormat = Writes[Issue] { iss =>
    Json.obj(
      "assetId"        -> iss.id,
      "name"           -> iss.name,
      "description"    -> iss.description,
      "quantity"       -> iss.quantity,
      "decimals"       -> iss.decimals,
      "isReissuable"   -> iss.isReissuable,
      "compiledScript" -> iss.compiledScript,
      "nonce"          -> iss.nonce
    )
  }
  implicit val reissueFormat      = Json.writes[Reissue]
  implicit val burnFormat         = Json.writes[Burn]
  implicit val sponsorFeeFormat   = Json.writes[SponsorFee]
  implicit val errorMessageFormat = Json.writes[ErrorMessage]
  implicit val jsonFormat         = Json.writes[InvokeScriptResult]

  implicit val monoid = new Monoid[InvokeScriptResult] {
    override val empty: InvokeScriptResult =
      InvokeScriptResult.this.empty

    override def combine(x: InvokeScriptResult, y: InvokeScriptResult): InvokeScriptResult =
      InvokeScriptResult(x.data ++ y.data, x.transfers ++ y.transfers)
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
      isr.data.map(PBTransactions.toPBDataEntry),
      isr.transfers.map(
        payment =>
          PBInvokeScriptResult.Payment(
            ByteString.copyFrom(PBRecipients.publicKeyHash(payment.address)),
            Some(PBAmounts.fromAssetAndAmount(payment.asset, payment.amount))
          )
      ),
      isr.issues.map(toPbIssue),
      isr.reissues.map(toPbReissue),
      isr.burns.map(toPbBurn),
      isr.error.map(toPbErrorMessage),
      isr.sponsorFees.map(toPbSponsorFee)
    )
  }

  private def toPbIssue(r: Issue) = {
    assert(r.compiledScript.isEmpty)
    PBInvokeScriptResult.Issue(
      ByteString.copyFrom(r.id.arr),
      r.name,
      r.description,
      r.quantity,
      r.decimals,
      r.isReissuable,
      ByteString.EMPTY,
      r.nonce
    )
  }

  private def toPbReissue(r: Reissue) =
    PBInvokeScriptResult.Reissue(ByteString.copyFrom(r.assetId.arr), r.quantity, r.isReissuable)

  private def toPbBurn(b: Burn) =
    PBInvokeScriptResult.Burn(ByteString.copyFrom(b.assetId.arr), b.quantity)

  private def toPbSponsorFee(sf: SponsorFee) =
    PBInvokeScriptResult.SponsorFee(Some(Amount(sf.assetId.toByteString, sf.minSponsoredAssetFee.getOrElse(0))))

  private def toPbErrorMessage(em: ErrorMessage) =
    PBInvokeScriptResult.ErrorMessage(em.code, em.text)

  private def toVanillaIssue(r: PBInvokeScriptResult.Issue): Issue = {
    assert(r.script.isEmpty)
    Issue(r.assetId.toByteStr, None, r.decimals, r.description, r.reissuable, r.name, r.amount, r.nonce)
  }

  private def toVanillaReissue(r: PBInvokeScriptResult.Reissue) =
    Reissue(r.assetId.toByteStr, r.isReissuable, r.amount)

  private def toVanillaBurn(b: PBInvokeScriptResult.Burn) =
    Burn(b.assetId.toByteStr, b.amount)

  private def toVanillaSponsorFee(sf: PBInvokeScriptResult.SponsorFee) = {
    val amount = sf.minFee.get
    SponsorFee(amount.assetId.toByteStr, Some(amount.amount).filter(_ > 0))
  }

  private def toVanillaErrorMessage(b: PBInvokeScriptResult.ErrorMessage) =
    ErrorMessage(b.code, b.text)

  def fromPB(pbValue: PBInvokeScriptResult): InvokeScriptResult = {
    InvokeScriptResult(
      pbValue.data.map(PBTransactions.toVanillaDataEntry),
      pbValue.transfers.map { p =>
        val (asset, amount) = PBAmounts.toAssetAndAmount(p.getAmount)
        InvokeScriptResult.Payment(PBRecipients.toAddress(p.address.toByteArray, AddressScheme.current.chainId).explicitGet(), asset, amount)
      },
      pbValue.issues.map(toVanillaIssue),
      pbValue.reissues.map(toVanillaReissue),
      pbValue.burns.map(toVanillaBurn),
      pbValue.sponsorFees.map(toVanillaSponsorFee),
      pbValue.errorMessage.map(toVanillaErrorMessage)
    )
  }

  case class ErrorMessage(code: Int, text: String)
}
