package com.wavesplatform.state

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.{IncompleteResult, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.protobuf.{Amount, _}
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBRecipients, PBTransactions, InvokeScriptResult => PBInvokeScriptResult}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.state.{InvokeScriptResult => R}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils._
import play.api.libs.json._

final case class InvokeScriptResult(
    data: Seq[R.DataEntry] = Nil,
    transfers: Seq[R.Payment] = Nil,
    issues: Seq[R.Issue] = Nil,
    reissues: Seq[R.Reissue] = Nil,
    burns: Seq[R.Burn] = Nil,
    sponsorFees: Seq[R.SponsorFee] = Nil,
    leases: Seq[R.Lease] = Nil,
    leaseCancels: Seq[R.LeaseCancel] = Nil,
    invokes: Seq[R.Invocation] = Nil,
    error: Option[R.ErrorMessage] = None
)

//noinspection TypeAnnotation
object InvokeScriptResult {
  type LeaseCancel = com.wavesplatform.lang.v1.traits.domain.LeaseCancel
  type SponsorFee = com.wavesplatform.lang.v1.traits.domain.SponsorFee
  type Issue = com.wavesplatform.lang.v1.traits.domain.Issue
  type Reissue = com.wavesplatform.lang.v1.traits.domain.Reissue
  type Burn = com.wavesplatform.lang.v1.traits.domain.Burn
  type DataEntry = com.wavesplatform.state.DataEntry[_]

  val empty = InvokeScriptResult()

  final case class AttachedPayment(asset: Asset, amount: Long)
  object AttachedPayment {
    implicit val attachedPaymentWrites = Json.writes[AttachedPayment]

    def fromInvokePaymentList(ps: Seq[InvokeScriptTransaction.Payment]): Seq[AttachedPayment] =
      ps.map(p => AttachedPayment(p.assetId, p.amount))
  }

  final case class Call(function: String, args: Seq[EVALUATED])
  object Call {
    implicit val callWrites = Json.writes[Call]

    def fromFunctionCall(fc: FUNCTION_CALL): Call = Call(fc.function.funcName, fc.args.collect { case e: EVALUATED => e })
  }

  final case class Invocation(dApp: Address, call: Call, payments: Seq[AttachedPayment], stateChanges: InvokeScriptResult)
  object Invocation {
    def calledAddresses(inv: InvokeScriptResult.Invocation): LazyList[Address] =
      LazyList(inv.dApp) #::: inv.stateChanges.invokes.to(LazyList).flatMap(calledAddresses)

    def calledAddresses(invs: Iterable[InvokeScriptResult.Invocation]): LazyList[Address] =
      invs.to(LazyList).flatMap(calledAddresses)
  }

  final case class Payment(address: Address, asset: Asset, amount: Long)
  object Payment {
    implicit val jsonWrites = Json.writes[Payment]
  }

  case class Lease(recipient: AddressOrAlias, amount: Long, nonce: Long, id: ByteStr)
  object Lease {
    implicit val recipientWrites = Writes[AddressOrAlias] {
      case address: Address => implicitly[Writes[Address]].writes(address)
      case alias: Alias     => JsString(alias.stringRepr)
      case _                => JsNull
    }
    implicit val jsonWrites = Json.writes[Lease]
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
  implicit val leaseCancelFormat  = Json.writes[LeaseCancel]
  implicit val errorMessageFormat = Json.writes[ErrorMessage]
  implicit val invocationFormat: Writes[Invocation] = new Writes[Invocation] {
    override def writes(i: Invocation) = Json.obj(
      "dApp"         -> i.dApp,
      "call"         -> i.call,
      "payments"     -> i.payments,
      "stateChanges" -> jsonFormat.writes(i.stateChanges)
    )
  }
  implicit val jsonFormat = Json.writes[InvokeScriptResult]

  implicit val monoid = new Monoid[InvokeScriptResult] {
    override val empty: InvokeScriptResult =
      InvokeScriptResult.this.empty

    override def combine(x: InvokeScriptResult, y: InvokeScriptResult): InvokeScriptResult = {
      InvokeScriptResult(
        data = x.data ++ y.data,
        transfers = x.transfers ++ y.transfers,
        issues = x.issues ++ y.issues,
        reissues = x.reissues ++ y.reissues,
        burns = x.burns ++ y.burns,
        sponsorFees = x.sponsorFees ++ y.sponsorFees,
        invokes = x.invokes ++ y.invokes,
        leases = x.leases ++ y.leases,
        leaseCancels = x.leaseCancels ++ y.leaseCancels,
        error = x.error.orElse(y.error)
      )
    }
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
      isr.sponsorFees.map(toPbSponsorFee),
      isr.leases.map(toPbLease),
      isr.leaseCancels.map(toPbLeaseCancel),
      isr.invokes.map(toPbInvocation)
    )
  }

  def fromLangResult(invokeId: ByteStr, result: ScriptResult): InvokeScriptResult = {
    import com.wavesplatform.lang.v1.traits.{domain => lang}

    def langAddressToAddress(a: lang.Recipient.Address): Address =
      Address.fromBytes(a.bytes.arr).explicitGet()

    def langTransferToPayment(t: lang.AssetTransfer): Payment =
      Payment(langAddressToAddress(t.address), Asset.fromCompatId(t.assetId), t.amount)

    def langLeaseToLease(l: lang.Lease): Lease =
      Lease(AddressOrAlias.fromRide(l.recipient).explicitGet(), l.amount, l.nonce, lang.Lease.calculateId(l, invokeId))

    result match {
      case ScriptResultV3(ds, ts, _) =>
        InvokeScriptResult(data = ds.map(DataEntry.fromLangDataOp), transfers = ts.map(langTransferToPayment))

      case ScriptResultV4(actions, _, ret) =>
        // XXX need return value processing
        val issues       = actions.collect { case i: lang.Issue         => i }
        val reissues     = actions.collect { case ri: lang.Reissue      => ri }
        val burns        = actions.collect { case b: lang.Burn          => b }
        val sponsorFees  = actions.collect { case sf: lang.SponsorFee   => sf }
        val dataOps      = actions.collect { case d: lang.DataOp        => DataEntry.fromLangDataOp(d) }
        val transfers    = actions.collect { case t: lang.AssetTransfer => langTransferToPayment(t) }
        val leases       = actions.collect { case l: lang.Lease         => langLeaseToLease(l) }
        val leaseCancels = actions.collect { case l: lang.LeaseCancel   => l }
        val invokes = result.invokes.map {
          case (dApp, fname, args, payments, r) =>
            Invocation(
              langAddressToAddress(dApp),
              Call(fname, args),
              (payments.map {
                case CaseObj(t, fields) =>
                  (fields("assetId"), fields("amount")) match {
                    case (CONST_BYTESTR(b), CONST_LONG(a)) => InvokeScriptResult.AttachedPayment(IssuedAsset(b), a)
                    case (_, CONST_LONG(a))                => InvokeScriptResult.AttachedPayment(Waves, a)
                  }
              }),
              fromLangResult(invokeId, r)
            )
        }
        InvokeScriptResult(dataOps, transfers, issues, reissues, burns, sponsorFees, leases, leaseCancels, invokes)

      case i: IncompleteResult => throw new IllegalArgumentException(s"Cannot cast incomplete result: $i")
    }
  }

  private def toPbCall(c: Call): PBInvokeScriptResult.Call = {
    PBInvokeScriptResult.Call(c.function, c.args.map(b => ByteString.copyFrom(Serde.serialize(b, true))))
  }

  private def toPbInvocation(i: Invocation) = {
    PBInvokeScriptResult.Invocation(
      ByteString.copyFrom(i.dApp.bytes),
      Some(toPbCall(i.call)),
      i.payments.map(p => Amount(PBAmounts.toPBAssetId(p.asset), p.amount)),
      Some(toPB(i.stateChanges))
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

  private def toPbLease(l: Lease) =
    PBInvokeScriptResult.Lease(Some(PBRecipients.create(l.recipient)), l.amount, l.nonce, l.id.toByteString)

  private def toPbLeaseCancel(l: LeaseCancel) =
    PBInvokeScriptResult.LeaseCancel(ByteString.copyFrom(l.id.arr))

  private def toPbErrorMessage(em: ErrorMessage) =
    PBInvokeScriptResult.ErrorMessage(em.code, em.text)

  private def toVanillaCall(i: PBInvokeScriptResult.Call): Call = {
    Call(i.function, i.args.map(a => Serde.deserialize(a.toByteArray, true, true).explicitGet()._1.asInstanceOf[EVALUATED]))
  }

  private def toVanillaInvocation(i: PBInvokeScriptResult.Invocation): Invocation = {
    Invocation(
      PBRecipients.toAddress(i.dApp.toByteArray, AddressScheme.current.chainId).explicitGet(),
      toVanillaCall(i.call.get),
      i.payments.map { p =>
        val (asset, amount) = PBAmounts.toAssetAndAmount(p)
        InvokeScriptResult.AttachedPayment(asset, amount)
      },
      fromPB(i.stateChanges.get)
    )
  }

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

  private def toVanillaLease(l: PBInvokeScriptResult.Lease) = {
    val recipient = PBRecipients.toAddressOrAlias(l.getRecipient, AddressScheme.current.chainId).explicitGet()
    Lease(recipient, l.amount, l.nonce, l.leaseId.toByteStr)
  }

  private def toVanillaLeaseCancel(sf: PBInvokeScriptResult.LeaseCancel) =
    LeaseCancel(sf.leaseId.toByteStr)

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
      pbValue.leases.map(toVanillaLease),
      pbValue.leaseCancels.map(toVanillaLeaseCancel),
      pbValue.invokes.map(toVanillaInvocation),
      pbValue.errorMessage.map(toVanillaErrorMessage)
    )
  }

  case class ErrorMessage(code: Int, text: String)
}
