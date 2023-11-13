package com.wavesplatform.state

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.{Terms, Types}
import com.wavesplatform.lang.v1.evaluator.{IncompleteResult, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.protobuf.transaction.InvokeScriptResult.Call.Argument
import com.wavesplatform.protobuf.transaction.InvokeScriptResult.Call.Argument.Value
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBRecipients, PBTransactions, InvokeScriptResult as PBInvokeScriptResult}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.protobuf.{Amount, *}
import com.wavesplatform.state.InvokeScriptResult as R
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils.*
import play.api.libs.json.*

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
  type SponsorFee  = com.wavesplatform.lang.v1.traits.domain.SponsorFee
  type Issue       = com.wavesplatform.lang.v1.traits.domain.Issue
  type Reissue     = com.wavesplatform.lang.v1.traits.domain.Reissue
  type Burn        = com.wavesplatform.lang.v1.traits.domain.Burn
  type DataEntry   = com.wavesplatform.state.DataEntry[?]

  val empty = InvokeScriptResult()

  final case class AttachedPayment(assetId: Asset, amount: Long)
  object AttachedPayment {
    implicit val attachedPaymentWrites: OWrites[AttachedPayment] = Json.writes[AttachedPayment]

    def fromInvokePaymentList(ps: Seq[InvokeScriptTransaction.Payment]): Seq[AttachedPayment] =
      ps.map(p => AttachedPayment(p.assetId, p.amount))
  }

  final case class Call(function: String, args: Seq[EVALUATED])
  object Call {
    implicit val callWrites: OWrites[Call] = Json.writes[Call]

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
    implicit val jsonWrites: OWrites[Payment] = Json.writes[Payment]
  }

  case class Lease(recipient: AddressOrAlias, amount: Long, nonce: Long, id: ByteStr)
  object Lease {
    implicit val recipientWrites: Writes[AddressOrAlias] = Writes[AddressOrAlias] {
      case address: Address => implicitly[Writes[Address]].writes(address)
      case alias: Alias     => JsString(alias.toString)
      case _                => JsNull
    }
    implicit val jsonWrites: OWrites[Lease] = Json.writes[Lease]
  }

  def paymentsFromPortfolio(addr: Address, portfolio: Portfolio): Seq[Payment] = {
    val waves  = InvokeScriptResult.Payment(addr, Waves, portfolio.balance)
    val assets = portfolio.assets.map { case (assetId, amount) => InvokeScriptResult.Payment(addr, assetId, amount) }
    (assets.toVector ++ Some(waves)).filter(_.amount != 0)
  }

  implicit val issueFormat: Writes[Issue] = Writes[Issue] { iss =>
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
  implicit val reissueFormat: OWrites[Reissue]           = Json.writes[Reissue]
  implicit val burnFormat: OWrites[Burn]                 = Json.writes[Burn]
  implicit val sponsorFeeFormat: OWrites[SponsorFee]     = Json.writes[SponsorFee]
  implicit val leaseCancelFormat: OWrites[LeaseCancel]   = Json.writes[LeaseCancel]
  implicit val errorMessageFormat: OWrites[ErrorMessage] = Json.writes[ErrorMessage]
  implicit val invocationFormat: Writes[Invocation] = (i: Invocation) =>
    Json.obj(
      "dApp"         -> i.dApp.toString,
      "call"         -> i.call,
      "payment"      -> i.payments,
      "stateChanges" -> jsonFormat.writes(i.stateChanges)
    )
  implicit val jsonFormat: OWrites[InvokeScriptResult] = Json.writes[InvokeScriptResult]

  implicit val monoid: Monoid[InvokeScriptResult] = new Monoid[InvokeScriptResult] {
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
    val pbValue = this.toPB(isr, addressForTransfer = false)
    PBUtils.encodeDeterministic(pbValue)
  }

  def fromBytes(bs: Array[Byte]): InvokeScriptResult = {
    val pbValue = PBInvokeScriptResult.parseFrom(bs)
    fromPB(pbValue)
  }

  def toPB(isr: InvokeScriptResult, addressForTransfer: Boolean): PBInvokeScriptResult = {
    PBInvokeScriptResult(
      isr.data.map(PBTransactions.toPBDataEntry),
      isr.transfers.map { payment =>
        val sender = if (addressForTransfer) payment.address.bytes else PBRecipients.publicKeyHash(payment.address)
        PBInvokeScriptResult.Payment(
          ByteString.copyFrom(sender),
          Some(PBAmounts.fromAssetAndAmount(payment.asset, payment.amount))
        )
      },
      isr.issues.map(toPbIssue),
      isr.reissues.map(toPbReissue),
      isr.burns.map(toPbBurn),
      isr.error.map(toPbErrorMessage),
      isr.sponsorFees.map(toPbSponsorFee),
      isr.leases.map(toPbLease),
      isr.leaseCancels.map(toPbLeaseCancel),
      isr.invokes.map(toPbInvocation(_, addressForTransfer))
    )
  }

  def fromLangResult(invokeId: ByteStr, result: ScriptResult): InvokeScriptResult = {
    import com.wavesplatform.lang.v1.traits.domain as lang

    def langAddressToAddress(a: lang.Recipient.Address): Address =
      Address.fromBytes(a.bytes.arr).explicitGet()

    def langTransferToPayment(t: lang.AssetTransfer): Payment =
      Payment(langAddressToAddress(t.recipientAddressBytes), Asset.fromCompatId(t.assetId), t.amount)

    def langLeaseToLease(l: lang.Lease): Lease =
      Lease(AddressOrAlias.fromRide(l.recipient).explicitGet(), l.amount, l.nonce, lang.Lease.calculateId(l, invokeId))

    result match {
      case ScriptResultV3(ds, ts, _) =>
        InvokeScriptResult(data = ds.map(DataEntry.fromLangDataOp), transfers = ts.map(langTransferToPayment))

      case ScriptResultV4(actions, _, _) =>
        // XXX need return value processing
        val issues       = actions.collect { case i: lang.Issue => i }
        val reissues     = actions.collect { case ri: lang.Reissue => ri }
        val burns        = actions.collect { case b: lang.Burn => b }
        val sponsorFees  = actions.collect { case sf: lang.SponsorFee => sf }
        val dataOps      = actions.collect { case d: lang.DataOp => DataEntry.fromLangDataOp(d) }
        val transfers    = actions.collect { case t: lang.AssetTransfer => langTransferToPayment(t) }
        val leases       = actions.collect { case l: lang.Lease => langLeaseToLease(l) }
        val leaseCancels = actions.collect { case l: lang.LeaseCancel => l }
        val invokes = result.invokes.map { case (dApp, fname, args, payments, r) =>
          Invocation(
            langAddressToAddress(dApp),
            Call(fname, args),
            payments.map { case CaseObj(_, fields) =>
              ((fields("assetId"), fields("amount")): @unchecked) match {
                case (CONST_BYTESTR(b), CONST_LONG(a)) => InvokeScriptResult.AttachedPayment(IssuedAsset(b), a)
                case (_, CONST_LONG(a))                => InvokeScriptResult.AttachedPayment(Waves, a)
              }
            },
            fromLangResult(invokeId, r)
          )
        }
        InvokeScriptResult(dataOps, transfers, issues, reissues, burns, sponsorFees, leases, leaseCancels, invokes)

      case i: IncompleteResult => throw new IllegalArgumentException(s"Cannot cast incomplete result: $i")
    }
  }

  import com.wavesplatform.protobuf.transaction.InvokeScriptResult as PBISR

  def rideExprToPB(arg: Terms.EXPR): PBISR.Call.Argument.Value = {
    import PBISR.Call.Argument.Value

    arg match {
      case Terms.CONST_LONG(t)     => Value.IntegerValue(t)
      case bs: Terms.CONST_BYTESTR => Value.BinaryValue(bs.bs.toByteString)
      case str: Terms.CONST_STRING => Value.StringValue(str.s)
      case Terms.CONST_BOOLEAN(b)  => Value.BooleanValue(b)
      case Terms.ARR(xs)           => Value.List(Argument.List(xs.map(x => Argument(rideExprToPB(x)))))
      case caseObj: Terms.CaseObj  => Value.CaseObj(ByteString.copyFrom(SerdeV1.serialize(caseObj, allowObjects = true)))
      case _                       => Value.Empty
    }
  }

  private def toPbCall(c: Call): PBInvokeScriptResult.Call = {
    // argsBytes = c.args.map(b => ByteString.copyFrom(Serde.serialize(b, true)))
    PBInvokeScriptResult.Call(c.function, args = c.args.map(a => PBISR.Call.Argument(rideExprToPB(a))))
  }

  private def toPbInvocation(i: Invocation, addressForTransfer: Boolean) = {
    PBInvokeScriptResult.Invocation(
      ByteString.copyFrom(i.dApp.bytes),
      Some(toPbCall(i.call)),
      i.payments.map(p => Amount(PBAmounts.toPBAssetId(p.assetId), p.amount)),
      Some(toPB(i.stateChanges, addressForTransfer))
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
    import com.wavesplatform.lang.v1.compiler.Terms

    def toVanillaTerm(v: Argument.Value): Terms.EVALUATED =
      v match {
        case Value.IntegerValue(value) => Terms.CONST_LONG(value)
        case Value.BinaryValue(value)  => Terms.CONST_BYTESTR(value.toByteStr).explicitGet()
        case Value.StringValue(value)  => Terms.CONST_STRING(value).explicitGet()
        case Value.BooleanValue(value) => Terms.CONST_BOOLEAN(value)
        case Value.List(value)         => Terms.ARR(value.items.map(a => toVanillaTerm(a.value)).toVector, limited = true).explicitGet()
        case Value.CaseObj(bytes) =>
          SerdeV1
            .deserialize(bytes.toByteArray, allowObjects = true)
            .toOption
            .collect { case (obj: CaseObj, _) => obj }
            .getOrElse(Terms.CaseObj(Types.UNIT, Map.empty))
        case _ => Terms.CaseObj(Types.UNIT, Map.empty)
      }

    val args = if (i.argsBytes.nonEmpty) i.argsBytes.map { bytes =>
      val (value, _) = SerdeV1.deserialize(bytes.toByteArray, allowObjects = true).explicitGet()
      value.asInstanceOf[EVALUATED]
    }
    else i.args.map(a => toVanillaTerm(a.value))
    Call(i.function, args)
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
