package com.wavesplatform.api.http.utils

import cats.implicits.{toBifunctorOps, toTraverseOps}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.requests.InvokeScriptRequest
import com.wavesplatform.api.http.requests.InvokeScriptRequest.FunctionCallPart
import com.wavesplatform.api.http.utils.BlockchainOverrides.AccountOverrides
import com.wavesplatform.api.http.utils.UtilsInvocationRequest.empty32Bytes
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address as RideAddress
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.AttachedPaymentExtractor
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{Asset, TransactionType, smart}
import play.api.libs.json.*
import supertagged.TaggedType

case class UtilsInvocationRequest(
    call: FunctionCallPart = FunctionCallPart("default", Nil),
    id: String = ByteStr(empty32Bytes).toString,
    fee: Long = FeeConstants(TransactionType.InvokeScript) * FeeUnit,
    feeAssetId: Option[String] = None,
    sender: Option[String] = None,
    senderPublicKey: String = ByteStr(empty32Bytes).toString,
    payment: Seq[Payment] = Nil,
    state: BlockchainOverrides = BlockchainOverrides()
) {
  def toInvocation: Either[ValidationError, Invocation] =
    for {
      senderPK <- PublicKey.fromBase58String(senderPublicKey)
      id       <- decodeBase58(id)
      functionCall = InvokeScriptRequest.buildFunctionCall(call)
      feeAssetId <- feeAssetId.traverse(decodeBase58)
      sender <-
        if (sender.nonEmpty || senderPK.arr.sameElements(empty32Bytes))
          sender
            .map(Address.fromString(_, None).map(a => RideAddress(ByteStr(a.bytes))))
            .getOrElse(Right(RideAddress(ByteStr(new Array[Byte](26)))))
        else
          Right(RideAddress(ByteStr(senderPK.toAddress.bytes)))
      payments <- AttachedPaymentExtractor
        .extractPayments(payment, V6, blockchainAllowsMultiPayment = true, smart.DApp)
        .leftMap(GenericError(_))
    } yield Invocation(functionCall, sender, senderPK, sender, senderPK, payments, id, fee, feeAssetId)

  private def decodeBase58(base58: String): Either[ValidationError, ByteStr] =
    ByteStr.decodeBase58(base58).toEither.leftMap(e => GenericError(String.valueOf(e.getMessage)))
}

object UtilsInvocationRequest {
  private val empty32Bytes = new Array[Byte](32)

  implicit val reads: Reads[UtilsInvocationRequest] = Json.using[Json.WithDefaultValues].reads[UtilsInvocationRequest]
}

case class BlockchainOverrides(accounts: Map[Address, AccountOverrides] = Map.empty) {
  def balance(address: Address, mayBeAssetId: Asset): Option[Long] = accounts.get(address).flatMap { account =>
    mayBeAssetId.fold(account.regularBalance)(account.assetBalances.get)
  }
}

object BlockchainOverrides {

  object Balance extends TaggedType[Long] {
    private val LongStringMaxLength = 20 // Long.MinValue.toString

    implicit val reads: Reads[Type] = Reads {
      case JsString(s) =>
        if (s.length > LongStringMaxLength) JsError("error.expected.numberdigitlimit")
        else
          s.toLongOption match {
            case Some(r) => JsSuccess(Balance(r))
            case None    => JsError(JsonValidationError("error.expected.numberformatexception"))
          }

      case JsNumber(d) =>
        if (d.isValidLong) JsSuccess(Balance(d.toLongExact))
        else JsError(JsonValidationError("error.invalid.long"))

      case _ => JsError(JsonValidationError("error.expected.jsnumberorjsstring"))
    }
  }

  type Balance = Balance.Type

  // TODO test errors, JsPath?
  implicit val accountsMapReads: Reads[Map[Address, AccountOverrides]] =
    Reads.mapReads[Address, AccountOverrides](x => Address.fromString(x).fold(e => JsError(s"Can't parse Address in accounts: $e"), JsSuccess(_)))

  implicit val reads: Reads[BlockchainOverrides] = Json.using[Json.WithDefaultValues].reads[BlockchainOverrides]

  case class AccountOverrides(assetBalances: Map[IssuedAsset, Balance] = Map.empty, regularBalance: Option[Balance] = None)

  object AccountOverrides {
    implicit val assetBalancesMapReads: Reads[Map[IssuedAsset, Balance]] =
      Reads.mapReads[IssuedAsset, Balance](x => Asset.assetReads.reads(JsString(x)))

    implicit val reads: Reads[AccountOverrides] = Json.using[Json.WithDefaultValues].reads[AccountOverrides]
  }
}
