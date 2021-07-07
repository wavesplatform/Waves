package com.wavesplatform.transaction.validation.impl

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.either._
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.TxValidationError.{GenericError, NonPositiveAmount, TooBigArray}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedNV, ValidatedV}
import com.wavesplatform.utils._

import scala.util.Try

object InvokeScriptTxValidator extends TxValidator[InvokeScriptTransaction] {
  override def validate(tx: InvokeScriptTransaction): ValidatedV[InvokeScriptTransaction] = {
    import tx._

    def checkAmounts(payments: Seq[Payment]): ValidatedNV = {
      val invalid = payments.filter(_.amount <= 0)
      if (invalid.nonEmpty)
        Invalid(NonEmptyList.fromListUnsafe(invalid.toList).map(p => NonPositiveAmount(p.amount, p.assetId.fold("Waves")(_.toString))))
      else Valid(())
    }

    def checkLength =
      if (tx.isProtobufVersion)
        PBTransactions
          .toPBInvokeScriptData(tx.dApp, tx.funcCallOpt, tx.payments)
          .toByteArray
          .length <= ContractLimits.MaxInvokeScriptSizeInBytes
      else tx.bytes().length <= ContractLimits.MaxInvokeScriptSizeInBytes

    val callableNameSize =
      funcCallOpt match {
        case Some(FUNCTION_CALL(FunctionHeader.User(internalName, _), _)) => internalName.utf8Bytes.length
        case _ => 0
      }

    V.seq(tx)(
      V.addressChainId(dApp, chainId),
      V.cond(
        funcCallOpt.isEmpty || funcCallOpt.get.args.size <= ContractLimits.MaxInvokeScriptArgs,
        GenericError(s"InvokeScript can't have more than ${ContractLimits.MaxInvokeScriptArgs} arguments")
      ),
      V.cond(
        callableNameSize <= ContractLimits.MaxDeclarationNameInBytes,
        GenericError(s"Callable function name size = $callableNameSize bytes must be less than ${ContractLimits.MaxDeclarationNameInBytes}")
      ),
      checkAmounts(payments),
      V.fee(fee),
      Try(checkLength)
        .toEither
        .leftMap(err => GenericError(err.getMessage))
        .filterOrElse(identity, TooBigArray)
        .toValidatedNel
        .map(_ => ())
    )
  }
}
