package com.wavesplatform.transaction.validation.impl

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CaseObj, EVALUATED}
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

    lazy val validLength =
      if (tx.isProtobufVersion)
        PBTransactions
          .toPBInvokeScriptData(tx.dAppAddressOrAlias, tx.funcCallOpt, tx.payments)
          .toByteArray
          .length <= ContractLimits.MaxInvokeScriptSizeInBytes
      else Try(tx.bytes().length <= ContractLimits.MaxInvokeScriptSizeInBytes).getOrElse(false)

    V.seq(tx)(
      V.cond(
        funcCallOpt.isEmpty || funcCallOpt.get.args.size <= ContractLimits.MaxInvokeScriptArgs,
        GenericError(s"InvokeScript can't have more than ${ContractLimits.MaxInvokeScriptArgs} arguments")
      ),
      V.cond(
        funcCallOpt.isEmpty || (funcCallOpt.get.function match {
          case FunctionHeader.User(internalName, _) =>
            internalName.utf8Bytes.length <= ContractLimits.MaxAnnotatedFunctionNameInBytes
          case _ => true
        }),
        GenericError(s"Callable function name size in bytes must be less than ${ContractLimits.MaxAnnotatedFunctionNameInBytes} bytes")
      ),
      V.cond(
        funcCallOpt.isEmpty || funcCallOpt.get.args.forall(x => x.isInstanceOf[EVALUATED] && !x.isInstanceOf[CaseObj] && !x.isInstanceOf[ARR]),
        GenericError("All arguments of invokeScript must be one of the types: Int, ByteVector, String, Boolean")
      ),
      checkAmounts(payments),
      V.fee(fee),
      V.cond(validLength, TooBigArray)
    )
  }
}
