package com.wavesplatform.transaction.validation.impl

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.transaction.TxValidationError.{GenericError, NonPositiveAmount, TooBigArray}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedNV, ValidatedV}

object InvokeScriptTxValidator extends TxValidator[InvokeScriptTransaction] {
  override def validate(tx: InvokeScriptTransaction): ValidatedV[InvokeScriptTransaction] = {
    def checkAmounts(payments: Seq[Payment]): ValidatedNV = {
      val invalid = payments.filter(_.amount <= 0)
      if (invalid.nonEmpty)
        Invalid(NonEmptyList.fromListUnsafe(invalid.toList).map(p => NonPositiveAmount(p.amount, p.assetId.fold("Waves")(_.toString))))
      else Valid(())
    }

    import tx._
    V.seq(tx)(
      V.cond(
        funcCallOpt.isEmpty || funcCallOpt.get.args.size <= ContractLimits.MaxInvokeScriptArgs,
        GenericError(s"InvokeScript can't have more than ${ContractLimits.MaxInvokeScriptArgs} arguments")
      ),
      V.cond(
        funcCallOpt.isEmpty || (funcCallOpt.get.function match {
          case FunctionHeader.User(internalName, _) =>
            internalName.getBytes("UTF-8").length <= ContractLimits.MaxAnnotatedFunctionNameInBytes
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
      V.cond(tx.bytes().length <= ContractLimits.MaxInvokeScriptSizeInBytes, TooBigArray)
    )
  }
}
