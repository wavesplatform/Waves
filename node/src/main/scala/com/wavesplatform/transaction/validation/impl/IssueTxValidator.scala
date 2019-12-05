package com.wavesplatform.transaction.validation.impl

import cats.data.Validated
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}
import com.wavesplatform.transaction.{TxValidationError, TxVersion}

object IssueTxValidator extends TxValidator[IssueTransaction] {
  override def validate(tx: IssueTransaction): ValidatedV[IssueTransaction] = {
    def assetName(name: Array[Byte]): ValidatedV[Array[Byte]] = {
      Validated
        .condNel(
          name.length >= IssueTransaction.MinAssetNameLength && name.length <= IssueTransaction.MaxAssetNameLength,
          name,
          TxValidationError.InvalidName
        )
    }

    def assetDescription(description: Array[Byte]): ValidatedV[Array[Byte]] = {
      Validated
        .condNel(
          description.length <= IssueTransaction.MaxAssetDescriptionLength,
          description,
          TxValidationError.TooBigArray
        )
    }

    def assetDecimals(decimals: Byte): ValidatedV[Byte] = {
      Validated
        .condNel(
          decimals >= 0 && decimals <= IssueTransaction.MaxAssetDecimals,
          decimals,
          TxValidationError.TooBigArray
        )
    }

    import tx._
    V.seq(tx)(
      V.positiveAmount(quantity, "assets"),
      assetName(tx.nameBytes),
      assetDescription(tx.descriptionBytes),
      assetDecimals(decimals),
      V.fee(fee),
      V.cond(version > TxVersion.V1 || script.isEmpty, GenericError("Script not supported")),
      V.cond(script.forall(_.isInstanceOf[ExprScript]), GenericError(s"Asset can only be assigned with Expression script, not Contract"))
    )
  }
}
