package scorex.transaction

import com.wavesplatform.lang.v1.ctx.Context
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptComplexityCalculator}
import com.wavesplatform.settings.{FeesSettings, FunctionalitySettings}
import com.wavesplatform.state._
import scorex.transaction.FeeCalculator._
import scorex.transaction.ValidationError.{GenericError, InsufficientFee}
import scorex.transaction.assets._
import scorex.transaction.smart.script.Script

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: FeesSettings, blockchain: Blockchain) {

  private val Kb = 1024

  private val functionCosts: Map[FunctionHeader, Long] = Context.functionCosts(com.wavesplatform.utils.dummyContext.functions.values)

  private val map: Map[String, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.map { v =>
        val maybeAsset = if (v.asset.toUpperCase == "WAVES") None else ByteStr.decodeBase58(v.asset).toOption
        val fee        = v.fee

        TransactionAssetFee(transactionType, maybeAsset).key -> fee
      }
    }
  }

  def enoughFee[T <: Transaction](tx: T, blockchain: Blockchain, fs: FunctionalitySettings): Either[ValidationError, T] =
    if (blockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) Right(tx)
    else enoughFee(tx)

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    val (txFeeAssetId, txFeeValue) = tx.assetFee
    val txAssetFeeKey              = TransactionAssetFee(tx.builder.typeId, txFeeAssetId).key
    for {
      txMinBaseFee <- Either.cond(map.contains(txAssetFeeKey), map(txAssetFeeKey), GenericError(s"Minimum fee is not defined for $txAssetFeeKey"))
      script <- Right(tx match {
        case tx: Transaction with Authorized => blockchain.accountScript(tx.sender)
        case _                               => None
      })
      _ <- Either.cond(script.isEmpty || txFeeAssetId.isEmpty,
                       (),
                       ValidationError.GenericError("Scripted accounts can accept transactions with Waves as fee only"))
      minTxFee = minFeeFor(tx, txFeeAssetId, txMinBaseFee)
      _ <- Either.cond(
        txFeeValue >= minTxFee,
        (),
        GenericError {
          s"Fee in ${txFeeAssetId.fold("WAVES")(_.toString)} for ${tx.builder.classTag} transaction does not exceed minimal value of $minTxFee"
        }
      )

      scriptComplexity <- script match {
        case Some(Script.Expr(expr)) =>
          ScriptComplexityCalculator(functionCosts, expr) match {
            case Right(x) => Right(settings.smartAccount.baseExtraCharge + x)
            case Left(e)  => Left(ValidationError.GenericError(e))
          }
        case Some(x) => throw new IllegalStateException(s"Doesn't know how to calculate complexity for a script of ${x.version} version")
        case None    => Right(0L)
      }

      totalRequiredFee = minTxFee + (scriptComplexity * settings.smartAccount.extraChargePerOp).toLong
      _ <- Either.cond(
        txFeeValue >= totalRequiredFee,
        (),
        InsufficientFee(s"Scripted account requires $totalRequiredFee fee for this transaction, but given: $txFeeValue")
      )
    } yield tx
  }

  private def minFeeFor(tx: Transaction, txFeeAssetId: Option[AssetId], txMinBaseFee: Long): Long = tx match {
    case tx: DataTransaction =>
      val sizeInKb = 1 + (tx.bytes().length - 1) / Kb
      txMinBaseFee * sizeInKb
    case tx: MassTransferTransaction =>
      val transferFeeSpec = map.getOrElse(
        TransactionAssetFee(V1TransferTransaction.typeId, txFeeAssetId).key,
        throw new IllegalStateException("Can't find spec for TransferTransaction")
      )
      transferFeeSpec + txMinBaseFee * tx.transfers.size
    case _ => txMinBaseFee
  }
}

object FeeCalculator {

  private case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {
    val key = s"TransactionAssetFee($txType, ${assetId.map(_.base58)})"
  }

}
