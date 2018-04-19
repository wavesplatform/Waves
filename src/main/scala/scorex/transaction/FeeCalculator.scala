package scorex.transaction

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.FeesSettings
import com.wavesplatform.state2.{ByteStr, Sponsorship}
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.transaction.ValidationError.{GenericError, UnsupportedTransactionType}
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.SetScriptTransaction

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: FeesSettings) {

  private val Kb = 1024

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

  def enoughFee[T <: Transaction](tx: T, s: SnapshotStateReader, history: History): Either[ValidationError, T] = {
    if (history.isFeatureActivated(BlockchainFeatures.SponsoredFee, history.height))
      enoughFee(tx, s)
    else
      enoughFee(tx)
  }

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    val feeSpec = map.get(TransactionAssetFee(tx.builder.typeId, tx.assetFee._1).key)
    val feeValue = tx match {
      case dt: DataTransaction =>
        val sizeInKb = 1 + (dt.bytes().length - 1) / Kb
        feeSpec.map(_ * sizeInKb)
      case mtt: MassTransferTransaction =>
        val transferFeeSpec = map.get(TransactionAssetFee(TransferTransaction.typeId, tx.assetFee._1).key)
        feeSpec.flatMap(mfee => transferFeeSpec.map(tfee => tfee + mfee * mtt.transfers.size))
      case _ => feeSpec
    }

    feeValue match {
      case Some(minimumFee) =>
        if (minimumFee <= tx.assetFee._2) {
          Right(tx)
        } else {
          Left(GenericError(
            s"Fee in ${tx.assetFee._1.fold("WAVES")(_.toString)} for ${tx.builder.classTag} transaction does not exceed minimal value of $minimumFee"))
        }
      case None =>
        Left(GenericError(s"Minimum fee is not defined for ${TransactionAssetFee(tx.builder.typeId, tx.assetFee._1).key}"))
    }
  }

  private def enoughFee[T <: Transaction](tx: T, s: SnapshotStateReader): Either[ValidationError, T] =
    for {
      feeInUnits <- tx match {
        case gtx: GenesisTransaction              => Right(0)
        case ptx: PaymentTransaction              => Right(1)
        case itx: IssueTransaction                => Right(1000)
        case sitx: SmartIssueTransaction          => Right(1000)
        case rtx: ReissueTransaction              => Right(1)
        case btx: BurnTransaction                 => Right(1)
        case ttx: TransferTransaction             => Right(1)
        case mtx: MassTransferTransaction         => Right(1 + (mtx.transfers.size + 1) / 2)
        case ltx: LeaseTransaction                => Right(1)
        case ltx: LeaseCancelTransaction          => Right(1)
        case etx: ExchangeTransaction             => Right(3)
        case atx: CreateAliasTransaction          => Right(1)
        case dtx: DataTransaction                 => Right(1 + (dtx.bytes().length - 1) / 1024)
        case sstx: SetScriptTransaction           => Right(1)
        case sttx: VersionedTransferTransaction   => Right(1)
        case stx: SponsorFeeTransaction           => Right(1000)
        case ctx: CancelFeeSponsorshipTransaction => Right(1000)
        case _                                    => Left(UnsupportedTransactionType)
      }
      wavesFee <- tx.assetFee._1 match {
        case None => Right(tx.assetFee._2)
        case Some(assetId) =>
          for {
            assetInfo <- s.assetDescription(assetId).toRight(GenericError(s"Asset $assetId does not exist, cannot be used to pay fees"))
            wavesFee <- Either.cond(
              assetInfo.sponsorship > 0,
              Sponsorship.toWaves(tx.assetFee._2, assetInfo.sponsorship),
              GenericError(s"Asset $assetId is not sponsored, cannot be used to pay fees")
            )
          } yield wavesFee
      }
      minimumFee = feeInUnits * Sponsorship.FeeUnit
      result <- Either.cond(
        wavesFee >= minimumFee,
        tx,
        GenericError(
          s"Fee in ${tx.assetFee._1.fold("WAVES")(_.toString)} for ${tx.builder.classTag} transaction does not exceed minimal value of $minimumFee WAVES")
      )
    } yield result
}

case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {

  val key = s"TransactionAssetFee($txType, ${assetId.map(_.base58)})"

}
