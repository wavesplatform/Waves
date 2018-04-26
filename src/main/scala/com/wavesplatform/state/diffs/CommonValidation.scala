package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{Portfolio, _}
import scorex.account.Address
import scorex.transaction.validation.ValidationError.{AlreadyInTheState, GenericError, Mistiming, UnsupportedTransactionType}
import scorex.transaction._
import scorex.transaction.assets.{MassTransferTransaction, VersionedTransferTransaction, _}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.base._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.modern.assets.{CancelFeeSponsorshipTx, SponsorFeeTx}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.validation.ValidationError

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration     = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours

  def disallowSendingGreaterThanBalance[T <: Transaction](blockchain: Blockchain,
                                                          settings: FunctionalitySettings,
                                                          blockTime: Long,
                                                          tx: T): Either[ValidationError, T] =
    if (blockTime >= settings.allowTemporaryNegativeUntil) {
      def checkTransfer(sender: Address, assetId: Option[AssetId], amount: Long, feeAssetId: Option[AssetId], feeAmount: Long) = {
        val amountDiff = assetId match {
          case Some(aid) => Portfolio(0, LeaseBalance.empty, Map(aid -> -amount))
          case None      => Portfolio(-amount, LeaseBalance.empty, Map.empty)
        }
        val feeDiff = feeAssetId match {
          case Some(aid) => Portfolio(0, LeaseBalance.empty, Map(aid -> -feeAmount))
          case None      => Portfolio(-feeAmount, LeaseBalance.empty, Map.empty)
        }

        val spendings       = Monoid.combine(amountDiff, feeDiff)
        val oldWavesBalance = blockchain.portfolio(sender).balance

        val newWavesBalance = oldWavesBalance + spendings.balance
        if (newWavesBalance < 0) {
          Left(
            GenericError(
              "Attempt to transfer unavailable funds: Transaction application leads to " +
                s"negative waves balance to (at least) temporary negative state, current balance equals $oldWavesBalance, " +
                s"spends equals ${spendings.balance}, result is $newWavesBalance"))
        } else if (spendings.assets.nonEmpty) {
          val oldAssetBalances = blockchain.portfolio(sender).assets
          val balanceError = spendings.assets.collectFirst {
            case (aid, delta) if oldAssetBalances.getOrElse(aid, 0L) + delta < 0 =>
              val availableBalance = oldAssetBalances.getOrElse(aid, 0L)
              GenericError(
                "Attempt to transfer unavailable funds: Transaction application leads to negative asset " +
                  s"'$aid' balance to (at least) temporary negative state, current balance is $availableBalance, " +
                  s"spends equals $delta, result is ${availableBalance + delta}")
          }

          balanceError.fold[Either[ValidationError, T]](Right(tx))(Left(_))
        } else Right(tx)
      }

      tx match {
        case ptx: PaymentTransaction if blockchain.portfolio(ptx.sender).balance < (ptx.amount + ptx.fee) =>
          Left(
            GenericError(
              "Attempt to pay unavailable funds: balance " +
                s"${blockchain.portfolio(ptx.sender).balance} is less than ${ptx.amount + ptx.fee}"))
        case ttx: TransferTransaction     => checkTransfer(ttx.sender, ttx.assetId, ttx.amount, ttx.feeAssetId, ttx.fee)
        case mtx: MassTransferTransaction => checkTransfer(mtx.sender, mtx.assetId, mtx.transfers.map(_.amount).sum, None, mtx.fee)
        case _                            => Right(tx)
      }
    } else Right(tx)

  def disallowDuplicateIds[T <: Transaction](blockchain: Blockchain,
                                             settings: FunctionalitySettings,
                                             height: Int,
                                             tx: T): Either[ValidationError, T] = tx match {
    case _: PaymentTransaction => Right(tx)
    case _                     => if (blockchain.containsTransaction(tx.id())) Left(AlreadyInTheState(tx.id(), 0)) else Right(tx)
  }

  def disallowBeforeActivationTime[T <: Transaction](blockchain: Blockchain, height: Int, tx: T): Either[ValidationError, T] = {

    def activationBarrier(b: BlockchainFeature) =
      Either.cond(
        blockchain.isFeatureActivated(b, height),
        tx,
        ValidationError.ActivationError(s"${tx.getClass.getSimpleName} transaction has not been activated yet")
      )

    tx match {
      case _: PaymentTransaction              => Right(tx)
      case _: GenesisTransaction              => Right(tx)
      case _: TransferTxBase                  => Right(tx)
      case t: IssueTxBase if t.script.isEmpty => Right(tx)
      case _: ReissueTxBase                   => Right(tx)
      case _: ExchangeTransaction             => Right(tx)
      case _: BurnTxBase                      => Right(tx)
      case _: LeaseTxBase                     => Right(tx)
      case _: LeaseCancelTxBase               => Right(tx)
      case _: CreateAliasTxBase               => Right(tx)
      case _: IssueTxBase                     => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: MassTransferTxBase              => activationBarrier(BlockchainFeatures.MassTransfer)
      case _: DataTxBase                      => activationBarrier(BlockchainFeatures.DataTransaction)
      case _: SetScriptTxBase                 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: SponsorFeeTxBase                => activationBarrier(BlockchainFeatures.FeeSponsorship)
      case _: CancelFeeSponsorshipTxBase      => activationBarrier(BlockchainFeatures.FeeSponsorship)
      case _                                  => Left(GenericError("Unknown transaction must be explicitly activated"))
    }
  }

  def disallowTxFromFuture[T <: Transaction](settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {
    val allowTransactionsFromFutureByTimestamp = tx.timestamp < settings.allowTransactionsFromFutureUntil
    if (!allowTransactionsFromFutureByTimestamp && tx.timestamp - time > MaxTimeTransactionOverBlockDiff.toMillis)
      Left(Mistiming(s"Transaction ts ${tx.timestamp} is from far future. BlockTime: $time"))
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > MaxTimePrevBlockOverTransactionDiff.toMillis =>
        Left(Mistiming(s"Transaction ts ${tx.timestamp} is too old. Previous block time: $prevBlockTime"))
      case _ => Right(tx)
    }

  def checkFee[T <: Transaction](blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: T): Either[ValidationError, T] = {
    if (height < Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) Right(tx)
    else
      for {
        feeInUnits <- tx match {
          case gtx: GenesisTransaction            => Right(0)
          case ptx: PaymentTransaction            => Right(1)
          case itx: IssueTransaction              => Right(1000)
          case sitx: SmartIssueTransaction        => Right(1000)
          case rtx: ReissueTransaction            => Right(1000)
          case btx: BurnTransaction               => Right(1)
          case ttx: TransferTransaction           => Right(1)
          case mtx: MassTransferTransaction       => Right(1 + (mtx.transfers.size + 1) / 2)
          case ltx: LeaseTransaction              => Right(1)
          case ltx: LeaseCancelTransaction        => Right(1)
          case etx: ExchangeTransaction           => Right(3)
          case atx: CreateAliasTransaction        => Right(1)
          case dtx: DataTransaction               => Right(1 + (dtx.bytes().length - 1) / 1024)
          case sstx: SetScriptTransaction         => Right(1)
          case sttx: VersionedTransferTransaction => Right(1)
          case stx: SponsorFeeTx                  => Right(1000)
          case ctx: CancelFeeSponsorshipTx        => Right(1000)
          case _                                  => Left(UnsupportedTransactionType)
        }
        wavesFee <- tx.assetFee._1 match {
          case None => Right(tx.assetFee._2)
          case Some(assetId) =>
            for {
              assetInfo <- blockchain.assetDescription(assetId).toRight(GenericError(s"Asset $assetId does not exist, cannot be used to pay fees"))
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
            s"Fee in ${tx.assetFee._1.fold("WAVES")(_.toString)} for ${tx.builder.classTag} does not exceed minimal value of $minimumFee WAVES")
        )
      } yield result
  }
}
