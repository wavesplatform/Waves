package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{Portfolio, _}
import scorex.account.Address
import scorex.transaction.ValidationError._
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.transfer._

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration     = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours
  private val ScriptExtraFee                              = 400000L

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
      case _: BurnTransaction         => Right(tx)
      case _: PaymentTransaction      => Right(tx)
      case _: GenesisTransaction      => Right(tx)
      case _: TransferTransactionV1   => Right(tx)
      case _: IssueTransactionV1      => Right(tx)
      case _: ReissueTransactionV1    => Right(tx)
      case _: ExchangeTransaction     => Right(tx)
      case _: LeaseTransaction        => Right(tx)
      case _: LeaseCancelTransaction  => Right(tx)
      case _: CreateAliasTransaction  => Right(tx)
      case _: MassTransferTransaction => activationBarrier(BlockchainFeatures.MassTransfer)
      case _: DataTransaction         => activationBarrier(BlockchainFeatures.DataTransaction)
      case _: SetScriptTransaction    => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: TransferTransactionV2   => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: IssueTransactionV2      => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: SponsorFeeTransaction   => activationBarrier(BlockchainFeatures.FeeSponsorship)
      case _                          => Left(GenericError("Unknown transaction must be explicitly activated"))
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

  def checkFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, Unit] = {
    def hasScript: Boolean = tx match {
      case tx: Transaction with Authorized => blockchain.accountScript(tx.sender).isDefined
      case _                               => false
    }
    def feeInUnits: Either[ValidationError, Int] = tx match {
      case _: GenesisTransaction       => Right(0)
      case _: PaymentTransaction       => Right(1)
      case _: IssueTransactionV1       => Right(1000)
      case _: IssueTransactionV2       => Right(1000)
      case _: ReissueTransactionV1     => Right(1000)
      case _: BurnTransaction          => Right(1)
      case _: TransferTransactionV1    => Right(1)
      case tx: MassTransferTransaction => Right(1 + (tx.transfers.size + 1) / 2)
      case _: LeaseTransaction         => Right(1)
      case _: LeaseCancelTransaction   => Right(1)
      case _: ExchangeTransaction      => Right(3)
      case _: CreateAliasTransaction   => Right(1)
      case tx: DataTransaction         => Right(1 + (tx.bytes().length - 1) / 1024)
      case _: SetScriptTransaction     => Right(1)
      case _: TransferTransactionV2    => Right(1)
      case _: SponsorFeeTransaction    => Right(1000)
      case _                           => Left(UnsupportedTransactionType)
    }
    if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs))
      for {
        feeInUnits <- feeInUnits
        txWavesFee <- tx.assetFee._1 match {
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
        _ <- Either.cond(
          txWavesFee >= minimumFee,
          (),
          GenericError(
            s"Fee in ${tx.assetFee._1.fold("WAVES")(_.toString)} for ${tx.builder.classTag} does not exceed minimal value of $minimumFee WAVES: $txWavesFee")
        )
        totalRequiredFee = minimumFee + (if (hasScript) ScriptExtraFee else 0L)
        _ <- Either.cond(
          txWavesFee >= totalRequiredFee,
          (),
          InsufficientFee(s"Scripted account requires $totalRequiredFee fee for this transaction, but given: $txWavesFee")
        )
      } yield ()
    else if (hasScript)
      for {
        feeInUnits <- feeInUnits
        txWavesFee <- tx.assetFee._1 match {
          case None    => Right(tx.assetFee._2)
          case Some(_) => Left(GenericError("Scripted accounts can accept transactions with Waves as fee only"))
        }
        totalRequiredFee = feeInUnits * Sponsorship.FeeUnit + ScriptExtraFee
        _ <- Either.cond(
          txWavesFee >= totalRequiredFee,
          (),
          InsufficientFee(s"Scripted account requires $totalRequiredFee fee for this transaction, but given: $txWavesFee")
        )
      } yield ()
    else Right(())
  }
}
