package scorex.transaction.state.database.blockchain

import scorex.account.{Account, Alias}
import scorex.settings.ChainParameters
import scorex.transaction.ValidationError.{AliasNotExists, TransactionValidationError}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.{AccState, ReasonIds}
import scorex.transaction.state.database.state.extension.ExchangeTransactionValidator

import scala.util.{Left, Right, Try}
import scala.concurrent.duration._
import scala.util.control.NonFatal

trait Validator {
  def validate(trans: Seq[Transaction], heightOpt: Option[Int] = None, blockTime: Long): (Seq[ValidationError], Seq[Transaction])
}

class ValidatorImpl(s: State, settings: ChainParameters) extends Validator {

  import Validator._

  private def validAgainstStateOneByOne(height: Int, txs: Seq[Transaction]): Seq[Either[ValidationError, Transaction]]
  = txs.map(t => validateAgainstState(t, height))

  private def validateAssetIssueReissueBurnTransactions(tx: Transaction): Either[StateValidationError, Transaction] = {
    def isIssuerAddress(assetId: Array[Byte], tx: SignedTransaction): Either[StateValidationError, SignedTransaction] = {
      s.findTransaction[Transaction](assetId) match {
        case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
        case Some(it: IssueTransaction) =>
          if (it.sender.address == tx.sender.address) Right(tx)
          else Left(TransactionValidationError(tx, "Asset was issued by other address"))
        case _ => Left(TransactionValidationError(tx, "Referenced transaction is not IssueTransaction"))
      }
    }

    tx match {
      case tx: ReissueTransaction =>
        isIssuerAddress(tx.assetId, tx).flatMap(t =>
          if (s.isReissuable(tx.assetId)) Right(t) else Left(TransactionValidationError(tx, "Asset is not reissuable")))
      case tx: BurnTransaction =>
        isIssuerAddress(tx.assetId, tx)
      case _ => Right(tx)
    }
  }

  private def validateLeaseTransactions(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseOpt = s.findTransaction[LeaseTransaction](tx.leaseId)
      leaseOpt match {
        case Some(leaseTx) if leaseTx.sender.publicKey.sameElements(leaseTx.sender.publicKey) => Right(tx)
        case Some(leaseTx) => Left(TransactionValidationError(tx, s"LeaseTransaction was leased by other sender"))
        case None => Left(TransactionValidationError(tx, s"Related LeaseTransaction not found"))
      }
    case tx: LeaseTransaction =>
      if (s.balance(tx.sender) - tx.fee - s.getLeasedSum(tx.sender.address) >= tx.amount) {
        Right(tx)
      } else {
        Left(TransactionValidationError(tx, s"Not enough effective balance to lease"))
      }
    case _ => Right(tx)
  }

  private def validateExchangeTransaction(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case om: ExchangeTransaction => ExchangeTransactionValidator.isValid(om, s.findPrevOrderMatchTxs(om))
    case _ => Right(tx)
  }

  private def genesisTransactionHeightMustBeZero(height: Int)(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case gtx: GenesisTransaction if height != 0 => Left(TransactionValidationError(tx, "GenesisTranaction cannot appear in non-initial block"))
    case _ => Right(tx)
  }

  private def disallowLeaseToSelfAlias(tx: Transaction): Either[StateValidationError, Transaction] = {
    tx match {
      case ltx: LeaseTransaction =>
        ltx.recipient match {
          case a: Alias => s.resolveAlias(a) match {
            case Some(acc) if ltx.sender.address == acc.address => Left(TransactionValidationError(tx, "Cannot lease to own alias"))
            case _ => Right(tx)
          }
          case _ => Right(tx)
        }
      case _ => Right(tx)
    }
  }

  private def disallowDuplicateIds(requirePaymentUniqueId: Long)(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case tx: PaymentTransaction if tx.timestamp < requirePaymentUniqueId => Right(tx)
    case tx: Transaction => if (s.included(tx.id).isEmpty) Right(tx)
    else Left(TransactionValidationError(tx, "(except for some cases of PaymentTransaction) cannot be duplicated"))
  }

  private def disallowBeforeActivationTime(s: ChainParameters)(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case tx: BurnTransaction if tx.timestamp <= s.allowBurnTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=${s.allowBurnTransactionAfterTimestamp}"))
    case tx: LeaseTransaction if tx.timestamp <= s.allowLeaseTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=${s.allowLeaseTransactionAfterTimestamp}"))
    case tx: LeaseCancelTransaction if tx.timestamp <= s.allowLeaseTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=${s.allowLeaseTransactionAfterTimestamp}"))
    case tx: ExchangeTransaction if tx.timestamp <= s.allowExchangeTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=${s.allowExchangeTransactionAfterTimestamp}"))
    case tx: CreateAliasTransaction if tx.timestamp <= s.allowCreateAliasTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=${s.allowCreateAliasTransactionAfterTimestamp}"))
    case _: BurnTransaction => Right(tx)
    case _: PaymentTransaction => Right(tx)
    case _: GenesisTransaction => Right(tx)
    case _: TransferTransaction => Right(tx)
    case _: IssueTransaction => Right(tx)
    case _: ReissueTransaction => Right(tx)
    case _: ExchangeTransaction => Right(tx)
    case _: LeaseTransaction => Right(tx)
    case _: LeaseCancelTransaction => Right(tx)
    case _: CreateAliasTransaction => Right(tx)
    case x => Left(TransactionValidationError(x, "Unknown transaction must be explicitly registered within ActivatedValidator"))
  }

  private def incrementingTimestamp(allowInvalidPaymentTransactionsByTimestamp: Long)(transaction: Transaction): Either[StateValidationError, Transaction] = {

    def isTimestampCorrect(tx: PaymentTransaction): Boolean = {
      s.lastAccountPaymentTransaction(tx.sender) match {
        case Some(lastTransaction) => lastTransaction.timestamp < tx.timestamp
        case None => true
      }
    }

    transaction match {
      case tx: PaymentTransaction =>
        val isCorrect = tx.timestamp < allowInvalidPaymentTransactionsByTimestamp || isTimestampCorrect(tx)
        if (isCorrect) Right(tx)
        else Left(TransactionValidationError(tx, s" is earlier than previous transaction after time=$allowInvalidPaymentTransactionsByTimestamp"))
      case _ => Right(transaction)
    }
  }

  private def addressAliasExists(tx: Transaction): Either[StateValidationError, Transaction] = {
    val maybeAlias = tx match {
      case ltx: LeaseTransaction => ltx.recipient match {
        case a: Account => None
        case a: Alias => Some(a)
      }
      case ttx: TransferTransaction => ttx.recipient match {
        case a: Account => None
        case a: Alias => Some(a)
      }
      case _ => None
    }

    maybeAlias match {
      case None => Right(tx)
      case Some(al) => s.resolveAlias(al) match {
        case Some(add) => Right(tx)
        case None => Left(AliasNotExists(al))
      }
    }
  }

  private def validateAgainstState(transaction: Transaction, height: Int): Either[ValidationError, Transaction] = {
    val validators: Seq[(Transaction) => Either[StateValidationError, Transaction]] = Seq(
      validateAssetIssueReissueBurnTransactions,
      validateLeaseTransactions,
      validateExchangeTransaction,
      genesisTransactionHeightMustBeZero(height),
      disallowLeaseToSelfAlias,
      disallowDuplicateIds(settings.requirePaymentUniqueId),
      disallowBeforeActivationTime(settings),
      incrementingTimestamp(settings.allowInvalidPaymentTransactionsByTimestamp),
      addressAliasExists)

    validators.toStream.map(_.apply(transaction)).find(_.isLeft) match {
      case Some(Left(e)) => Left(e)
      case _ => Right(transaction)
    }
  }

  private def filterIfPaymentTransactionWithGreaterTimesatampAlreadyPresent(txs: Seq[Transaction]): Seq[Either[ValidationError, Transaction]] = {
    val allowInvalidPaymentTransactionsByTimestamp = txs.nonEmpty && txs.map(_.timestamp).max < settings.allowInvalidPaymentTransactionsByTimestamp
    if (allowInvalidPaymentTransactionsByTimestamp) {
      txs.map(Right(_))
    } else {
      val invalidPaymentTransactionsByTimestamp = invalidatePaymentTransactionsByTimestamp(txs)
      txs.map(t1 => if (!invalidPaymentTransactionsByTimestamp.exists(t2 => t2.id sameElements t1.id))
        Right(t1)
      else Left(TransactionValidationError(t1, s"is invalid due to one of previous transactions in the sequence is PaymentTransaction with a greater timestamp")))
    }
  }

  private def invalidatePaymentTransactionsByTimestamp(transactions: Seq[Transaction]): Seq[Transaction] = {
    val paymentTransactions = transactions.filter(_.isInstanceOf[PaymentTransaction])
      .map(_.asInstanceOf[PaymentTransaction])

    val initialSelection: Map[String, (List[Transaction], Long)] = Map(paymentTransactions.map { payment =>
      val address = payment.sender.address
      val stateTimestamp = s.lastAccountPaymentTransaction(payment.sender) match {
        case Some(lastTransaction) => lastTransaction.timestamp
        case _ => 0
      }
      address -> (List[Transaction](), stateTimestamp)
    }: _*)

    val orderedTransaction = paymentTransactions.sortBy(_.timestamp)
    val selection: Map[String, (List[Transaction], Long)] = orderedTransaction.foldLeft(initialSelection) { (s, t) =>
      val address = t.sender.address
      val tuple = s(address)
      if (t.timestamp > tuple._2) {
        s.updated(address, (tuple._1, t.timestamp))
      } else {
        s.updated(address, (tuple._1 :+ t, tuple._2))
      }
    }

    selection.foldLeft(List[Transaction]()) { (l, s) => l ++ s._2._1 }
  }

  private def filterTransactionsFromFuture(trans: Seq[Transaction], blockTime: Long): Seq[Either[ValidationError, Transaction]] = {
    val allowTransactionsFromFutureByTimestamp = trans.nonEmpty && trans.map(_.timestamp).max < settings.allowTransactionsFromFutureUntil
    if (allowTransactionsFromFutureByTimestamp) {
      trans.map(Right(_))
    } else {
      trans.map {
        tx =>
          if ((tx.timestamp - blockTime).millis <= SimpleTransactionModule.MaxTimeTransactionOverBlockDiff)
            Right(tx)
          else Left(TransactionValidationError(tx, s"Transaction is from far future. BlockTime: $blockTime"))
      }
    }
  }

  private def validateExchangeTxs(txs: Seq[Transaction], height: Int): Seq[Either[ValidationError, Transaction]] = {

    txs.foldLeft(Seq.empty[Either[ValidationError, Transaction]]) {
      case (seq, tx) => validateWithBlockTxs(tx, seq.filter(_.isRight).map(_.right.get)) match {
        case Left(err) => Left(err) +: seq
        case Right(t) => Right(t) +: seq
      }
    }.reverse
  }

  private def validateWithBlockTxs(tx: Transaction, blockTxs: Seq[Transaction]): Either[StateValidationError, Transaction] = tx match {
    case om: ExchangeTransaction =>
      val thisExchanges: Set[ExchangeTransaction] = blockTxs.collect {
        case a: ExchangeTransaction if a != tx && (a.buyOrder == om.buyOrder || a.sellOrder == om.sellOrder) => a
      }.toSet

      ExchangeTransactionValidator.isValid(om, s.findPrevOrderMatchTxs(om) ++ thisExchanges)
    case _ => Right(tx)
  }

  private def filterByBalanceApplicationErrors(allowUnissuedAssets: Boolean, trans: Seq[Transaction]): Seq[Either[ValidationError, Transaction]] = {
    val (_, validatedTxs) = trans.foldLeft((Map.empty[AssetAcc, (AccState, ReasonIds)], Seq.empty[Either[ValidationError, Transaction]])) {
      case ((currentState, seq), tx) =>
        try {
          val changes0 = BalanceChangeCalculator.balanceChanges(s)(tx).right.get
          val changes = if (allowUnissuedAssets) {
            changes0
          } else {
            changes0.sortBy(_.delta)
          }

          val newStateAfterBalanceUpdates = changes.foldLeft(currentState) { case (iChanges, bc) =>
            //update balances sheet

            val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(s.assetBalance(bc.assetAcc), s.effectiveBalance(bc.assetAcc.account)), List.empty))
            val newBalance = safeSum(currentChange._1.balance, bc.delta).get
            if (tx.timestamp < settings.allowTemporaryNegativeUntil || newBalance >= 0) {
              iChanges.updated(bc.assetAcc, (AccState(newBalance, currentChange._1.effectiveBalance), tx.id +: currentChange._2))
            } else {
              throw new Error(s"Transaction leads to negative state: ${currentChange._1.balance} + ${bc.delta} = ${currentChange._1.balance + bc.delta}")
            }
          }

          val ebc = BalanceChangeCalculator.effectiveBalanceChanges(s)(tx).right.get
          val newStateAfterEffectiveBalanceChanges = ebc.foldLeft(newStateAfterBalanceUpdates) { case (iChanges, bc) =>
            //update effective balances sheet
            val currentChange = iChanges.getOrElse(AssetAcc(bc.account, None), (AccState(s.assetBalance(AssetAcc(bc.account, None)), s.effectiveBalance(bc.account)), List.empty))
            val newEffectiveBalance = safeSum(currentChange._1.effectiveBalance, bc.amount).get
            if (tx.timestamp < settings.allowTemporaryNegativeUntil || newEffectiveBalance >= 0) {
              iChanges.updated(AssetAcc(bc.account, None), (AccState(currentChange._1.balance, newEffectiveBalance), currentChange._2))
            } else {
              throw new Error(s"Transaction leads to negative effective balance: ${currentChange._1.effectiveBalance} + ${bc.amount} = ${currentChange._1.effectiveBalance + bc.amount}")
            }
          }
          (newStateAfterEffectiveBalanceChanges, seq :+ Right(tx))
        } catch {
          case NonFatal(e) =>
            //            log.debug(e.getMessage)
            (currentState, seq :+ Left(TransactionValidationError(tx, e.getMessage)))
        }
    }
    validatedTxs
  }

  override final def validate(trans: Seq[Transaction], heightOpt: Option[Int], blockTime: Long): (Seq[ValidationError], Seq[Transaction]) = {
    val height = heightOpt.getOrElse(s.stateHeight)
    val (err0, validOneByOne) = validAgainstStateOneByOne(height, trans).segregate()
    val (err1, validAgainstConsecutivePayments) = filterIfPaymentTransactionWithGreaterTimesatampAlreadyPresent(validOneByOne).segregate()
    val (err2, filteredFarFuture) = filterTransactionsFromFuture(validAgainstConsecutivePayments, blockTime).segregate()
    val allowUnissuedAssets = filteredFarFuture.nonEmpty && validOneByOne.map(_.timestamp).max < settings.allowUnissuedAssetsUntil
    val (err3, filteredOvermatch) = validateExchangeTxs(filteredFarFuture, height).segregate()
    val (err4, result) = filterByBalanceApplicationErrors(allowUnissuedAssets, filteredOvermatch).segregate()
    (err0 ++ err1 ++ err2 ++ err3 ++ err4, result)
  }
}

object Validator {

  implicit class ValidatorExt(v: Validator) {

    def validate[T <: Transaction](tx: T, blockTime: Long): Either[ValidationError, T] = v.validate(Seq(tx), None, blockTime) match {
      case (_, Seq(t)) => Right(t.asInstanceOf[T])
      case (Seq(err), _) => Left(err)
    }

    // utility calls from test only

    def isValid(tx: Transaction, blockTime: Long): Boolean = validate(tx, blockTime).isRight

    def isValid(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Boolean = v.validate(txs, height, blockTime)._2.size == txs.size

  }

  implicit class SeqEitherHelper[L, R](eis: Seq[Either[L, R]]) {
    def segregate(): (Seq[L], Seq[R]) = (eis.filter(_.isLeft).map(_.left.get),
      eis.filter(_.isRight).map(_.right.get))
  }

  def safeSum(first: Long, second: Long): Try[Long] = Try {
    Math.addExact(first, second)
  }

}
