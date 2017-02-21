package scorex.transaction.state.database.blockchain

import org.h2.mvstore.MVStore
import play.api.libs.json.{JsNumber, JsObject}
import scorex.account.Account
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.settings.ChainParameters
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state._
import scorex.transaction.state.database.state.extension._
import scorex.transaction.state.database.state.storage._
import scorex.utils.{NTP, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal


/**
  * Validation and processing of data with respect to the local storage
  *
  * Use fromDB method of StoredState object to create new instance
  */
class StoredState(protected[blockchain] val storage: StateStorageI with OrderMatchStorageI,
                  val leaseExtendedState: LeaseExtendedState,
                  val assetsExtension: AssetsExtendedState,
                  val incrementingTimestampValidator: IncrementingTimestampValidator,
                  val validators: Seq[Validator],
                  settings: ChainParameters) extends LagonakiState with ScorexLogging {

  override def included(id: Array[Byte]): Option[Int] = storage.included(id, None)

  def stateHeight: Int = storage.stateHeight

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = {
    val address = account.address
    storage.getAccountAssets(address).foldLeft(Map.empty[AssetId, (Long, Boolean, Long, IssueTransaction)]) { (result, asset) =>
      val triedAssetId = Base58.decode(asset)
      val balance = balanceByKey(address + asset, _.balance)

      if (triedAssetId.isSuccess) {
        val assetId = triedAssetId.get
        getIssueTransaction(assetId) match {
          case Some(issueTransaction) =>
            result.updated(assetId, (balance, assetsExtension.isReissuable(assetId), totalAssetQuantity(assetId), issueTransaction))
          case None =>
            result
        }
      } else {
        result
      }
    }
  }

  def rollbackTo(rollbackTo: Int): State = synchronized {
    @tailrec
    def deleteNewer(key: Address): Unit = {
      val currentHeight = storage.getLastStates(key).getOrElse(0)
      if (currentHeight > rollbackTo) {
        val changes = storage.removeAccountChanges(key, currentHeight)
        changes.reason.foreach(id => {
          storage.getTransaction(id) match {
            case Some(t: AssetIssuance) =>
              assetsExtension.rollbackTo(t.assetId, currentHeight)
            case Some(t: BurnTransaction) =>
              assetsExtension.rollbackTo(t.assetId, currentHeight)
            case Some(t: LeaseTransaction) =>
              leaseExtendedState.cancelLease(t)
            case Some(t: LeaseCancelTransaction) =>
              leaseExtendedState.cancelLeaseCancel(t)
            case _ =>
          }
          storage.removeTransaction(id)
        })
        val prevHeight = changes.lastRowHeight
        storage.putLastStates(key, prevHeight)
        deleteNewer(key)
      }
    }

    storage.lastStatesKeys.foreach { key =>
      deleteNewer(key)
    }
    storage.setStateHeight(rollbackTo)
    this
  }

  override def processBlock(block: Block): Try[State] = Try {
    val trans = block.transactionData
    val fees: Map[AssetAcc, (AccState, Reasons)] = Block.feesDistribution(block)
      .map(m => m._1 -> (AccState(assetBalance(m._1) + m._2, effectiveBalance(m._1.account) + m._2), List(FeesStateChange(m._2))))

    val newBalances: Map[AssetAcc, (AccState, Reasons)] =
      calcNewBalances(trans, fees, block.timestampField.value < settings.allowTemporaryNegativeUntil)
    newBalances.foreach(nb => require(nb._2._1.balance >= 0))

    applyChanges(newBalances, block.timestampField.value)
    log.trace(s"New state height is ${storage.stateHeight}, hash: $hash, totalBalance: $totalBalance")

    this
  }

  override def balance(account: Account, atHeight: Option[Int] = None): Long =
    assetBalance(AssetAcc(account, None), atHeight)

  def assetBalance(account: AssetAcc, atHeight: Option[Int] = None): Long = {
    balanceByKey(account.key, _.balance, atHeight)
  }

  private def heightWithConfirmations(heightOpt: Option[Int], confirmations: Int): Int = {
    Math.max(1, heightOpt.getOrElse(storage.stateHeight) - confirmations)
  }

  override def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int]): Long =
    balance(account, Some(heightWithConfirmations(heightOpt, confirmations)))

  override def accountTransactions(account: Account, limit: Int = DefaultLimit): Seq[Transaction] = {
    val accountAssets = storage.getAccountAssets(account.address)
    val keys = account.address :: accountAssets.map(account.address + _).toList

    def getTxSize(m: SortedMap[Int, Set[Transaction]]) = m.foldLeft(0)((size, txs) => size + txs._2.size)

    def getRowTxs(row: Row): Set[Transaction] = row.reason.flatMap(id => storage.getTransaction(id)).toSet

    keys.foldLeft(SortedMap.empty[Int, Set[Transaction]]) { (result, key) =>

      storage.getLastStates(key) match {
        case Some(accHeight) if getTxSize(result) < limit || accHeight > result.firstKey =>
          @tailrec
          def loop(h: Int, acc: SortedMap[Int, Set[Transaction]]): SortedMap[Int, Set[Transaction]] = {
            storage.getAccountChanges(key, h) match {
              case Some(row) =>
                val rowTxs = getRowTxs(row)
                val resAcc = acc + (h -> (rowTxs ++ acc.getOrElse(h, Set.empty[Transaction])))
                if (getTxSize(resAcc) < limit) {
                  loop(row.lastRowHeight, resAcc)
                } else {
                  if (row.lastRowHeight > resAcc.firstKey) {
                    loop(row.lastRowHeight, resAcc.tail)
                  } else {
                    resAcc
                  }
                }
              case _ => acc
            }
          }

          loop(accHeight, result)
        case _ => result
      }

    }.values.flatten.toList.sortWith(_.timestamp > _.timestamp).take(limit)
  }

  /**
    * Returns sequence of valid transactions
    */
  override final def validate(trans: Seq[Transaction], heightOpt: Option[Int] = None, blockTime: Long): Seq[Transaction] = {
    val height = heightOpt.getOrElse(storage.stateHeight)

    val txs = trans.filter(t => validateAgainstState(t, height).isRight)

    val allowInvalidPaymentTransactionsByTimestamp = txs.nonEmpty &&
      txs.map(_.timestamp).max < settings.allowInvalidPaymentTransactionsByTimestamp
    val validTransactions = if (allowInvalidPaymentTransactionsByTimestamp) {
      txs
    } else {
      val invalidPaymentTransactionsByTimestamp = incrementingTimestampValidator.invalidatePaymentTransactionsByTimestamp(txs)
      excludeTransactions(txs, invalidPaymentTransactionsByTimestamp)
    }

    val allowTransactionsFromFutureByTimestamp = validTransactions.nonEmpty &&
      validTransactions.map(_.timestamp).max < settings.allowTransactionsFromFutureUntil
    val filteredFromFuture = if (allowTransactionsFromFutureByTimestamp) {
      validTransactions
    } else {
      filterTransactionsFromFuture(validTransactions, blockTime)
    }

    val allowUnissuedAssets = filteredFromFuture.nonEmpty && txs.map(_.timestamp).max < settings.allowUnissuedAssetsUntil


    def filterValidTransactionsByState(trans: Seq[Transaction]): Seq[Transaction] = {
      val (state, validTxs) = trans.foldLeft((Map.empty[AssetAcc, (AccState, ReasonIds)], Seq.empty[Transaction])) {
        case ((currentState, seq), tx) =>
          try {
            val changes = if (allowUnissuedAssets) {
              tx.balanceChanges()
            } else {
              tx.balanceChanges().sortBy(_.delta)
            }
            def safeSum(first: Long, second: Long): Long = {
              try {
                Math.addExact(first, second)
              } catch {
                case e: ArithmeticException =>
                  throw new Error(s"Transaction leads to overflow balance: $first + $second = ${first + second}")
              }
            }
            val newStateAfterBalanceUpdates = changes.foldLeft(currentState) { case (iChanges, bc) =>
              //update balances sheet

              val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(assetBalance(bc.assetAcc), effectiveBalance(bc.assetAcc.account)), List.empty))
              val newBalance = safeSum(currentChange._1.balance, bc.delta)
              if (tx.timestamp < settings.allowTemporaryNegativeUntil || newBalance >= 0) {
                iChanges.updated(bc.assetAcc, (AccState(newBalance, currentChange._1.effectiveBalance), tx.id +: currentChange._2))
              } else {
                throw new Error(s"Transaction leads to negative state: ${currentChange._1.balance} + ${bc.delta} = ${currentChange._1.balance + bc.delta}")
              }
            }

            val newStateAfterEffectiveBalanceChanges = leaseExtendedState.effectiveBalanceChanges(tx).foldLeft(newStateAfterBalanceUpdates) {     case (iChanges, bc) =>
              //update effective balances sheet
              val currentChange = iChanges.getOrElse(AssetAcc(bc.account, None), (AccState(assetBalance(AssetAcc(bc.account, None)), effectiveBalance(bc.account)), List.empty))
              val newEffectiveBalance = safeSum(currentChange._1.effectiveBalance, bc.amount)
              if (tx.timestamp < settings.allowTemporaryNegativeUntil || newEffectiveBalance >= 0) {
                iChanges.updated(AssetAcc(bc.account, None), (AccState(currentChange._1.balance, newEffectiveBalance), currentChange._2))
              } else {
                throw new Error(s"Transaction leads to negative effective balance: ${currentChange._1.effectiveBalance} + ${bc.amount} = ${currentChange._1.effectiveBalance + bc.amount}")
              }
            }
            (newStateAfterEffectiveBalanceChanges, seq :+ tx)
          } catch {
            case NonFatal(e) =>
              log.debug(e.getMessage)
              (currentState, seq)
          }
      }
      validTxs
    }

    filterValidTransactionsByState(filteredFromFuture)
  }

  private[blockchain] def calcNewBalances(trans: Seq[Transaction],
                                          fees: Map[AssetAcc, (AccState, Reasons)],
                                          allowTemporaryNegative: Boolean):
  Map[AssetAcc, (AccState, Reasons)] = {
    val newBalances: Map[AssetAcc, (AccState, Reasons)] = trans.foldLeft(fees) { case (changes, tx) =>
      val newStateAfterBalanceUpdates = tx.balanceChanges().foldLeft(changes) { case (iChanges, bc) =>
        //update balances sheet
        val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(assetBalance(bc.assetAcc), effectiveBalance(bc.assetAcc.account)), List.empty))
        val newBalance = if (currentChange._1.balance == Long.MinValue) {
          Long.MinValue
        } else {
          Try(Math.addExact(currentChange._1.balance, bc.delta)).getOrElse(Long.MinValue)
        }

        if (allowTemporaryNegative || newBalance >= 0) {
          iChanges.updated(bc.assetAcc, (AccState(newBalance, currentChange._1.effectiveBalance), tx +: currentChange._2))
        } else {
          throw new Error(s"Transaction leads to negative balance ($newBalance): ${tx.json}")
        }
      }

      val newStateAfterEffectiveBalanceChanges = leaseExtendedState.effectiveBalanceChanges(tx).foldLeft(newStateAfterBalanceUpdates) {     case (iChanges, bc) =>
        //update effective balances sheet
        val wavesAcc = AssetAcc(bc.account, None)
        val currentChange = iChanges.getOrElse(wavesAcc, (AccState(assetBalance(AssetAcc(bc.account, None)), effectiveBalance(bc.account)), List.empty))
        val newEffectiveBalance = if (currentChange._1.effectiveBalance == Long.MinValue) {
          Long.MinValue
        } else {
          Try(Math.addExact(currentChange._1.effectiveBalance, bc.amount)).getOrElse(Long.MinValue)
        }
        if (allowTemporaryNegative || newEffectiveBalance >= 0) {
          iChanges.updated(wavesAcc, (AccState(currentChange._1.balance, newEffectiveBalance), currentChange._2))
        } else {
          throw new Error(s"Transaction leads to negative effective balance: ${currentChange._1.effectiveBalance} + ${bc.amount} = ${currentChange._1.effectiveBalance + bc.amount}")
        }
      }
      newStateAfterEffectiveBalanceChanges
    }
    newBalances
  }

  private[blockchain] def totalAssetQuantity(assetId: AssetId): Long = assetsExtension.getAssetQuantity(assetId)

  private[blockchain] def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)],
                                       blockTs: Long = NTP.correctedTime()): Unit = synchronized {
    storage.setStateHeight(storage.stateHeight + 1)
    val h = storage.stateHeight
    changes.foreach { ch =>
      val change = Row(ch._2._1, ch._2._2.map(_.id), storage.getLastStates(ch._1.key).getOrElse(0))
      storage.putAccountChanges(ch._1.key, h, change)
      storage.putLastStates(ch._1.key, h)
      ch._2._2.foreach {
        case tx: Transaction =>
          validators.foreach(_.process(this, tx, blockTs, h))
        case _ =>
      }
      storage.updateAccountAssets(ch._1.account.address, ch._1.assetId)
    }
  }

  private[blockchain] def filterValidTransactions(trans: Seq[Transaction]): Seq[Transaction] = {
    trans.foldLeft((Map.empty[AssetAcc, (AccState, ReasonIds)], Seq.empty[Transaction])) {
      case ((currentState, validTxs), tx) =>
        try {
          val stateAfterBalanceUpdates = tx.balanceChanges().foldLeft(currentState) { case (iChanges, bc) =>
            //update balances sheet
            val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(assetBalance(bc.assetAcc), effectiveBalance(bc.assetAcc.account)), List.empty))

            val newBalance = if (currentChange._1.balance == Long.MinValue) {
              Long.MinValue
            } else {
              Try(Math.addExact(currentChange._1.balance, bc.delta)).getOrElse(Long.MinValue)
            }

            if (tx.timestamp >= settings.allowTemporaryNegativeUntil || newBalance < 0) {
              throw new Error(s"Transaction leads to negative balance ($newBalance): ${tx.json}")
            }

            iChanges.updated(bc.assetAcc, (AccState(newBalance, currentChange._1.effectiveBalance), tx.id +: currentChange._2))
          }

          val stateAfterEffectiveBalanceUpdates = leaseExtendedState.effectiveBalanceChanges(tx).foldLeft(stateAfterBalanceUpdates) { case (iChanges, bc) =>
            val currentChange = iChanges.getOrElse(AssetAcc(bc.account, None), (AccState(assetBalance(AssetAcc(bc.account, None)), effectiveBalance(bc.account)), List.empty))

            val newEffectiveBalance = if (currentChange._1.effectiveBalance == Long.MinValue) {
              Long.MinValue
            } else {
              Try(Math.addExact(currentChange._1.balance, bc.amount)).getOrElse(Long.MinValue)
            }

            if (tx.timestamp >= settings.allowTemporaryNegativeUntil || newEffectiveBalance < 0) {
              throw new Error(s"Transaction leads to negative effective balance ($newEffectiveBalance): ${tx.json}")
            }

            iChanges.updated(AssetAcc(bc.account, None), (AccState(currentChange._1.balance, newEffectiveBalance), currentChange._2))
          }
          (stateAfterEffectiveBalanceUpdates, validTxs :+ tx)
        } catch {
          case NonFatal(e) =>
            (currentState, validTxs)
        }
    }._2
  }

  private def balanceByKey(key: String, calculatedBalance: AccState => Long, atHeight: Option[Int] = None): Long = {
    storage.getLastStates(key) match {
      case Some(h) if h > 0 =>
        val requiredHeight = atHeight.getOrElse(storage.stateHeight)
        require(requiredHeight >= 0, s"Height should not be negative, $requiredHeight given")

        @tailrec
        def loop(hh: Int, min: Long = Long.MaxValue): Long = {
          val rowOpt = storage.getAccountChanges(key, hh)
          require(rowOpt.isDefined, s"accountChanges($key).get($hh) is null. lastStates.get(address)=$h")
          val row = rowOpt.get
          if (hh <= requiredHeight) {
            Math.min(calculatedBalance(row.state), min)
          } else if (row.lastRowHeight == 0) {
            0L
          } else {
            loop(row.lastRowHeight, Math.min(calculatedBalance(row.state), min))
          }
        }

        loop(h)
      case _ =>
        0L
    }
  }


  private val DefaultLimit = 50

  private def excludeTransactions(transactions: Seq[Transaction], exclude: Iterable[Transaction]) =
    transactions.filter(t1 => !exclude.exists(t2 => t2.id sameElements t1.id))


  private def filterTransactionsFromFuture(transactions: Seq[Transaction], blockTime: Long): Seq[Transaction] = {
    transactions.filter {
      tx => (tx.timestamp - blockTime).millis <= SimpleTransactionModule.MaxTimeForUnconfirmed
    }
  }

  def validateAgainstState(transaction: Transaction, height: Int): Either[ValidationError, Transaction] = {
    validators.toStream.map(_.validate(this, transaction,height)).find(_.isLeft) match {
      case Some(Left(e)) => Left(e)
      case _ => Right(transaction)
    }
  }


  private def getIssueTransaction(assetId: AssetId): Option[IssueTransaction] =
    storage.getTransactionBytes(assetId).flatMap(b => IssueTransaction.parseBytes(b).toOption)


  //for debugging purposes only
  def totalBalance: Long = storage.lastStatesKeys.map(address => balanceByKey(address, _.balance)).sum

  //for debugging purposes only
  def toJson(heightOpt: Option[Int] = None): JsObject = {
    val ls = storage.lastStatesKeys.map(add => add -> balanceByKey(add, _.balance, heightOpt))
      .filter(b => b._2 != 0).sortBy(_._1)
    JsObject(ls.map(a => a._1 -> JsNumber(a._2)).toMap)
  }

  //for debugging purposes only
  def toWavesJson(heightOpt: Int): JsObject = {
    val ls = storage.lastStatesKeys.filter(a => a.length == 35).map(add => add -> balanceAtHeight(add, heightOpt))
      .filter(b => b._2 != 0).sortBy(_._1).map(b => b._1 -> JsNumber(b._2))
    JsObject(ls)
  }

  //for debugging purposes only
  private def balanceAtHeight(key: String, atHeight: Int): Long = {
    storage.getLastStates(key) match {
      case Some(h) if h > 0 =>

        @tailrec
        def loop(hh: Int): Long = {
          val row = storage.getAccountChanges(key, hh).get
          if (hh <= atHeight) {
            row.state.balance
          } else if (row.lastRowHeight == 0) {
            0L
          } else {
            loop(row.lastRowHeight)
          }
        }

        loop(h)
      case _ =>
        0L
    }
  }

  def assetDistribution(assetId: Array[Byte]): Map[String, Long] = storage.assetDistribution(assetId)

  //for debugging purposes only
  override def toString: String = toJson().toString()

  //for debugging purposes only
  def hash: Int = {
    (BigInt(FastCryptographicHash(toString.getBytes)) % Int.MaxValue).toInt
  }

  override def effectiveBalance(account: Account, height: Option[Int]): Long = {
    balanceByKey(account.address, _.effectiveBalance, height)
  }

  override def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int]): Long =
    effectiveBalance(account, Some(heightWithConfirmations(heightOpt, confirmations)))
}

object StoredState {
  def fromDB(mvStore: MVStore, settings: ChainParameters): StoredState = {
    val storage = new MVStoreStateStorage with MVStoreOrderMatchStorage with MVStoreAssetsExtendedStateStorage with MVStoreLeaseExtendedStateStorage {
      override val db: MVStore = mvStore
      if (db.getStoreVersion > 0) {
        db.rollback()
      }
    }
    val assetExtendedState = new AssetsExtendedState(storage)
    val leaseExtendedState = new LeaseExtendedState(storage)
    val incrementingTimestampValidator = new IncrementingTimestampValidator(settings.allowInvalidPaymentTransactionsByTimestamp, storage)
    val validators = Seq(
      assetExtendedState,
      incrementingTimestampValidator,
      leaseExtendedState,
      new GenesisValidator,
      new OrderMatchStoredState(storage),
      new IncludedValidator(storage, settings.requirePaymentUniqueId),
      new ActivatedValidator(settings.allowBurnTransactionAfterTimestamp,
        settings.allowLeaseTransactionAfterTimestamp,
        settings.allowExchangeTransactionAfterTimestamp
      )
    )
    new StoredState(storage, leaseExtendedState, assetExtendedState, incrementingTimestampValidator, validators, settings)
  }

}
