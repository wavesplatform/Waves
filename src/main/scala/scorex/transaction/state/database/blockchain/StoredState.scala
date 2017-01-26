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
class StoredState(protected val storage: StateStorageI with OrderMatchStorageI,
                  val assetsExtension: AssetsExtendedState,
                  val incrementingTimestampValidator: IncrementingTimestampValidator,
                  val validators: Seq[StateExtension],
                  settings: ChainParameters) extends LagonakiState with ScorexLogging {

  override def included(id: Array[Byte], heightOpt: Option[Int]): Option[Int] = storage.included(id, heightOpt)

  def stateHeight: Int = storage.stateHeight

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = {
    val address = account.address
    storage.getAccountAssets(address).foldLeft(Map.empty[AssetId, (Long, Boolean, Long, IssueTransaction)]) { (result, asset) =>
      val triedAssetId = Base58.decode(asset)
      val balance = balanceByKey(address + asset)

      if (triedAssetId.isSuccess) {
        val assetId = triedAssetId.get
        val maybeIssueTransaction = getIssueTransaction(assetId)
        if (maybeIssueTransaction.isDefined)
          result.updated(assetId, (balance, assetsExtension.isReissuable(assetId), totalAssetQuantity(assetId),
            maybeIssueTransaction.get))
        else result
      } else result
    }
  }

  def rollbackTo(rollbackTo: Int): State = synchronized {
    def deleteNewer(key: Address): Unit = {
      val currentHeight = storage.getLastStates(key).getOrElse(0)
      if (currentHeight > rollbackTo) {
        val changes = storage.removeAccountChanges(key, currentHeight)
        changes.reason.foreach(id => {
          storage.removeTransaction(id)
          storage.getTransaction(id) match {
            case Some(t: AssetIssuance) =>
              assetsExtension.rollbackTo(t.assetId, currentHeight)
            case Some(t: BurnTransaction) =>
              assetsExtension.rollbackTo(t.assetId, currentHeight)
            case _ =>
          }
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
    val trans = block.transactions
    val fees: Map[AssetAcc, (AccState, Reasons)] = block.consensusModule.feesDistribution(block)
      .map(m => m._1 -> (AccState(assetBalance(m._1) + m._2), List(FeesStateChange(m._2))))

    val newBalances: Map[AssetAcc, (AccState, Reasons)] = calcNewBalances(trans, fees, block.timestampField.value < settings.allowTemporaryNegativeUntil)
    newBalances.foreach(nb => require(nb._2._1.balance >= 0))

    applyChanges(newBalances, block.timestampField.value)
    log.trace(s"New state height is ${storage.stateHeight}, hash: $hash, totalBalance: $totalBalance")

    this
  }

  override def balance(account: Account, atHeight: Option[Int] = None): Long =
    assetBalance(AssetAcc(account, None), atHeight)

  def assetBalance(account: AssetAcc, atHeight: Option[Int] = None): Long = {
    balanceByKey(account.key, atHeight)
  }

  override def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int]): Long =
    balance(account, Some(Math.max(1, heightOpt.getOrElse(storage.stateHeight) - confirmations)))

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
                  if (row.lastRowHeight > resAcc.firstKey) loop(row.lastRowHeight, resAcc.tail)
                  else resAcc
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

    val txs = trans.filter(t => isValid(t, height))

    val allowInvalidPaymentTransactionsByTimestamp = txs.nonEmpty && txs.map(_.timestamp).max < settings.allowInvalidPaymentTransactionsByTimestamp
    val validTransactions = if (allowInvalidPaymentTransactionsByTimestamp) {
      txs
    } else {
      val invalidPaymentTransactionsByTimestamp = incrementingTimestampValidator.invalidatePaymentTransactionsByTimestamp(txs)
      excludeTransactions(txs, invalidPaymentTransactionsByTimestamp)
    }

    val allowTransactionsFromFutureByTimestamp = validTransactions.nonEmpty && validTransactions.map(_.timestamp).max < settings.allowTransactionsFromFutureUntil
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
            val changes = if (allowUnissuedAssets) tx.balanceChanges() else tx.balanceChanges().sortBy(_.delta)
            val newState = changes.foldLeft(currentState) { case (iChanges, bc) =>
              //update balances sheet
              def safeSum(first: Long, second: Long): Long = {
                try {
                  Math.addExact(first, second)
                } catch {
                  case e: ArithmeticException =>
                    throw new Error(s"Transaction leads to overflow balance: $first + $second = ${first + second}")
                }
              }

              val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(assetBalance(bc.assetAcc)), List.empty))
              val newBalance = safeSum(currentChange._1.balance, bc.delta)
              if (newBalance >= 0 || tx.timestamp < settings.allowTemporaryNegativeUntil) {
                iChanges.updated(bc.assetAcc, (AccState(newBalance), tx.id +: currentChange._2))
              } else {
                throw new Error(s"Transaction leads to negative state: ${currentChange._1.balance} + ${bc.delta} = ${currentChange._1.balance + bc.delta}")
              }
            }
            (newState, seq :+ tx)
          } catch {
            case NonFatal(e) =>
              (currentState, seq)
          }
      }
      validTxs
    }

    filterValidTransactionsByState(filteredFromFuture)
  }

  private[blockchain] def calcNewBalances(trans: Seq[Transaction], fees: Map[AssetAcc, (AccState, Reasons)], allowTemporaryNegative: Boolean):
  Map[AssetAcc, (AccState, Reasons)] = {
    val newBalances: Map[AssetAcc, (AccState, Reasons)] = trans.foldLeft(fees) { case (changes, tx) =>
      tx.balanceChanges().foldLeft(changes) { case (iChanges, bc) =>
        //update balances sheet
        val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(assetBalance(bc.assetAcc)), List.empty))
        val newBalance = if (currentChange._1.balance == Long.MinValue) Long.MinValue
        else Try(Math.addExact(currentChange._1.balance, bc.delta)).getOrElse(Long.MinValue)

        if (newBalance < 0 && !allowTemporaryNegative) {
          throw new Error(s"Transaction leads to negative balance ($newBalance): ${tx.json}")
        }

        iChanges.updated(bc.assetAcc, (AccState(newBalance), tx +: currentChange._2))
      }
    }
    newBalances
  }

  private[blockchain] def totalAssetQuantity(assetId: AssetId): Long = assetsExtension.getAssetQuantity(assetId)

  private[blockchain] def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)], blockTs: Long = NTP.correctedTime()): Unit = synchronized {
    storage.setStateHeight(storage.stateHeight + 1)
    val h = storage.stateHeight
    changes.foreach { ch =>
      val change = Row(ch._2._1, ch._2._2.map(_.id), storage.getLastStates(ch._1.key).getOrElse(0))
      storage.putAccountChanges(ch._1.key, h, change)
      storage.putLastStates(ch._1.key, h)
      ch._2._2.foreach {
        case tx: Transaction =>
          validators.foreach(_.process(tx, blockTs, h))
        case _ =>
      }
      storage.updateAccountAssets(ch._1.account.address, ch._1.assetId)
    }
  }

  private[blockchain] def filterValidTransactions(trans: Seq[Transaction]):
  Seq[Transaction] = {
    trans.foldLeft((Map.empty[AssetAcc, (AccState, ReasonIds)], Seq.empty[Transaction])) {
      case ((currentState, validTxs), tx) =>
        try {
          val newState = tx.balanceChanges().foldLeft(currentState) { case (iChanges, bc) =>
            //update balances sheet
            val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(assetBalance(bc.assetAcc)), List.empty))
            val newBalance = if (currentChange._1.balance == Long.MinValue) Long.MinValue
            else Try(Math.addExact(currentChange._1.balance, bc.delta)).getOrElse(Long.MinValue)

            if (newBalance < 0 && tx.timestamp >= settings.allowTemporaryNegativeUntil) {
              throw new Error(s"Transaction leads to negative balance ($newBalance): ${tx.json}")
            }

            iChanges.updated(bc.assetAcc, (AccState(newBalance), tx.id +: currentChange._2))
          }
          (newState, validTxs :+ tx)
        } catch {
          case NonFatal(e) =>
            (currentState, validTxs)
        }
    }._2
  }

  private def balanceByKey(key: String, atHeight: Option[Int] = None): Long = {
    storage.getLastStates(key) match {
      case Some(h) if h > 0 =>
        val requiredHeight = atHeight.getOrElse(storage.stateHeight)
        require(requiredHeight >= 0, s"Height should not be negative, $requiredHeight given")

        def loop(hh: Int, min: Long = Long.MaxValue): Long = {
          val rowOpt = storage.getAccountChanges(key, hh)
          require(rowOpt.isDefined, s"accountChanges($key).get($hh) is null. lastStates.get(address)=$h")
          val row = rowOpt.get
          if (hh <= requiredHeight) Math.min(row.state.balance, min)
          else if (row.lastRowHeight == 0) 0L
          else loop(row.lastRowHeight, Math.min(row.state.balance, min))
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

  private[blockchain] def isValid(transaction: Transaction, height: Int): Boolean = {
    validators.forall(_.isValid(transaction, height))
  }


  private def getIssueTransaction(assetId: AssetId): Option[IssueTransaction] =
    storage.getTransactionBytes(assetId).flatMap(b => IssueTransaction.parseBytes(b).toOption)


  //for debugging purposes only
  def totalBalance: Long = storage.lastStatesKeys.map(address => balanceByKey(address)).sum

  //for debugging purposes only
  def toJson(heightOpt: Option[Int] = None): JsObject = {
    val ls = storage.lastStatesKeys.map(add => add -> balanceByKey(add, heightOpt))
      .filter(b => b._2 != 0).sortBy(_._1)
    JsObject(ls.map(a => a._1 -> JsNumber(a._2)).toMap)
  }

  //for debugging purposes only
  def toWavesJson(heightOpt: Int): JsObject = {
    val ls = storage.lastStatesKeys.map(add => add -> balanceAtHeight(add, heightOpt))
      .filter(b => b._1.length == 35 && b._2 != 0).sortBy(_._1).map(b => b._1 -> JsNumber(b._2))
    JsObject(ls)
  }

  //for debugging purposes only
  private def balanceAtHeight(key: String, atHeight: Int): Long = {
    storage.getLastStates(key) match {
      case Some(h) if h > 0 =>

        def loop(hh: Int): Long = {
          val row = storage.getAccountChanges(key, hh).get
          if (hh <= atHeight) row.state.balance
          else if (row.lastRowHeight == 0) 0L
          else loop(row.lastRowHeight)
        }

        loop(h)
      case _ =>
        0L
    }
  }

  //for debugging purposes only
  override def toString: String = toJson().toString()

  //for debugging purposes only
  def hash: Int = {
    (BigInt(FastCryptographicHash(toString.getBytes)) % Int.MaxValue).toInt
  }

}

object StoredState {
  def fromDB(mvStore: MVStore, settings: ChainParameters): StoredState = {
    val storage = new MVStoreStateStorage with MVStoreOrderMatchStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
      if (db.getStoreVersion > 0) db.rollback()
    }
    val extendedState = new AssetsExtendedState(storage)
    val incrementingTimestampValidator = new IncrementingTimestampValidator(settings, storage)
    val validators = Seq(
      extendedState,
      incrementingTimestampValidator,
      new GenesisValidator,
      new OrderMatchStoredState(storage),
      new IncludedValidator(storage, settings),
      new ActivatedValidator(settings)
    )
    new StoredState(storage, extendedState, incrementingTimestampValidator, validators, settings)
  }

}