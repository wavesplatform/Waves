package scorex.transaction.state.database.blockchain

import org.h2.mvstore.MVStore
import play.api.libs.json.{JsNumber, JsObject}
import scorex.account.{Account, Alias}
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.settings.ChainParameters
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.blockchain.StoredState._
import scorex.transaction.state.database.state._
import scorex.transaction.state.database.state.storage._
import scorex.transaction.{Transaction, _}
import scorex.utils.{NTP, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.reflect.ClassTag
import scala.util.Try


class StoredState(private val storage: StateStorageI with AssetsExtendedStateStorageI with OrderMatchStorageI
  with LeaseExtendedStateStorageI with AliasExtendedStorageI,
                  settings: ChainParameters) extends State with ScorexLogging {

  def applyAssetIssueReissueBurnTransaction(height: Int)(tx: Transaction): Unit = tx match {
    case tx: AssetIssuance =>
      addAsset(tx.assetId, height, tx.id, tx.quantity, tx.reissuable)
    case tx: BurnTransaction =>
      burnAsset(tx.assetId, height, tx.id, -tx.amount)
    case _ =>
  }

  def addAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long, reissuable: Boolean): Unit = {
    val asset = Base58.encode(assetId)
    val transaction = Base58.encode(transactionId)
    val assetAtHeight = s"$asset@$height"
    val assetAtTransaction = s"$asset@$transaction"

    if (storage.getHeights(Base58.encode(assetId)).isEmpty ||
      (reissuable && isReissuable(assetId)) ||
      !reissuable) {
      storage.setReissuable(assetAtTransaction, reissuable)
    } else {
      throw new RuntimeException("Asset is not reissuable")
    }
    storage.addHeight(asset, height)
    storage.addTransaction(assetAtHeight, transaction)
    storage.setQuantity(assetAtTransaction, quantity)
    storage.setReissuable(assetAtTransaction, reissuable)
  }

  def burnAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long): Unit = {
    require(quantity <= 0, "Quantity of burned asset should be negative")

    val asset = Base58.encode(assetId)
    val transaction = Base58.encode(transactionId)
    val assetAtHeight = s"$asset@$height"
    val assetAtTransaction = s"$asset@$transaction"

    storage.addHeight(asset, height)
    storage.addTransaction(assetAtHeight, transaction)
    storage.setQuantity(assetAtTransaction, quantity)
  }

  def assetRollbackTo(assetId: AssetId, height: Int): Unit = {
    val asset = Base58.encode(assetId)

    val heights = storage.getHeights(asset)
    val heightsToRemove = heights.filter(h => h > height)
    storage.setHeight(asset, heights -- heightsToRemove)

    val transactionsToRemove: Seq[String] = heightsToRemove.foldLeft(Seq.empty[String]) { (result, h) =>
      result ++ storage.getTransactions(s"$asset@$h")
    }

    val keysToRemove = transactionsToRemove.map(t => s"$asset@$t")

    keysToRemove.foreach(storage.removeKey)
  }

  def isReissuable(assetId: AssetId): Boolean = {
    val asset = Base58.encode(assetId)
    val heights = storage.getHeights(asset)

    heights.lastOption match {
      case Some(lastHeight) =>
        val transactions = storage.getTransactions(s"$asset@$lastHeight")
        if (transactions.nonEmpty) {
          val transaction = transactions.last
          storage.isReissuable(s"$asset@$transaction")
        } else false
      case None => false
    }
  }

  def applyExchangeTransaction(blockTs: Long)(tx: Transaction): Unit = tx match {
    case om: ExchangeTransaction =>
      def isSaveNeeded(order: Order): Boolean = {
        order.expiration >= blockTs
      }

      def putOrder(order: Order) = {
        if (isSaveNeeded(order)) {
          val orderDay = calcStartDay(order.expiration)
          storage.putSavedDays(orderDay)
          val orderIdStr = Base58.encode(order.id)
          val omIdStr = Base58.encode(om.id)
          val prev = storage.getOrderMatchTxByDay(orderDay, orderIdStr).getOrElse(Array.empty[String])
          if (!prev.contains(omIdStr)) {
            storage.putOrderMatchTxByDay(orderDay, orderIdStr, prev :+ omIdStr)
          }
        }
      }

      def removeObsoleteDays(timestamp: Long): Unit = {
        val ts = calcStartDay(timestamp)
        val daysToRemove: List[Long] = storage.savedDaysKeys.filter(t => t < ts)
        if (daysToRemove.nonEmpty) {
          synchronized {
            storage.removeOrderMatchDays(daysToRemove)
          }
        }
      }

      putOrder(om.buyOrder)
      putOrder(om.sellOrder)
      removeObsoleteDays(blockTs)

    case _ =>
  }

  def findPrevOrderMatchTxs(order: Order): Set[ExchangeTransaction] = {

    def parseTxSeq(a: Array[String]): Set[ExchangeTransaction] =
      for {
        idStr: String <- a.toSet
        idBytes <- Base58.decode(idStr).toOption
        tx <- findTransaction[ExchangeTransaction](idBytes)
      } yield tx

    val orderDay = calcStartDay(order.expiration)
    if (storage.containsSavedDays(orderDay)) {
      parseTxSeq(storage.getOrderMatchTxByDay(calcStartDay(order.expiration), Base58.encode(order.id))
        .getOrElse(Array.empty[String]))
    } else Set.empty[ExchangeTransaction]
  }

  def getLeasedSum(address: AddressString): Long = storage.getLeasedSum(address)

  private def updateLeasedSum(account: Account, update: Long => Long): Unit = {
    val address = account.address
    val newLeasedBalance = update(storage.getLeasedSum(address))
    storage.updateLeasedSum(address, newLeasedBalance)
  }

  def applyLease(tx: LeaseTransaction): Unit = {
    updateLeasedSum(tx.sender, _ + tx.amount)
  }

  def cancelLease(tx: LeaseTransaction): Unit = {
    updateLeasedSum(tx.sender, _ - tx.amount)
  }

  def cancelLeaseCancel(tx: LeaseCancelTransaction): Unit = {
    val leaseTx = storage.getExistedLeaseTx(tx.leaseId)
    applyLease(leaseTx)
  }

  def applyLeaseTransactions(tx: Transaction): Unit = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseTx = storage.getExistedLeaseTx(tx.leaseId)
      cancelLease(leaseTx)
    case tx: LeaseTransaction =>
      applyLease(tx)
    case _ =>
  }

  def registerAlias(tx: Transaction): Unit = tx match {
    case at: CreateAliasTransaction => persistAlias(at.sender, at.alias)
    case _ => ()
  }

  def registerTransactionById(height: Int)(tx: Transaction): Unit = {
    storage.putTransaction(tx, height)
  }

  def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction] = {
    def loop(h: Int, address: AddressString): Option[PaymentTransaction] = {
      storage.getAccountChanges(address, h) match {
        case Some(row) =>
          val accountTransactions = row.reason
            .flatMap(id => findTransaction[PaymentTransaction](id))
            .filter(_.sender.address == address)
          if (accountTransactions.nonEmpty) Some(accountTransactions.maxBy(_.timestamp))
          else loop(row.lastRowHeight, address)
        case _ => None
      }
    }

    storage.getLastStates(account.address) match {
      case Some(height) => loop(height, account.address)
      case None => None
    }
  }

  def included(id: Array[Byte]): Option[Int] = storage.included(id, None)

  def stateHeight: Int = storage.stateHeight

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = {
    val address = account.address
    storage.getAccountAssets(address).foldLeft(Map.empty[AssetId, (Long, Boolean, Long, IssueTransaction)]) { (result, asset) =>
      val triedAssetId = Base58.decode(asset)
      val balance = balanceByKey(address + asset, _.balance, storage.stateHeight)

      if (triedAssetId.isSuccess) {
        val assetId = triedAssetId.get
        findTransaction[IssueTransaction](assetId) match {
          case Some(issueTransaction) =>
            result.updated(assetId, (balance, isReissuable(assetId), totalAssetQuantity(assetId), issueTransaction))
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
    def deleteNewer(key: AddressString): Unit = {
      val currentHeight = storage.getLastStates(key).getOrElse(0)
      if (currentHeight > rollbackTo) {
        val changes = storage.removeAccountChanges(key, currentHeight)
        changes.reason.foreach(id => {
          storage.getTransaction(id) match {
            case Some(t: AssetIssuance) =>
              assetRollbackTo(t.assetId, currentHeight)
            case Some(t: BurnTransaction) =>
              assetRollbackTo(t.assetId, currentHeight)
            case Some(t: LeaseTransaction) =>
              cancelLease(t)
            case Some(t: LeaseCancelTransaction) =>
              cancelLeaseCancel(t)
            case Some(t: CreateAliasTransaction) =>
              storage.removeAlias(t.alias.name)
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

  def processBlock(block: Block): Try[State] = Try {
    val trans = block.transactionData
    val fees: Map[AssetAcc, (AccState, Reasons)] = Block.feesDistribution(block)
      .map(m => m._1 -> (AccState(assetBalance(m._1) + m._2, effectiveBalance(m._1.account) + m._2), List(FeesStateChange(m._2))))

    val newBalances: Map[AssetAcc, (AccState, Reasons)] =
      calcNewBalances(trans, fees, block.timestamp < settings.allowTemporaryNegativeUntil)
    newBalances.foreach(nb => require(nb._2._1.balance >= 0))

    applyChanges(newBalances, block.timestamp)

    this
  }

  private def balance(account: Account, atHeight: Option[Int]): Long =
    assetBalanceAtHeight(AssetAcc(account, None), atHeight)

  def balance(account: Account): Long =
    assetBalanceAtHeight(AssetAcc(account, None), None)

  private def assetBalanceAtHeight(account: AssetAcc, atHeight: Option[Int] = None): Long = {
    balanceByKey(account.key, _.balance, atHeight.getOrElse(storage.stateHeight))
  }

  def assetBalance(account: AssetAcc): Long = assetBalanceAtHeight(account, Some(storage.stateHeight))

  private def heightWithConfirmations(confirmations: Int): Int = {
    Math.max(1, storage.stateHeight - confirmations)
  }

  private def heightWithConfirmations(height : Int, confirmations: Int): Int = {
    Math.max(1, height - confirmations)
  }

  def balanceWithConfirmations(account: Account, confirmations: Int): Long =
    balance(account, Some(heightWithConfirmations(confirmations)))

  def accountTransactions(account: Account, limit: Int): Seq[Transaction] = {
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

  def resolveAlias(a: Alias): Option[Account] = storage
    .addressByAlias(a.name)
    .map(addr => Account.fromBase58String(addr).right.get)

  def getAlias(a: Account): Option[Alias] = storage
    .aliasByAddress(a.address)
    .map(addr => Alias(addr).right.get)

  private def persistAlias(ac: Account, al: Alias): Unit = storage.persistAlias(ac.address, al.name)

  def calcNewBalances(trans: Seq[Transaction], fees: Map[AssetAcc, (AccState, Reasons)], allowTemporaryNegative: Boolean): Map[AssetAcc, (AccState, Reasons)] = {
    val newBalances: Map[AssetAcc, (AccState, Reasons)] = trans.foldLeft(fees) { case (changes, tx) =>
      val bcs = BalanceChangeCalculator.balanceChanges(this)(tx).right.get
      val newStateAfterBalanceUpdates = bcs.foldLeft(changes) { case (iChanges, bc) =>
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

      val ebcs = BalanceChangeCalculator.effectiveBalanceChanges(this)(tx).right.get
      val newStateAfterEffectiveBalanceChanges = ebcs.foldLeft(newStateAfterBalanceUpdates) { case (iChanges, bc) =>
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

  def totalAssetQuantity(assetId: AssetId): Long = {
    val asset = Base58.encode(assetId)
    val heights = storage.getHeights(asset)

    val sortedHeights = heights.toSeq.sorted
    val transactions: Seq[String] = sortedHeights.foldLeft(Seq.empty[String]) { (result, h) =>
      result ++ storage.getTransactions(s"$asset@$h")
    }

    transactions.foldLeft(0L) { (result, transaction) =>
      result + storage.getQuantity(s"$asset@$transaction")
    }
  }

  def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)],
                   blockTs: Long = NTP.correctedTime()): Unit = synchronized {
    storage.setStateHeight(storage.stateHeight + 1)
    val height = storage.stateHeight

    val processors: Seq[(Transaction) => Unit] = Seq(
      applyAssetIssueReissueBurnTransaction(height),
      applyLeaseTransactions,
      registerAlias,
      applyExchangeTransaction(blockTs),
      registerTransactionById(height))

    changes.foreach { ch =>
      val change = Row(ch._2._1, ch._2._2.map(_.id), storage.getLastStates(ch._1.key).getOrElse(0))
      storage.putAccountChanges(ch._1.key, height, change)
      storage.putLastStates(ch._1.key, height)
      ch._2._2.foreach {
        case tx: Transaction =>

          processors.foreach(_.apply(tx))
        case _ =>
      }
      storage.updateAccountAssets(ch._1.account.address, ch._1.assetId)
    }
  }

  private def balanceByKey(key: String, calculatedBalance: AccState => Long, atHeight: Int): Long = {
    storage.getLastStates(key) match {
      case Some(h) if h > 0 =>
        require(atHeight >= 0, s"Height should not be negative, $atHeight given")

        @tailrec
        def loop(hh: Int, min: Long = Long.MaxValue): Long = {
          val rowOpt = storage.getAccountChanges(key, hh)
          require(rowOpt.isDefined, s"accountChanges($key).get($hh) is null. lastStates.get(address)=$h")
          val row = rowOpt.get
          if (hh <= atHeight) {
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

  //for debugging purposes only
  def toJson(heightOpt: Option[Int]): JsObject = {
    val ls = storage.lastStatesKeys.map(add => add -> balanceByKey(add, _.balance, heightOpt.getOrElse(storage.stateHeight)))
      .filter(b => b._2 != 0).sortBy(_._1)
    JsObject(ls.map(a => a._1 -> JsNumber(a._2)).toMap)
  }

  def wavesDistributionAtHeight(height: Int): JsObject = {

    def balanceAtHeight(key: String): Long = {
      storage.getLastStates(key) match {
        case Some(h) if h > 0 =>

          @tailrec
          def loop(hh: Int): Long = {
            val row = storage.getAccountChanges(key, hh).get
            if (hh <= height) {
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

    val ls = storage.lastStatesKeys.filter(a => a.length == 35).map(add => add -> balanceAtHeight(add))
      .filter(b => b._2 != 0).sortBy(_._1).map(b => b._1 -> JsNumber(b._2))
    JsObject(ls)
  }

  def assetDistribution(assetId: Array[Byte]): Map[String, Long] = storage.assetDistribution(assetId)

  def effectiveBalance(account: Account): Long = balanceByKey(account.address, _.effectiveBalance, storage.stateHeight)

  def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, height: Int): Long =
    balanceByKey(account.address, _.effectiveBalance, Math.max(1, height - confirmations))

  def findTransaction[T <: Transaction](id: Array[Byte])(implicit ct: ClassTag[T]): Option[T] = {
    storage.getTransaction(id) match {
      case Some(tx) if ct.runtimeClass.isAssignableFrom(tx.getClass) => Some(tx.asInstanceOf[T])
      case _ => None
    }
  }
}

object StoredState {


  implicit class SeqEitherHelper[L, R](eis: Seq[Either[L, R]]) {
    def segregate(): (Seq[L], Seq[R]) = (eis.filter(_.isLeft).map(_.left.get),
      eis.filter(_.isRight).map(_.right.get))
  }

  def fromDB(mvStore: MVStore, settings: ChainParameters): State = {
    val storage = new MVStoreStateStorage
      with MVStoreOrderMatchStorage
      with MVStoreAssetsExtendedStateStorage
      with MVStoreLeaseExtendedStateStorage
      with MVStoreAliasExtendedStorage {
      override val db: MVStore = mvStore
      if (db.getStoreVersion > 0) {
        db.rollback()
      }
    }

    new StoredState(storage, settings)
  }

  def calcStartDay(t: Long): Long = {
    val ts = t / 1000
    ts - ts % (24 * 60 * 60)
  }
}
