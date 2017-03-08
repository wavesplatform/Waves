package scorex.transaction.state.database.blockchain

import org.h2.mvstore.MVStore
import play.api.libs.json.{JsNumber, JsObject}
import scorex.account.{Account, Alias}
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.settings.ChainParameters
import scorex.transaction.ValidationError.{AliasNotExists, TransactionValidationError}
import scorex.transaction.{Transaction, _}
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state._
import scorex.transaction.state.database.state.extension._
import scorex.transaction.state.database.state.storage._
import scorex.utils.{NTP, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Left, Right, Try}
import scorex.transaction.state.database.blockchain.StoredState._

class StoredState(protected[blockchain] val storage: StateStorageI with AssetsExtendedStateStorageI with OrderMatchStorageI with LeaseExtendedStateStorageI with AliasExtendedStorageI,
                  settings: ChainParameters) extends State with ScorexLogging {


  val assetsExtension = new AssetsExtendedState(storage)

  def exchangeTransactionValidatorF(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case om: ExchangeTransaction => ExchangeTransactionValidator.isValid(om, findPrevOrderMatchTxs(om))
    case _ => Right(tx)
  }

  def exchangeTransactionProcessor(blockTs: Long)(tx: Transaction): Unit = tx match {
    case om: ExchangeTransaction => putOrderMatch(om, blockTs)
    case _ =>
  }

  private def putOrderMatch(om: ExchangeTransaction, blockTs: Long): Unit = {
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
  }

  private def calcStartDay(t: Long): Long = {
    val ts = t / 1000
    ts - ts % (24 * 60 * 60)
  }


  private def parseTxSeq(a: Array[String]): Set[ExchangeTransaction] = {
    a.toSet.flatMap { s: String => Base58.decode(s).toOption }.flatMap { id =>
      storage.getTransactionBytes(id).flatMap(b => ExchangeTransaction.parseBytes(b).toOption)
    }
  }

  private def findPrevOrderMatchTxs(om: ExchangeTransaction): Set[ExchangeTransaction] = {
    findPrevOrderMatchTxs(om.buyOrder) ++ findPrevOrderMatchTxs(om.sellOrder)
  }

  def findPrevOrderMatchTxs(order: Order): Set[ExchangeTransaction] = {
    val orderDay = calcStartDay(order.expiration)
    if (storage.containsSavedDays(orderDay)) {
      parseTxSeq(storage.getOrderMatchTxByDay(calcStartDay(order.expiration), Base58.encode(order.id))
        .getOrElse(Array.empty[String]))
    } else Set.empty[ExchangeTransaction]
  }

  def validateWithBlockTxs(tx: Transaction, blockTxs: Seq[Transaction]): Either[StateValidationError, Transaction] = tx match {
    case om: ExchangeTransaction =>
      val thisExchanges: Set[ExchangeTransaction] = blockTxs.collect {
        case a: ExchangeTransaction if a != tx && (a.buyOrder == om.buyOrder || a.sellOrder == om.sellOrder) => a
      }.toSet

      ExchangeTransactionValidator.isValid(om, findPrevOrderMatchTxs(om) ++ thisExchanges)
    case _ => Right(tx)
  }


  def getLeasedSum(address: AddressString): Long = storage.getLeasedSum(address)

  def leaseTransactionValidatorF(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseOpt = storage.getLeaseTx(tx.leaseId)
      leaseOpt match {
        case Some(leaseTx) if leaseTx.sender.publicKey.sameElements(leaseTx.sender.publicKey) => Right(tx)
        case Some(leaseTx) => Left(TransactionValidationError(tx, s"LeaseTransaction was leased by other sender"))
        case None => Left(TransactionValidationError(tx, s"Related LeaseTransaction not found"))
      }
    case tx: LeaseTransaction =>
      if (balance(tx.sender) - tx.fee - storage.getLeasedSum(tx.sender.address) >= tx.amount) {
        Right(tx)
      } else {
        Left(TransactionValidationError(tx, s"Not enough effective balance to lease"))
      }
    case _ => Right(tx)
  }

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

  def leaseProcessor(tx: Transaction): Unit = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseTx = storage.getExistedLeaseTx(tx.leaseId)
      cancelLease(leaseTx)
    case tx: LeaseTransaction =>
      applyLease(tx)
    case _ =>
  }


  def addressAliasValidatorF(tx: Transaction): Either[StateValidationError, Transaction] = {

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
      case Some(al) => storage.addressByAlias(al.name) match {
        case Some(add) => Right(tx)
        case None => Left(AliasNotExists(al))
      }
    }
  }

  def addressAliasProcessorF(tx: Transaction): Unit = tx match {
    case at: CreateAliasTransaction => persistAlias(at.sender, at.alias)
    case _ => ()
  }

  def leaseToSelfValidatorF(tx: Transaction): Either[StateValidationError, Transaction] = {

    tx match {
      case ltx: LeaseTransaction =>
        ltx.recipient match {
          case a: Alias => resolveAlias(a) match {
            case Some(acc) if ltx.sender.address == acc.address => Left(TransactionValidationError(tx, "Cannot lease to own alias"))
            case _ => Right(tx)
          }
          case _ => Right(tx)
        }
      case _ => Right(tx)
    }
  }

  def genesisValidatorF(height: Int)(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case gtx: GenesisTransaction if height != 0 => Left(TransactionValidationError(tx, "GenesisTranaction cannot appear in non-initial block"))
    case _ => Right(tx)
  }

  def includedValidatorF(requirePaymentUniqueId: Long)(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
    case tx: PaymentTransaction if tx.timestamp < requirePaymentUniqueId => Right(tx)
    case tx: Transaction => if (included(tx.id).isEmpty) Right(tx)
    else Left(TransactionValidationError(tx, "(except for some cases of PaymentTransaction) cannot be duplicated"))
  }

  def includedProcessorF(height: Int)(tx: Transaction): Unit = {
    storage.putTransaction(tx, height)
  }

  def incrementingTimestampValidatorF(allowInvalidPaymentTransactionsByTimestamp: Long)(transaction: Transaction): Either[StateValidationError, Transaction] = {

    def isTimestampCorrect(tx: PaymentTransaction): Boolean = {
      lastAccountPaymentTransaction(tx.sender) match {
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

  def invalidatePaymentTransactionsByTimestamp(transactions: Seq[Transaction]): Seq[Transaction] = {
    val paymentTransactions = transactions.filter(_.isInstanceOf[PaymentTransaction])
      .map(_.asInstanceOf[PaymentTransaction])

    val initialSelection: Map[String, (List[Transaction], Long)] = Map(paymentTransactions.map { payment =>
      val address = payment.sender.address
      val stateTimestamp = lastAccountPaymentTransaction(payment.sender) match {
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

  def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction] = {
    def loop(h: Int, address: AddressString): Option[PaymentTransaction] = {
      storage.getAccountChanges(address, h) match {
        case Some(row) =>
          val accountTransactions = row.reason.flatMap(id => storage.getTransaction(id))
            .filter(_.isInstanceOf[PaymentTransaction])
            .map(_.asInstanceOf[PaymentTransaction])
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

  def activatedValidatorF(s: ChainParameters)(tx: Transaction): Either[StateValidationError, Transaction] = tx match {
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

  val activatedValidatorAdapter = (s: StoredState, t: Transaction, h: Int) => activatedValidatorF(settings)(t)
  val genesisValidatorAdapter = (s: StoredState, t: Transaction, h: Int) => genesisValidatorF(h)(t)
  val includedValidatorAdapter = (s: StoredState, t: Transaction, h: Int) => includedValidatorF(settings.requirePaymentUniqueId)(t)
  val incrementingTimestampValidatorAdapter = (s: StoredState, t: Transaction, i: Int) => incrementingTimestampValidatorF(settings.allowInvalidPaymentTransactionsByTimestamp)(t)
  val leaseToSelfValidatorAdapter = (s: StoredState, t: Transaction, h: Int) => leaseToSelfValidatorF(t)
  val addressAliasValidatorAdapter = (s: StoredState, t: Transaction, h: Int) => addressAliasValidatorF(t)
  val leaseValidatorAdapter = (s: StoredState, t: Transaction, h: Int) => leaseTransactionValidatorF(t)
  val exchangeTransactionValidatorAdapter = (s: StoredState, t: Transaction, h: Int) => exchangeTransactionValidatorF(t)

  val validators: Seq[(StoredState, Transaction, Int) => Either[StateValidationError, Transaction]] = Seq(
    assetsExtension).map(v => v.validate _) ++ Seq(incrementingTimestampValidatorAdapter,
    leaseValidatorAdapter,
    genesisValidatorAdapter,
    addressAliasValidatorAdapter,
    leaseToSelfValidatorAdapter,
    includedValidatorAdapter,
    exchangeTransactionValidatorAdapter,
    activatedValidatorAdapter)


  val includedProcessorAdapter = (s: StoredState, t: Transaction, blockTs: Long, height: Int) => includedProcessorF(height)(t)
  val addressAliasProcessorAdapter = (s: StoredState, t: Transaction, blockTs: Long, height: Int) => addressAliasProcessorF(t)
  val leaseProcessorAdapter = (s: StoredState, t: Transaction, blockTs: Long, height: Int) => leaseProcessor(t)
  val exchangeTransactionProcessorAdapter = (s: StoredState, t: Transaction, blockTs: Long, height: Int) => exchangeTransactionProcessor(blockTs)(t)

  val processors: Seq[(StoredState, Transaction, Long, Int) => Unit] = Seq(
    assetsExtension).map(p => p.process _) ++
    Seq(leaseProcessorAdapter, addressAliasProcessorAdapter, exchangeTransactionProcessorAdapter, includedProcessorAdapter)

  override def included(id: Array[Byte]): Option[Int] = storage.included(id, None)

  def stateHeight: Int = storage.stateHeight

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = {
    val address = account.address
    storage.getAccountAssets(address).foldLeft(Map.empty[AssetId, (Long, Boolean, Long, IssueTransaction)]) { (result, asset) =>
      val triedAssetId = Base58.decode(asset)
      val balance = balanceByKey(address + asset, _.balance, storage.stateHeight)

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
    def deleteNewer(key: AddressString): Unit = {
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

  private def balance(account: Account, atHeight: Option[Int]): Long =
    assetBalanceAtHeight(AssetAcc(account, None), atHeight)

  override def balance(account: Account): Long =
    assetBalanceAtHeight(AssetAcc(account, None), None)

  private def assetBalanceAtHeight(account: AssetAcc, atHeight: Option[Int] = None): Long = {
    balanceByKey(account.key, _.balance, atHeight.getOrElse(storage.stateHeight))
  }

  override def assetBalance(account: AssetAcc): Long = assetBalanceAtHeight(account, Some(storage.stateHeight))

  private def heightWithConfirmations(heightOpt: Option[Int], confirmations: Int): Int = {
    Math.max(1, heightOpt.getOrElse(storage.stateHeight) - confirmations)
  }

  override def balanceWithConfirmations(account: Account, confirmations: Int): Long =
    balance(account, Some(heightWithConfirmations(None, confirmations)))

  override def accountTransactions(account: Account, limit: Int): Seq[Transaction] = {
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

  private def validAgainstStateOneByOne(height: Int, txs: Seq[Transaction]): Seq[Either[ValidationError, Transaction]] = txs.map(t => validateAgainstState(t, height))

  def validateExchangeTxs(txs: Seq[Transaction], height: Int): Seq[Either[ValidationError, Transaction]] = {

    txs.foldLeft(Seq.empty[Either[ValidationError, Transaction]]) {
      case (seq, tx) => validateWithBlockTxs(tx, seq.filter(_.isRight).map(_.right.get)) match {
        case Left(err) => Left(err) +: seq
        case Right(t) => Right(t) +: seq
      }
    }.reverse
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

  private def safeSum(first: Long, second: Long): Try[Long] = Try {
    Math.addExact(first, second)
  }

  def filterByBalanceApplicationErrors(allowUnissuedAssets: Boolean, trans: Seq[Transaction]): Seq[Either[ValidationError, Transaction]] = {
    val (_, validatedTxs) = trans.foldLeft((Map.empty[AssetAcc, (AccState, ReasonIds)], Seq.empty[Either[ValidationError, Transaction]])) {
      case ((currentState, seq), tx) =>
        try {
          val changes0 = BalanceChangeCalculator.balanceChanges(this)(tx).right.get
          val changes = if (allowUnissuedAssets) {
            changes0
          } else {
            changes0.sortBy(_.delta)
          }

          val newStateAfterBalanceUpdates = changes.foldLeft(currentState) { case (iChanges, bc) =>
            //update balances sheet

            val currentChange = iChanges.getOrElse(bc.assetAcc, (AccState(assetBalance(bc.assetAcc), effectiveBalance(bc.assetAcc.account)), List.empty))
            val newBalance = safeSum(currentChange._1.balance, bc.delta).get
            if (tx.timestamp < settings.allowTemporaryNegativeUntil || newBalance >= 0) {
              iChanges.updated(bc.assetAcc, (AccState(newBalance, currentChange._1.effectiveBalance), tx.id +: currentChange._2))
            } else {
              throw new Error(s"Transaction leads to negative state: ${currentChange._1.balance} + ${bc.delta} = ${currentChange._1.balance + bc.delta}")
            }
          }

          val ebc = BalanceChangeCalculator.effectiveBalanceChanges(this)(storage)(tx).right.get
          val newStateAfterEffectiveBalanceChanges = ebc.foldLeft(newStateAfterBalanceUpdates) { case (iChanges, bc) =>
            //update effective balances sheet
            val currentChange = iChanges.getOrElse(AssetAcc(bc.account, None), (AccState(assetBalance(AssetAcc(bc.account, None)), effectiveBalance(bc.account)), List.empty))
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
            log.debug(e.getMessage)
            (currentState, seq :+ Left(TransactionValidationError(tx, e.getMessage)))
        }
    }
    validatedTxs
  }

  def resolveAlias(a: Alias): Option[Account] = storage
    .addressByAlias(a.name)
    .map(addr => Account.fromBase58String(addr).right.get)

  def getAlias(a: Account): Option[Alias] = storage
    .aliasByAddress(a.address)
    .map(addr => Alias(addr).right.get)

  def persistAlias(ac: Account, al: Alias): Unit = storage.persistAlias(ac.address, al.name)

  override final def validate(trans: Seq[Transaction], heightOpt: Option[Int] = None, blockTime: Long): (Seq[ValidationError], Seq[Transaction]) = {
    val height = heightOpt.getOrElse(storage.stateHeight)
    val (err0, validOneByOne) = validAgainstStateOneByOne(height, trans).segregate()
    val (err1, validAgainstConsecutivePayments) = filterIfPaymentTransactionWithGreaterTimesatampAlreadyPresent(validOneByOne).segregate()
    val (err2, filteredFarFuture) = filterTransactionsFromFuture(validAgainstConsecutivePayments, blockTime).segregate()
    val allowUnissuedAssets = filteredFarFuture.nonEmpty && validOneByOne.map(_.timestamp).max < settings.allowUnissuedAssetsUntil
    val (err3, filteredOvermatch) = validateExchangeTxs(filteredFarFuture, height).segregate()
    val (err4, result) = filterByBalanceApplicationErrors(allowUnissuedAssets, filteredOvermatch).segregate()
    (err0 ++ err1 ++ err2 ++ err3 ++ err4, result)
  }

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

      val ebcs = BalanceChangeCalculator.effectiveBalanceChanges(this)(storage)(tx).right.get
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

  def totalAssetQuantity(assetId: AssetId): Long = assetsExtension.getAssetQuantity(assetId)

  def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)],
                   blockTs: Long = NTP.correctedTime()): Unit = synchronized {
    storage.setStateHeight(storage.stateHeight + 1)
    val h = storage.stateHeight
    changes.foreach { ch =>
      val change = Row(ch._2._1, ch._2._2.map(_.id), storage.getLastStates(ch._1.key).getOrElse(0))
      storage.putAccountChanges(ch._1.key, h, change)
      storage.putLastStates(ch._1.key, h)
      ch._2._2.foreach {
        case tx: Transaction =>
          processors.foreach(_.apply(this, tx, blockTs, h))
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

  def validateAgainstState(transaction: Transaction, height: Int): Either[ValidationError, Transaction] = {
    validators.toStream.map(_.apply(this, transaction, height)).find(_.isLeft) match {
      case Some(Left(e)) => Left(e)
      case _ => Right(transaction)
    }
  }


  private def getIssueTransaction(assetId: AssetId): Option[IssueTransaction] =
    storage.getTransactionBytes(assetId).flatMap(b => IssueTransaction.parseBytes(b).toOption)


  //for debugging purposes only
  def totalBalance: Long = storage.lastStatesKeys.map(address => balanceByKey(address, _.balance, storage.stateHeight)).sum

  //for debugging purposes only
  def toJson(heightOpt: Option[Int]): JsObject = {
    val ls = storage.lastStatesKeys.map(add => add -> balanceByKey(add, _.balance, heightOpt.getOrElse(storage.stateHeight)))
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
  def hash: Int = {
    (BigInt(FastCryptographicHash(toJson(None).toString().getBytes)) % Int.MaxValue).toInt
  }

  override def effectiveBalance(account: Account): Long = balanceByKey(account.address, _.effectiveBalance, storage.stateHeight)

  override def effectiveBalanceWithConfirmations(account: Account, confirmations: Int): Long =
    balanceByKey(account.address, _.effectiveBalance, heightWithConfirmations(None, confirmations))

  override def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, height: Int): Long =
    balanceByKey(account.address, _.effectiveBalance, heightWithConfirmations(Some(height), confirmations))

  def getAssetQuantity(assetId: AssetId): Long = assetsExtension.getAssetQuantity(assetId)

  def getAssetName(assetId: AssetId): String = assetsExtension.getAssetName(assetId)


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

}
