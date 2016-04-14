package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import play.api.libs.json.{JsNumber, JsObject}
import scorex.account.Account
import scorex.block.Block
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction._
import scorex.transaction.state.database.state._
import scorex.utils.ScorexLogging

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.util.Try


/** Store current balances only, and balances changes within effective balance depth.
  * Store transactions for selected accounts only.
  * If no filename provided, blockchain lives in RAM (intended for tests only).
  *
  * Use apply method of StoredState object to create new instance
  */
class StoredState(fileNameOpt: Option[String]) extends LagonakiState with ScorexLogging {


  val HeightKey = "height"
  val DataKey = "dataset"
  val LastStates = "lastStates"
  val IncludedTx = "includedTx"

  private val db = fileNameOpt match {
    case Some(fileName) => new MVStore.Builder().fileName(fileName).compress().open()
    case None => new MVStore.Builder().open()
  }
  db.rollback()

  private def accountChanges(key: Address): MVMap[Int, Row] = db.openMap(key.toString)

  private val lastStates: MVMap[Address, Int] = db.openMap(LastStates)

  /**
    * Transaction Signature -> Block height Map
    */
  private val includedTx: MVMap[Array[Byte], Int] = db.openMap(IncludedTx)

  private val heightMap: MVMap[String, Int] = db.openMap(HeightKey)

  if (Option(heightMap.get(HeightKey)).isEmpty) heightMap.put(HeightKey, 0)

  def stateHeight: Int = heightMap.get(HeightKey)

  private def setStateHeight(height: Int): Unit = heightMap.put(HeightKey, height)

  private def applyChanges(ch: Map[Address, (AccState, Reason)]): Unit = synchronized {
    setStateHeight(stateHeight + 1)
    val h = stateHeight
    ch.foreach { ch =>
      val change = Row(ch._2._1, ch._2._2, Option(lastStates.get(ch._1)).getOrElse(0))
      accountChanges(ch._1).put(h, change)
      lastStates.put(ch._1, h)
      ch._2._2.foreach(t => includedTx.put(t.signature, h))
    }
    db.commit()
  }

  def rollbackTo(rollbackTo: Int): State = synchronized {
    def deleteNewer(key: Address): Unit = {
      val currentHeight = lastStates.get(key)
      if (currentHeight > rollbackTo) {
        val dataMap = accountChanges(key)
        val changes = dataMap.remove(currentHeight)
        changes.reason.foreach(t => includedTx.remove(t.signature))
        val prevHeight = changes.lastRowHeight
        lastStates.put(key, prevHeight)
        deleteNewer(key)
      }
    }
    lastStates.keySet().foreach { key =>
      deleteNewer(key)
    }
    setStateHeight(rollbackTo)
    db.commit()
    this
  }

  override def processBlock(block: Block): Try[State] = Try {
    val trans = block.transactions
    trans.foreach(t => if (included(t).isDefined) throw new Error(s"Transaction $t is already in state"))
    val fees: Map[Account, (AccState, Reason)] = block.consensusModule.feesDistribution(block)
      .map(m => m._1 ->(AccState(balance(m._1.address) + m._2), Seq(FeesStateChange(m._2))))

    val newBalances: Map[Account, (AccState, Reason)] = calcNewBalances(trans, fees)
    newBalances.foreach(nb => require(nb._2._1.balance >= 0))

    applyChanges(newBalances.map(a => a._1.address -> a._2))
    log.debug(s"New state height is $stateHeight, hash: $hash, totalBalance: $totalBalance")

    this
  }

  private def calcNewBalances(trans: Seq[Transaction], fees: Map[Account, (AccState, Reason)]):
  Map[Account, (AccState, Reason)] = {
    val newBalances: Map[Account, (AccState, Reason)] = trans.foldLeft(fees) { case (changes, atx) =>
      atx match {
        case tx: LagonakiTransaction =>
          tx.balanceChanges().foldLeft(changes) { case (iChanges, (acc, delta)) =>
            //update balances sheet
            val add = acc.address
            val t: Map[Account, (AccState, Reason)] = iChanges
            val currentChange: (AccState, Reason) = iChanges.getOrElse(acc, (AccState(balance(add)), Seq.empty))
            iChanges.updated(acc, (AccState(currentChange._1.balance + delta), tx +: currentChange._2))
          }

        case m =>
          throw new Error("Wrong transaction type in pattern-matching" + m)
      }
    }
    newBalances
  }

  override def balanceWithConfirmations(address: String, confirmations: Int): Long =
    balance(address, Some(Math.max(1, stateHeight - confirmations)))

  override def balance(address: String, atHeight: Option[Int] = None): Long = Option(lastStates.get(address)) match {
    case Some(h) if h > 0 =>
      val requiredHeight = atHeight.getOrElse(stateHeight)
      require(requiredHeight >= 0, s"Height should not be negative, $requiredHeight given")
      def loop(hh: Int): Long = {
        val row = accountChanges(address).get(hh)
        require(Option(row).isDefined, s"accountChanges($address).get($hh) is null.  lastStates.get(address)=$h")
        if (row.lastRowHeight < requiredHeight) row.state.balance
        else if (row.lastRowHeight == 0) 0L
        else loop(row.lastRowHeight)
      }
      loop(h)
    case _ => 0L
  }

  def totalBalance: Long = lastStates.keySet().map(add => balance(add)).sum

  override def accountTransactions(account: Account): Array[LagonakiTransaction] = {
    Option(lastStates.get(account.address)) match {
      case Some(accHeight) =>
        val m = accountChanges(account.address)
        def loop(h: Int, acc: Array[LagonakiTransaction]): Array[LagonakiTransaction] = Option(m.get(h)) match {
          case Some(heightChangesBytes) =>
            val heightChanges = heightChangesBytes
            val heightTransactions = heightChanges.reason.toArray.filter(_.isInstanceOf[LagonakiTransaction])
              .map(_.asInstanceOf[LagonakiTransaction])
            loop(heightChanges.lastRowHeight, heightTransactions ++ acc)
          case None => acc
        }
        loop(accHeight, Array.empty)
      case None => Array.empty
    }
  }

  override def included(signature: Array[Byte], heightOpt: Option[Int]): Option[Int] =
    Option(includedTx.get(signature)).filter(_ < heightOpt.getOrElse(Int.MaxValue))

  //return seq of valid transactions
  @tailrec
  override final def validate(trans: Seq[Transaction], heightOpt: Option[Int] = None): Seq[Transaction] = {
    val height = heightOpt.getOrElse(stateHeight)
    val txs = trans.filter(t => included(t).isEmpty && isValid(t, height))
    val nb = calcNewBalances(txs, Map.empty)
    val negativeBalances: Map[Account, (AccState, Reason)] = nb.filter(b => b._2._1.balance < 0)
    val toRemove: Iterable[Transaction] = negativeBalances flatMap { b =>
      val accTransactions = trans.filter(_.isInstanceOf[PaymentTransaction]).map(_.asInstanceOf[PaymentTransaction])
        .filter(_.sender.address == b._1.address)
      var sumBalance = b._2._1.balance
      accTransactions.sortBy(-_.amount).takeWhile { t =>
        val prevSum = sumBalance
        sumBalance = sumBalance + t.amount + t.fee
        prevSum < 0
      }
    }
    val validTransactions = txs.filter(t => !toRemove.exists(tr => tr.signature sameElements t.signature))
    if (validTransactions.size == txs.size) txs
    else if (validTransactions.nonEmpty) validate(validTransactions, heightOpt)
    else validTransactions
  }

  private def isValid(transaction: Transaction, height: Int): Boolean = transaction match {
    case tx: PaymentTransaction =>
      tx.signatureValid && tx.validate == ValidationResult.ValidateOke && this.included(tx, Some(height)).isEmpty
    case gtx: GenesisTransaction =>
      height == 0
    case otx: Any =>
      log.error(s"Wrong kind of tx: $otx")
      false
  }

  //for debugging purposes only
  def toJson(heightOpt: Option[Int] = None): JsObject = {
    val ls = lastStates.keySet().map(add => add -> balance(add, heightOpt)).filter(b => b._2 != 0).toList.sortBy(_._1)
    JsObject(ls.map(a => a._1 -> JsNumber(a._2)).toMap)
  }

  //for debugging purposes only
  override def toString: String = toJson().toString()

  def hash: Int = {
    (BigInt(FastCryptographicHash(toString.getBytes())) % Int.MaxValue).toInt
  }
}