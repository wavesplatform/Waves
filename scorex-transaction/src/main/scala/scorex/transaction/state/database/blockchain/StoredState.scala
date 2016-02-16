package scorex.transaction.state.database.blockchain

import java.io.{DataInput, DataOutput, File}

import org.mapdb._
import play.api.libs.json.{JsNumber, JsObject}
import scorex.account.Account
import scorex.block.Block
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.util.{Random, Try}


/** Store current balances only, and balances changes within effective balance depth.
  * Store transactions for selected accounts only.
  * If no filename provided, blockchain lives in RAM (intended for tests only).
  *
  * Use apply method of StoredState object to create new instance
  */
class StoredState(fileNameOpt: Option[String]) extends LagonakiState with ScorexLogging {

  private object RowSerializer extends Serializer[Row] {
    override def serialize(dataOutput: DataOutput, row: Row): Unit = {
      DataIO.packInt(dataOutput, row.lastRowHeight)
      DataIO.packLong(dataOutput, row.state.balance)
      DataIO.packInt(dataOutput, row.reason.length)
      row.reason.foreach { scr =>
        DataIO.packInt(dataOutput, scr.bytes.length)
        dataOutput.write(scr.bytes)
      }
    }

    override def deserialize(dataInput: DataInput, i: Int): Row = {
      val lastRowHeight = DataIO.unpackInt(dataInput)
      val b = DataIO.unpackLong(dataInput)
      val txCount = DataIO.unpackInt(dataInput)
      val txs = (1 to txCount).toArray.map { _ =>
        val txSize = DataIO.unpackInt(dataInput)
        val b = new Array[Byte](txSize)
        dataInput.readFully(b)
        if (txSize > 0) LagonakiTransaction.parse(b).get //todo: .get w/out catching
        else FeesStateChange
      }
      Row(AccState(b), txs, lastRowHeight)
    }
  }

  type Adress = String

  case class AccState(balance: Long)

  type Reason = Seq[StateChangeReason]

  case class Row(state: AccState, reason: Reason, lastRowHeight: Int)

  val HeightKey = "height"
  val DataKey = "dataset"
  val LastStates = "lastStates"

  private val db = fileNameOpt match {
    case Some(fileName) =>
      DBMaker.fileDB(new File(fileName))
        .closeOnJvmShutdown()
        .cacheSize(2048)
        .checksumEnable()
        .fileMmapEnable()
        .make()
    case None => DBMaker.memoryDB().snapshotEnable().make()
  }

  private def accountChanges(key: Adress): HTreeMap[Integer, Row] = db.hashMap(
    key.toString,
    Serializer.INTEGER,
    RowSerializer,
    null)

  val lastStates = db.hashMap[Adress, Int](LastStates)

  if (Option(db.atomicInteger(HeightKey).get()).isEmpty) db.atomicInteger(HeightKey).set(0)

  def stateHeight: Int = db.atomicInteger(HeightKey).get()

  private def setStateHeight(height: Int): Unit = db.atomicInteger(HeightKey).set(height)

  private def applyChanges(ch: Map[Adress, (AccState, Reason)]): Unit = synchronized {
    val h = stateHeight
    ch.foreach { ch =>
      val change = Row(ch._2._1, ch._2._2, Option(lastStates.get(ch._1)).getOrElse(0))
      accountChanges(ch._1).put(h, change)
      lastStates.put(ch._1, h)
    }
    setStateHeight(h + 1)
    db.commit()
  }

  def rollbackTo(rollbackTo: Int): Unit = synchronized {
    def deleteNewer(key: Adress): Unit = {
      val currentHeight = lastStates.get(key)
      if (currentHeight > rollbackTo) {
        val dataMap = accountChanges(key)
        val prevHeight = dataMap.remove(currentHeight).lastRowHeight
        lastStates.put(key, prevHeight)
        deleteNewer(key)
      }
    }
    lastStates.keySet().foreach { key =>
      deleteNewer(key)
    }
    setStateHeight(rollbackTo)
    db.commit()
  }

  override def processBlock(block: Block): Try[State] = Try {
    val trans = block.transactions
    trans.foreach(t => if (included(t).isDefined) throw new Error(s"Transaction $t is already in state"))
    val fees: Map[Account, (AccState, Reason)] = block.consensusModule.feesDistribution(block)
      .map(m => m._1 ->(AccState(balance(m._1.address) + m._2), Seq(FeesStateChange)))

    val newBalances: Map[Account, (AccState, Reason)] = calcNewBalances(trans, fees)
    newBalances.foreach(nb => require(nb._2._1.balance >= 0))

    applyChanges(newBalances.map(a => a._1.address -> a._2))
    log.debug(s"New state height is $stateHeight, hash: $hash")

    this
  }

  def calcNewBalances(trans: Seq[Transaction], fees: Map[Account, (AccState, Reason)]):
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
    case None => 0L
    case Some(h) =>
      val requiredHeight = atHeight.getOrElse(stateHeight)
      require(requiredHeight >= 0, s"Height should not be negative, $requiredHeight given")
      def loop(hh: Int): Long = {
        val row = accountChanges(address).get(hh)
        if (row.lastRowHeight < requiredHeight) row.state.balance
        else if (row.lastRowHeight == 0) 0L
        else loop(row.lastRowHeight)
      }
      loop(h)
  }

  //  def totalBalance: Long = balances.keySet().toList.map(i => balances.get(i)).sum

  override def accountTransactions(account: Account): Array[LagonakiTransaction] = ???

  override def stopWatchingAccountTransactions(account: Account): Unit = ???

  override def watchAccountTransactions(account: Account): Unit = ???

  def included(tx: Transaction, heightOpt: Option[Int] = None): Option[Int] = {
    Option(lastStates.get(tx.recipient.address)).flatMap { lastChangeHeight =>
      def loop(hh: Int): Option[Int] = if (hh > 0) {
        val row = accountChanges(tx.recipient.address).get(hh)
        if (heightOpt.isDefined && heightOpt.get < hh) loop(row.lastRowHeight)
        else if (row.lastRowHeight > 0) {
          val inCurrentChange = row.reason.filter(_.isInstanceOf[Transaction])
            .exists(scr => scr.asInstanceOf[Transaction].signature sameElements tx.signature)
          if (inCurrentChange) Some(hh)
          else loop(row.lastRowHeight)
        } else None
      } else None
      loop(lastChangeHeight)
    }
  }

  //return seq of valid transactions
  override def validate(txs: Seq[Transaction], heightOpt: Option[Int] = None): Seq[Transaction] = {
    val height = heightOpt.getOrElse(stateHeight)
    val nb = calcNewBalances(txs, Map.empty)
    val negativeBalance: Option[(Account, (AccState, Reason))] = nb.find(b => b._2._1.balance < 0)
    negativeBalance match {
      case Some(b) => validate(Random.shuffle(txs).tail, Some(height))
      case None => txs
    }
  }

  private def isValid(transaction: Transaction, height: Int): Boolean = transaction match {
    case tx: PaymentTransaction =>
      val r = tx.signatureValid && tx.validate(this) == ValidationResult.ValidateOke &&
        this.included(tx, Some(height)).isEmpty
      if (!r) log.debug(s"Invalid $tx: ${tx.signatureValid}&&${tx.validate(this)}&&" +
        this.included(tx, Some(stateHeight)))
      r
    case gtx: GenesisTransaction =>
      height == 0
    case otx: Any =>
      log.error(s"Wrong kind of tx: $otx")
      false
  }

  def toJson: JsObject = {
    val ls = lastStates.keySet().map(add => add -> balance(add)).filter(b => b._2 != 0).toList.sortBy(_._1)
    JsObject(ls.map(a => a._1 -> JsNumber(a._2)).toMap)
  }

  //for debugging purposes only
  override def toString: String = toJson.toString()

  def hash: Int = {
    (BigInt(FastCryptographicHash(toString.getBytes())) % Int.MaxValue).toInt
  }
}