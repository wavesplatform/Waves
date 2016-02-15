package scorex.transaction.state.database.blockchain

import java.io.File

import org.mapdb._
import play.api.libs.json.{JsNumber, JsObject}
import scorex.account.Account
import scorex.block.Block
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scala.util.Try


/** Store current balances only, and balances changes within effective balance depth.
  * Store transactions for selected accounts only.
  * If no filename provided, blockchain lives in RAM (intended for tests only).
  *
  * Use apply method of StoredState object to create new instance
  */
class StoredState(fileNameOpt: Option[String]) extends LagonakiState with ScorexLogging {
  /*
    private object AccSerializer extends Serializer[Account] {
      override def serialize(dataOutput: DataOutput, a: Account): Unit =
        Serializer.STRING.serialize(dataOutput, a.address)

      override def deserialize(dataInput: DataInput, i: Int): Account = {
        val address = Serializer.STRING.deserialize(dataInput, i)
        new Account(address)
      }
    }

    private object TxArraySerializer extends Serializer[Array[LagonakiTransaction]] {
      override def serialize(dataOutput: DataOutput, txs: Array[LagonakiTransaction]): Unit = {
        DataIO.packInt(dataOutput, txs.length)
        txs.foreach { tx =>
          val bytes = tx.bytes
          DataIO.packInt(dataOutput, bytes.length)
          dataOutput.write(bytes)
        }
      }

      override def deserialize(dataInput: DataInput, i: Int): Array[LagonakiTransaction] = {
        val txsCount = DataIO.unpackInt(dataInput)
        (1 to txsCount).toArray.map { _ =>
          val txSize = DataIO.unpackInt(dataInput)
          val b = new Array[Byte](txSize)
          dataInput.readFully(b)
          LagonakiTransaction.parse(b).get //todo: .get w/out catching
        }
      }
    }*/

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

  private def accountChanges(key: Adress): HTreeMap[Int, Row] = db.hashMapCreate(key.toString).makeOrGet()

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
    //TODO require transaction is not included to state
    val fees: Map[Account, (AccState, Reason)] = block.consensusModule.feesDistribution(block)
      .map(m => m._1 ->(AccState(balance(m._1.address) + m._2), Seq(FeesStateChange)))

    val newBalances: Map[Account, (AccState, Reason)] = trans.foldLeft(fees) { case (changes, atx) =>
      atx match {
        case tx: LagonakiTransaction =>
          tx.balanceChanges().foldLeft(changes) { case (iChanges, (acc, delta)) =>
            //update balances sheet
            val add = acc.address
            val t: Map[Account, (AccState, Reason)] = iChanges
            val currentChange: (AccState, Reason) = iChanges.getOrElse(acc, (AccState(balance(add)), Seq.empty))
            require(currentChange._1.balance + delta >= 0, "Account balancve should not be negative")
            iChanges.updated(acc, (AccState(currentChange._1.balance + delta), tx +: currentChange._2))
          }

        case m =>
          throw new Error("Wrong" +
            "transaction type in pattern-matching" + m)
      }
    }

    applyChanges(newBalances.map(a => a._1.address -> a._2))
    log.debug(s"New state height is $stateHeight, hash: $hash")

    this
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
    val tmpBalances = TrieMap[Account, Long]()

    val r = txs.filter(t => isValid(t, height)).foldLeft(Seq.empty: Seq[Transaction]) { case (acc, atx) =>
      atx match {
        case tx: LagonakiTransaction =>
          val changes = tx.balanceChanges().foldLeft(Map.empty: Map[Account, Long]) { case (iChanges, (txAcc, delta)) =>
            //update balances sheet
            val currentChange = iChanges.getOrElse(txAcc, 0L)
            val newChange = currentChange + delta
            iChanges.updated(txAcc, newChange)
          }
          val check = changes.forall { a =>
            val b = tmpBalances.getOrElseUpdate(a._1, balance(a._1.address))
            val newBalance = b + a._2
            if (newBalance >= 0) tmpBalances.put(a._1, newBalance)
            newBalance >= 0
          }
          if (check) tx +: acc
          else acc
        case _ => acc
      }
    }
    if (r.size == txs.size) r else validate(r, Some(height))
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