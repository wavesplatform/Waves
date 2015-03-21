package scorex.database

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer

import com.yandex.yoctodb.DatabaseFormat
import com.yandex.yoctodb.immutable.Database
import com.yandex.yoctodb.mutable.DocumentBuilder
import com.yandex.yoctodb.query.QueryBuilder._
import com.yandex.yoctodb.query.{DocumentProcessor, QueryBuilder}
import com.yandex.yoctodb.util.UnsignedByteArrays
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable


class YoctoBlockchainImpl extends BlockChain {

  val signaturesIndex = TrieMap[Int, Array[Byte]]()
  val blocksIndex = TrieMap[Int, Block]()

  //todo: block fees
  override def appendBlock(block: Block): BlockChain = {
    val dbBuilder = DatabaseFormat.getCurrent.newDatabaseBuilder()

    val blockTransactions = block.transactions

    blockTransactions.foreach { tx =>
      val db0 = DatabaseFormat.getCurrent.newDocumentBuilder()

      val documentBuilder = tx match {
        case ptx: PaymentTransaction =>
          db0.withField("account", tx.getCreator().get.address, DocumentBuilder.IndexOption.FILTERABLE)
            .withField("account", ptx.recipient.address, DocumentBuilder.IndexOption.FILTERABLE)
            .withPayload(ptx.toBytes())

        case gtx: GenesisTransaction =>
          db0.withField("account", gtx.recipient.address, DocumentBuilder.IndexOption.FILTERABLE)
            .withPayload(gtx.toBytes())

        case _ => throw new RuntimeException(s"Serialization not implemented for $tx")
      }

      dbBuilder.merge(documentBuilder)
    }


    val h = height() + 1
    val os = new FileOutputStream(filename(h))
    dbBuilder.buildWritable().writeTo(os)
    signaturesIndex += h -> block.signature
    blocksIndex += h -> block
    this
  }

  override def heightOf(block: Block): Option[Int] = signaturesIndex.find(_._2.sameElements(block.signature)).map(_._1)

  override def blockAt(height: Int): Option[Block] = blocksIndex.get(height)

  override def contains(block: Block): Boolean = signaturesIndex.exists(_._2.sameElements(block.signature))

  override def accountTransactions(account: Account): Seq[Transaction] = {
    val chainDb = compositeDb()
    val q1 = select().where(QueryBuilder.eq("account", UnsignedByteArrays.from(account.address)))

    val seq = mutable.Buffer[Transaction]()

    chainDb.execute(q1, new DocumentProcessor {
      override def process(i: Int, database: Database): Boolean = {
        seq += transactionFromByteBuffer(database.getDocument(i))
        true
      }
    })

    seq.toSeq
  }

  //todo: fromHeight & confirmations parameters ignored now
  override def balance(address: String, fromHeight: Int, confirmations: Int): BigDecimal = {
    val chainDb = compositeDb()

    val q1 = select().where(QueryBuilder.eq("account", UnsignedByteArrays.from(address)))

    //todo: concurrency problems with mutable.Buffer?
    val seq = mutable.Buffer[BigDecimal]()

    chainDb.execute(q1, new DocumentProcessor {
      override def process(i: Int, database: Database): Boolean = {
        val tx = transactionFromByteBuffer(database.getDocument(i))
        seq += tx.getAmount(new Account(address))
        true
      }
    })

    seq.sum
  }

  private def compositeDb() = {
    val dbs = (1 to height()).toSeq.map { h =>
      DatabaseFormat.getCurrent
        .getDatabaseReader
        .from(new File(filename(h)), false)
    }
    DatabaseFormat.getCurrent.getDatabaseReader.composite(dbs)
  }

  override def height(): Int = signaturesIndex.size

  private def filename(height: Int) = s"/tmp/block-${height + 1}"

  private def transactionFromByteBuffer(bb: ByteBuffer): Transaction = {
    val ba = new Array[Byte](bb.remaining())
    bb.get(ba)
    Transaction.fromBytes(ba)
  }

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = ???

  override def blockByHeader(signature: Array[Byte]): Option[Block] =
    signaturesIndex.find(_._2.sameElements(signature)).map(_._1).map(h => blocksIndex(h))

  override def confirmations(tx: Transaction): Option[Int] = ???

  override def discardBlock(): BlockChain = ???

  override def child(block: Block): Option[Block] = ???

  override def generatedBy(account: Account): Seq[Block] = ???
}