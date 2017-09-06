package com.wavesplatform.database

import java.io.StringReader
import java.sql.Timestamp
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.ReentrantLock
import javax.sql.DataSource

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.state2.reader.{LeaseDetails, SnapshotStateReader}
import com.wavesplatform.state2.{AssetDescription, AssetInfo, ByteStr, Diff, LeaseBalance, VolumeAndFee, StateWriter}
import org.postgresql.copy.CopyManager
import org.postgresql.core.BaseConnection
import scalikejdbc.{DB, DBSession, using, _}
import scorex.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}
import scorex.block.Block
import scorex.transaction.{TransactionParser, _}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

class PostgresWriter(ds: DataSource) extends SnapshotStateReader with StateWriter {
  import PostgresWriter._


  override def portfolio(a: Address) = ???

  private def readOnly[A](f: DBSession => A): A = using(DB(ds.getConnection))(_.readOnly(f))

  private def localTx[A](f: DBSession => A): A = using(DB(ds.getConnection)) { db =>
    db.localTx(f(_))
  }

  override def accountScript(address: Address) = ???

  override def balanceSnapshots(address: Address, from: Int, to: Int) = ???

  override def transactionInfo(id: ByteStr) = ???

  override def containsTransaction(id: ByteStr) = readOnly { implicit s =>
    sql"select count(*) from transaction_offsets where tx_id = ?"
      .bind(id.arr)
      .map(_.get[Int](1))
      .single()
      .apply()
      .isEmpty
  }

  private val balanceCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000).build(new CacheLoader[Address, java.lang.Long] {
    override def load(key: Address) = readOnly { implicit s =>
      sql"""select wb.regular_balance, wb.effective_balance
           |from waves_balances wb
           |where wb.address = ?
           |order by wb.height desc
           |limit 1""".stripMargin
        .bind(key.address)
        .map(_.get[java.lang.Long](1))
        .single()
        .apply()
        .getOrElse(0L)
    }
  })

  private val assetBalanceCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .build(new CacheLoader[Address, Map[ByteStr, Long]] {
      override def load(key: Address) = readOnly { implicit s =>
        sql"""with latest_heights as (
             |  select address, asset_id, max(height) height
             |  from asset_balances
             |  where address = ? group by address, asset_id)
             |select ab.asset_id, ab.balance from asset_balances ab, latest_heights lh
             |where ab.height = lh.height
             |and ab.asset_id = lh.asset_id
             |and ab.address = lh.address""".stripMargin
          .bind(key.address)
          .map(rs => ByteStr.decodeBase58(rs.get[String](1)).get -> rs.get[Long](2))
          .list()
          .apply()
          .toMap
      }
    })

  private val assetInfoCache = CacheBuilder.newBuilder()
    .recordStats()
    .maximumSize(10000)
    .build(new CacheLoader[ByteStr, Option[AssetInfo]] {
      override def load(key: ByteStr) = readOnly { implicit s =>
        sql"select reissuable, quantity from asset_quantity where asset_id = ? order by height desc limit 1"
          .bind(key.base58)
          .map { rs => AssetInfo(rs.get[Boolean](1), rs.get[Long](2)) }
          .single()
          .apply()
      }
    })

  private val assetDescriptionCache = CacheBuilder.newBuilder()
    .recordStats()
    .maximumSize(10000)
    .build(new CacheLoader[ByteStr, Option[AssetDescription]] {
      override def load(key: ByteStr) = readOnly { implicit s =>
        sql"""select ai.issuer, ai.name, ai.decimals, bool_and(aq.reissuable)
             |from asset_info ai, asset_quantity aq
             |where ai.asset_id = aq.asset_id
             |and ai.asset_id = ?
             |group by ai.issuer, ai.name, ai.decimals""".stripMargin
          .bind(key.base58)
          .map { rs => AssetDescription(
            PublicKeyAccount(rs.get[Array[Byte]](1)),
            rs.get[Array[Byte]](2),
            rs.get[Int](3),
            rs.get[Boolean](4)) }
          .single()
          .apply()
      }
    })

  override def assetDescription(id: ByteStr) = assetDescriptionCache.get(id)

  private val h = new AtomicInteger(readOnly { implicit s =>
    sql"select coalesce(max(height), 0) from blocks".map(_.get[Int](1)).single().apply().getOrElse(0)
  })

  override def height = h.get()

  override def addressTransactions(address: Address, types: Set[TransactionParser.TransactionType.Value], from: Int, count: Int) = ???

  override def resolveAlias(a: Alias) = readOnly { implicit s =>
    sql"select address from aliases where alias = ?"
      .bind(a.bytes.arr)
      .map(rs => Address.fromString(rs.get[String](1)).right.get)
      .single()
      .apply()
  }

  override def activeLeases = ???

  override def leaseDetails(leaseId: ByteStr) = readOnly { implicit s =>
    sql"""with this_lease_status as (
         |select lease_id, bool_and(active) active from lease_status where lease_id = ? group by lease_id)
         |select li.*, tl.active from lease_info li, this_lease_status tl
         |where li.lease_id = tl.lease_id""".stripMargin
      .bind(leaseId.base58)
      .map(rs => LeaseDetails(
        PublicKeyAccount(rs.get[Array[Byte]](2)),
        AddressOrAlias.fromString(rs.get[String](3)).right.get,
        rs.get[Int](5),
        rs.get[Long](4),
        rs.get[Boolean](6)))
      .single()
      .apply()
  }

  override def filledVolumeAndFee(orderId: ByteStr) = readOnly { implicit s =>
    sql"""with this_order as (select ? order_id)
         |select coalesce(fq.filled_quantity, 0), coalesce(fq.fee, 0) from this_order tho
         |left join filled_quantity fq on tho.order_id = fq.order_id
         |order by fq.height desc
         |limit 1""".stripMargin
      .bind(orderId.base58)
      .map(rs => VolumeAndFee(rs.get[Long](1), rs.get[Long](2)))
      .single()
      .apply()
      .getOrElse(VolumeAndFee(0, 0))
  }

  def effectiveBalanceAtHeightWithConfirmations(acc: Address, atHeight: Int, confirmations: Int) =
    readOnly { implicit s =>
      sql"""with lowest_height as (
           |    select height, address from waves_balances
           |    where address = ? and height <= ?
           |    order by height desc limit 1)
           |select coalesce(min(wb.effective_balance), 0) from waves_balances wb, lowest_height lh
           |where wb.address = lh.address
           |and wb.height <= ?
           |and wb.height >= lh.height""".stripMargin
        .bind(acc.address, atHeight - confirmations, atHeight)
        .map(_.get[Long](1))
        .single()
        .apply()
        .getOrElse(0L)
    }

  override def append(diff: Diff, block: Block): Unit = localTx { implicit s =>
    val newHeight = sql"select coalesce(max(height), 0) from blocks".map(_.get[Int](1)).single().apply().fold(1)(_ + 1)

    h.set(newHeight)

    storeBlocks(block, newHeight)
    storeIssuedAssets(diff, newHeight)
    storeReissuedAssets(diff, newHeight)
    storeFilledQuantity(diff, newHeight)
    storeLeaseInfo(diff, newHeight)

    sql"insert into lease_status (lease_id, active, height) values (?,?,?)"
      .batch(diff.transactions.collect {
        case (_, (_, lt: LeaseTransaction, _)) => Seq(lt.id().base58, true, newHeight)
        case (_, (_, lc: LeaseCancelTransaction, _)) => Seq(lc.leaseId.base58, false, newHeight)
      }.toSeq: _*)
      .apply()

    storeLeaseBalances(diff, newHeight)
    storeWavesBalances(diff, newHeight)
    storeAssetBalances(diff, newHeight)

    sql"insert into aliases (alias, address, height) values (?,?,?)"
      .batch(diff.transactions.values.collect {
        case (_, cat: CreateAliasTransaction, _) => Seq(cat.alias.bytes.arr, cat.sender.toAddress.address, newHeight)
      }.toSeq: _*)
      .apply()

    lock.lock()
    try blocksAdded.signal() finally lock.unlock()
  }

  private def storeWavesBalances(blockDiff: Diff, newHeight: Int)(implicit session: DBSession) = ???


  private def storeLeaseBalances(diff: Diff, newHeight: Int)(implicit session: DBSession) = {
    sql"""insert into lease_balances (address, lease_in, lease_out, height)
         |select address, lease_in + ?, lease_out + ?, ? from (
         |(select * from lease_balances
         |where address = ?
         |order by height desc limit 1)
         |union all
         |(select ?, 0, 0, 0)
         |) as foo order by height desc limit 1""".stripMargin
      .batch((for {
        (address, p) <- diff.portfolios
        if p.lease.in != 0 || p.lease.out != 0
      } yield Seq(p.lease.in, p.lease.out, newHeight, address.address, address.address)).toSeq: _*)
      .apply()
  }

  private def storeAssetBalances(blockDiff: Diff, newHeight: Int)(implicit session: DBSession) = ???

  private def storeLeaseInfo(diff: Diff, newHeight: Int)(implicit session: DBSession) = {
    sql"insert into lease_info (lease_id, sender, recipient, amount, height) values (?,?,?,?,?)"
      .batch(diff.transactions.collect {
        case (_, (_, lt: LeaseTransaction, _)) =>
          Seq(lt.id().base58, lt.sender.publicKey, lt.recipient.stringRepr, lt.amount, newHeight)
      }.toSeq: _*)
      .apply()
  }

  private def storeFilledQuantity(diff: Diff, newHeight: Int)(implicit session: DBSession) =
    sql"""insert into filled_quantity(order_id, filled_quantity, fee, height)
         |select order_id, filled_quantity + ?, fee + ?, ? from (
         |(select * from filled_quantity where order_id = ? order by height desc limit 1)
         |union all
         |(select ?, 0, 0, 0)
         |) as latest_filled_quantity order by height desc limit 1""".stripMargin
      .batch((for {
        (orderId, fillInfo) <- diff.orderFills
      } yield Seq(fillInfo.volume, fillInfo.fee, newHeight, orderId.base58, orderId.base58)).toSeq: _*)
      .apply()

  private def storeReissuedAssets(diff: Diff, newHeight: Int)(implicit session: DBSession) = {
    sql"""insert into asset_quantity
         |with this_asset as (select ? asset_id)
         |select ta.asset_id, coalesce(aq.total_quantity, 0) + ?, ?, ?
         |from this_asset ta
         |left join asset_quantity aq on ta.asset_id = aq.asset_id
         |order by aq.height desc
         |limit 1""".stripMargin
      .batch(diff.issuedAssets.map { case (id, ai) => Seq(id.base58, ai.volume, ai.isReissuable, newHeight) }.toSeq: _*)
      .apply()
  }

  private def storeIssuedAssets(diff: Diff, newHeight: Int)(implicit session: DBSession) = {
    val issuedAssetParams = diff.transactions.values.collect {
      case (_, i: IssueTransaction, _) =>
        Seq(i.assetId().base58, i.sender.publicKey, i.decimals, i.name, i.description, newHeight): Seq[Any]
    }.toSeq

    sql"insert into asset_info(asset_id, issuer, decimals, name, description, height) values (?,?,?,?,?,?)"
      .batch(issuedAssetParams: _*)
      .apply()

    diff.issuedAssets.foreach {
      case (id, ai) => assetInfoCache.put(id, Some(ai))
    }
  }

  private def storeBlocks(block: Block, newHeight: Int)(implicit session: DBSession): Unit =
    sql"""insert into blocks (height, block_id, block_timestamp, generator_address, block_data_bytes, score)
         |values (?,?,?,?,?,?)""".stripMargin
      .bind(newHeight, block.uniqueId.base58, new Timestamp(block.timestamp),
        block.signerData.generator.toAddress.stringRepr, block.bytes, block.blockScore)
      .update()
      .apply()

  private def storeAddressTransactionIds(diff: Diff, newHeight: Int)(implicit session: DBSession) = {
    val input = for {
      (_, (_, tx, addresses)) <- diff.transactions
      address <- addresses
    } yield tx match {
      case pt: PaymentTransaction => s"${address.address},${ByteStr(pt.hash()).base58},${pt.signature.base58},$newHeight"
      case t: SignedTransaction => s"${address.address},${t.id().base58},${t.signature.base58},$newHeight"
      case gt: GenesisTransaction => s"${address.address},${gt.id().base58},${gt.signature.base58},$newHeight"
    }

    new CopyManager(session.connection.unwrap(classOf[BaseConnection])).copyIn(
      "copy address_transaction_ids (address, tx_id, signature, height) from stdin with (format csv)",
      new StringReader(input.mkString("\n"))
    )
  }

  private def storeTransactions(transactions: Seq[Transaction], newHeight: Int)(implicit session: DBSession) = {
    val exchangeParams = Seq.newBuilder[Seq[Any]]
    val transferParams = Seq.newBuilder[Seq[Any]]
    val transactionParams = Seq.newBuilder[Seq[Any]]

    transactions.foreach {
      case pt: PaymentTransaction =>
        transactionParams += Seq(ByteStr(pt.hash()).base58, pt.signature.base58, transactionTypes(pt.transactionType.id - 1), "{}", newHeight)
      case t: SignedTransaction =>
        transactionParams += Seq(t.id().base58, t.signature.base58, transactionTypes(t.transactionType.id - 1),  "{}", newHeight)
        t match {
          case et: ExchangeTransaction =>
            exchangeParams += Seq(et.id().base58, et.buyOrder.assetPair.amountAsset.map(_.base58).getOrElse(""),
              et.buyOrder.assetPair.priceAsset.map(_.base58).getOrElse(""), et.amount, et.price, newHeight)
          case tt: TransferTransaction =>
            transferParams += Seq(tt.id().base58, tt.sender.address, tt.recipient.stringRepr, tt.assetId.map(_.base58).getOrElse(""), tt.amount,
              tt.feeAssetId.map(_.base58).getOrElse(""), tt.fee, newHeight)
          case _ =>
        }
      case gt: GenesisTransaction =>
        transactionParams += Seq(gt.id().base58, gt.signature.base58, transactionTypes(gt.transactionType.id - 1), "{}", newHeight)
    }

    val copyManager = new CopyManager(session.connection.unwrap(classOf[BaseConnection]))

    copyManager.copyIn(
      "copy transactions (tx_id, signature, tx_type, tx_json, height) from stdin with (format csv)",
      new StringReader(transactionParams.result().map(_.mkString(",")).mkString("\n")))

    copyManager.copyIn(
      "copy exchange_transactions (tx_id, amount_asset_id, price_asset_id, amount, price, height) from stdin with (format csv)",
      new StringReader(exchangeParams.result().map(_.mkString(",")).mkString("\n")))

    copyManager.copyIn(
      "copy transfer_transactions (tx_id, sender, recipient, asset_id, amount, fee_asset_id, fee, height) from stdin with (format csv)",
      new StringReader(transferParams.result().map(_.mkString(",")).mkString("\n")))
  }

  private val lock = new ReentrantLock()
  private val blocksAdded = lock.newCondition()

  {
    val t = new Thread(() => while (true) {
      lock.lock()
      try {
        blocksAdded.await()

        localTx { implicit s =>
          sql"""with max_transaction_height as (select max(height) height from transactions)
               |select blocks.height, blocks.block_data_bytes from blocks, max_transaction_height
               |where blocks.height > max_transaction_height.height
               |and blocks.tx_count > 0
               |order by blocks.height asc
               |limit 5
             """.stripMargin
            .map(rs => rs.get[Int](1) -> Block.parseBytes(rs.get[Array[Byte]](2)).get.transactionData)
            .list()
            .apply()
            .foreach {
              case (height, transactions) => storeTransactions(transactions, height)
            }
        }

      } finally lock.unlock()
    }, "transaction-updater")
    t.setDaemon(true)
    t.run()
  }

  override def rollbackTo(targetBlockId: ByteStr) = ???
}

object PostgresWriter {
  val transactionTypes = IndexedSeq(
    "genesis",
    "payment",
    "issue",
    "transfer",
    "reissue",
    "burn",
    "exchange",
    "lease",
    "lease_cancel",
    "create_alias")
}

