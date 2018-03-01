package com.wavesplatform.database

import java.sql.Timestamp
import javax.sql.DataSource

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.LeaseDetails
import com.wavesplatform.state2.{AssetDescription, AssetInfo, BalanceSnapshot, ByteStr, LeaseBalance, Portfolio, VolumeAndFee}
import scalikejdbc._
import scorex.account.{Address, Alias, PublicKeyAccount}
import scorex.block.Block
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.Script
import scorex.transaction.{TransactionParser, _}

object SQLiteWriter {
  object q {
    val height = sql"select max(height) from blocks".map(_.get[Option[Int]](1))
    val score =  sql"select cumulative_score from blocks order by height desc limit 1".map(rs => BigInt(rs.get[String](1)))
    val blockBytesByHeight = sql"select block_data_bytes from blocks where height = ?".map(_.get[Array[Byte]](1))
    val blockBytesById = sql"select block_data_bytes from blocks where block_id = ?".map(_.get[Array[Byte]](1))
    val wavesBalance =
      sql"""select wb.regular_balance from waves_balances wb
           |where wb.address = ?
           |order by wb.height desc
           |limit 1""".stripMargin.map(_.get[Long](1))
    val leaseBalance = sql"select lease_in, lease_out from lease_balances where address = ? order by height desc limit 1"
      .map(rs => LeaseBalance(rs.get[Long](1), rs.get[Long](2)))

    val assetBalance =
      sql"""with latest_heights as (
           |  select address, asset_id, max(height) height
           |  from asset_balances
           |  where address = ? group by address, asset_id)
           |select ab.asset_id, ab.balance from asset_balances ab, latest_heights lh
           |where ab.height = lh.height
           |and ab.asset_id = lh.asset_id
           |and ab.address = lh.address""".stripMargin
      .map(rs => rs.get[ByteStr](1) -> rs.get[Long](2))

    val aliasesOfAddress = sql"select alias from aliases where address = ?"
      .map(_.get[Alias](1))

    val addressOfAlias = sql"select address from aliases where alias = ?"
      .map(_.get[Address](1))

    val volumeAndFee =
      sql"""with this_order as (select ? order_id)
           |select ifnull(fq.filled_quantity, 0), ifnull(fq.fee, 0) from this_order tho
           |left join filled_quantity fq on tho.order_id = fq.order_id
           |order by fq.height desc limit 1""".stripMargin
      .map(rs => VolumeAndFee(rs.get[Long](1), rs.get[Long](2)))

    val lastBlockBytes = sql"select block_data_bytes from blocks order by height desc limit 1"
      .map(_.get[Array[Byte]](1))

    val assetInfo = sql"select reissuable, quantity from asset_quantity where asset_id = ? order by height desc limit 1"
      .map { rs => AssetInfo(rs.get[Boolean](1), rs.get[Long](2)) }

    val assetDescription =
      sql"""select ai.issuer, ai.name, ai.decimals, min(aq.reissuable)
        |from asset_info ai, asset_quantity aq
        |where ai.asset_id = aq.asset_id
        |and ai.asset_id = ?
        |group by ai.issuer, ai.name, ai.decimals""".stripMargin
      .map { rs => AssetDescription(
        rs.get[PublicKeyAccount](1),
        rs.get[Array[Byte]](2),
        rs.get[Int](3),
        rs.get[Boolean](4)) }
  }
}

class SQLiteWriter(ds: DataSource, fs: FunctionalitySettings) extends Caches {
  import SQLiteWriter.q

  private def readOnly[A](f: DBSession => A): A = using(DB(ds.getConnection))(_.localTx(f))

  private def roOpt[A](query: SQL[A, HasExtractor], params: Any*): Option[A] = readOnly { implicit s =>
    query.bind(params: _*).single().apply()
  }

  private def roMap[K, V](query: SQL[(K, V), HasExtractor], params: Any*): Map[K, V] = readOnly { implicit s =>
    query.bind(params: _*).list().apply().toMap
  }

  private def roSeq[A](query: SQL[A, HasExtractor], params: Any*): Seq[A] = readOnly { implicit s =>
    query.bind(params: _*).list().apply()
  }

  override protected def loadMaxAddressId(): BigInt = ???

  override protected def loadAddressId(address: Address): Option[BigInt] = ???

  override protected def doAppend(block: Block, addresses: Map[Address, BigInt], wavesBalances: Map[BigInt, Long], assetBalances: Map[BigInt, Map[AssetId, Long]], leaseBalances: Map[BigInt, LeaseBalance], leaseStates: Map[AssetId, Boolean], transactions: Map[AssetId, (Transaction, Set[BigInt])], reissuedAssets: Map[AssetId, AssetInfo], filledQuantity: Map[AssetId, VolumeAndFee], scripts: Map[BigInt, Option[Script]]): Unit = ???

  override protected def loadScore(): BigInt = roOpt(q.score).getOrElse(BigInt(0))

  override protected def loadHeight(): Int = roOpt(q.height).flatten.getOrElse(0)

  override protected def loadPortfolio(address: Address): Portfolio = ???

  override protected def loadAssetInfo(assetId: ByteStr): Option[AssetInfo] = roOpt(q.assetInfo, assetId.arr)

  override protected def loadAssetDescription(assetId: AssetId): Option[AssetDescription] = roOpt(q.assetDescription, assetId.arr)

  override protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee = roOpt(q.volumeAndFee, orderId.arr).getOrElse(VolumeAndFee.empty)

  override protected def loadLastBlock = roOpt(q.lastBlockBytes).map(bytes => Block.parseBytes(bytes).get)

  override protected def loadScript(address: Address): Option[Script] = ???

  override protected def loadApprovedFeatures(): Map[Short, Int] = ???

  override protected def loadActivatedFeatures(): Map[Short, Int] = ???

  override def scoreOf(blockId: ByteStr) = ???

  override def blockHeaderAndSize(height: Int) = ???

  override def blockHeaderAndSize(blockId: ByteStr) = ???

  override def blockBytes(height: Int) = roOpt(q.blockBytesByHeight, height)

  override def blockBytes(blockId: ByteStr) = roOpt(q.blockBytesById, blockId)

  private val approvedFeaturesQuery = {
    val switchHeight = fs.doubleFeaturesPeriodsAfterHeight
    val initialInterval = fs.featureCheckBlocksPeriod
    val targetVoteCount = fs.blocksForFeatureActivation
    SQL(s"""with approval_heights as (
           |    select feature_id, count(height) vote_count,
           |    case when height > $switchHeight then ((height - 1) / ${initialInterval * 2} + 1) * ${initialInterval * 2}
           |    else ((height - 1) / $initialInterval + 1) * $initialInterval end target_height
           |    from feature_votes
           |    group by feature_id, target_height)
           |select feature_id, min(target_height) from approval_heights
           |where vote_count >= case when target_height > $switchHeight then ${targetVoteCount * 2} else $targetVoteCount end
           |group by feature_id""".stripMargin)
  }

  private def loadApprovedFeatures()(implicit s: DBSession) =
    approvedFeaturesQuery
      .map(rs => (rs.get[Short](1), rs.get[Int](2)))
      .list()
      .apply()
      .toMap

  override def featureVotes(height: Int) = readOnly { implicit s =>
    val windowSize = fs.featureCheckBlocksPeriod * (if (height >= fs.doubleFeaturesPeriodsAfterHeight) 2 else 1)
    val windowStart = (height - 1) / windowSize * windowSize + 1
    sql"select feature_id, count(height) from feature_votes where height between ? and ? group by feature_id"
      .bind(windowStart, windowStart + windowSize - 1)
      .map(rs => rs.get[Short](1) -> rs.get[Int](2))
      .list()
      .apply()
      .toMap
  }

  override def heightOf(blockId: ByteStr) = readOnly { implicit s =>
    sql"select height from blocks where block_id = ?"
      .bind(blockId.arr)
      .map(_.get[Int](1))
      .single()
      .apply()
  }

  override def lastBlockIds(howMany: Int) = readOnly { implicit s =>
    sql"select block_id from blocks order by height desc limit ?"
      .bind(howMany)
      .map(_.get[ByteStr](1))
      .list()
      .apply()
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int) = ???

  override def parent(childId: ByteStr, back: Int) = ???

  override def transactionInfo(id: ByteStr) = ???

  override def addressTransactions(address: Address,
                                   types: Set[TransactionParser.TransactionType.Value],
                                   from: Int, count: Int) = readOnly { implicit s =>
    ???
  }

  override def resolveAlias(a: Alias) = roOpt(q.addressOfAlias, a.bytes.arr)

  override def activeLeases = ???

  override def leaseDetails(leaseId: ByteStr) = readOnly { implicit s =>
    sql"""with this_lease_status as (
         |select lease_id, min(active) active from lease_status where lease_id = ? group by lease_id)
         |select li.sender_public_key, li.recipient, li.height, li.amount, tl.active
         |from lease_info li, this_lease_status tl
         |where li.lease_id = tl.lease_id""".stripMargin
      .bind(leaseId.arr)
      .map(rs => LeaseDetails(
        rs.get[PublicKeyAccount](1),
        rs.get(2)(addressOrAlias),
        rs.get[Int](3),
        rs.get[Long](4),
        rs.get[Boolean](5)))
      .single()
      .apply()
  }

  def balanceSnapshots(acc: Address, from: Int, to: Int) = {
    val bottomLimit = from
    val queryParams: Seq[Any] = Seq(bottomLimit, acc.stringRepr, acc.stringRepr, bottomLimit + 1, to)
    readOnly { implicit s =>
      sql"""with
           |earliest_balance as (
           |  select height, regular_balance from waves_balances where height <= ? and address = ? order by height desc limit 1
           |),
           |affected_balances as (
           |  select height, regular_balance from waves_balances where address = ? and height between ? and ?
           |  union all select * from  earliest_balance
           |),
           |earliest_lease as (
           |  select height, lease_in, lease_out from lease_balances where height <= ? and address = ? order by height desc limit 1
           |),
           |affected_leases as (
           |  select height, lease_in, lease_out from lease_balances where address = ? and height between ? and ?
           |  union all select * from earliest_lease
           |)
           |select coalesce(ab.height, al.height) height, ifnull(ab.regular_balance, 0), ifnull(al.lease_in, 0), ifnull(al.lease_out, 0)
           |from affected_balances ab left join affected_leases al using(height)
           |union
           |select coalesce(ab.height, al.height) height, ifnull(ab.regular_balance, 0), ifnull(al.lease_in, 0), ifnull(al.lease_out, 0)
           |from affected_leases al left join affected_balances ab using(height)
           |order by height asc""".stripMargin
        .bind(queryParams ++ queryParams: _*)
        .map(rs => BalanceSnapshot(rs.get[Int](1), rs.get[Long](2), rs.get[Long](3), rs.get[Long](4)))
        .list()
        .apply()
        .scanLeft(BalanceSnapshot(0, 0, 0, 0)) { (prevB, thisB) =>
          BalanceSnapshot(thisB.height,
            if (thisB.regularBalance == 0) prevB.regularBalance else thisB.regularBalance,
            if (thisB.leaseIn == 0) prevB.leaseIn else thisB.leaseIn,
            if (thisB.leaseOut == 0) prevB.leaseOut else thisB.leaseOut
          )
        }
    }
  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[Block] = using(DB(ds.getConnection))(_.localTx { implicit s =>
    val targetHeight = sql"select height from blocks where block_id = ?"
      .bind(targetBlockId.arr)
      .map(_.get[Int](1))
      .single()
      .apply()

    val blocks = targetHeight.fold(Seq.empty[Block]) { h =>
      val recoveredTransactions = sql"select block_data_bytes from blocks where height > ?"
        .bind(h)
        .map(_.get[Array[Byte]](1))
        .list()
        .apply()
        .map(Block.parseBytes(_).get)

      sql"delete from blocks where height > ?"
        .bind(h)
        .update()
        .apply()

      recoveredTransactions
    }

    blocks
  })

  def doAppend(block: Block,
                        wavesBalances: Map[Address, Long],
                        assetBalances: Map[Address, Map[ByteStr, Long]],
                        leaseBalances: Map[Address, LeaseBalance],
                        leaseStates: Map[ByteStr, Boolean],
                        transactions: Map[ByteStr, (Transaction, Set[Address])],
                        reissuedAssets: Map[ByteStr, AssetInfo],
                        filledQuantity: Map[ByteStr, VolumeAndFee]): Unit = {
    using(DB(ds.getConnection)) { db =>
      db.localTx { implicit s =>
        storeBlock(block)
        storeTransactions(transactions)
        storeIssuedAssets(transactions.collect { case (_, (it: IssueTransaction, _)) => it }.toSeq)
        storeReissuedAssets(reissuedAssets)
        storeFilledQuantity(filledQuantity)

        storeLeaseInfo(transactions.collect { case (_, (lt: LeaseTransaction, _)) => lt}.toSeq)
        sql"insert into lease_status (lease_id, active, height) values (?,?,?)"
          .batch(leaseStates.map { case (id, active) => Seq(id.arr, active, height) }.toSeq: _*)
          .apply()

        storeWavesBalances(wavesBalances)
        storeAssetBalances(assetBalances)
        storeLeaseBalances(leaseBalances)

        sql"insert into aliases (alias, address, height) values (?,?,?)"
          .batch(transactions.collect {
            case (_, (cat: CreateAliasTransaction, _)) => Seq(cat.alias.bytes.arr, cat.sender.toAddress.bytes.arr)
          }.toSeq: _*)
          .apply()

        storeAddressTransactionIds(transactions)

        cleanup()
      }
    }
  }

  private def cleanup()(implicit session: DBSession): Unit = if (height % 2000 == 0) {
    sql"analyze".update().apply()

    val heightParams = height - 2000

    sql"""with latest_safe_balances as (select address, max(height) height from waves_balances where height <= ?),
         |balances_to_remove as (
         |    select wb.address, wb.height from latest_safe_balances lsb
         |    inner join waves_balances wb using (address) where wb.height < lsb.height)
         |delete from waves_balances where (address, height) in balances_to_remove""".stripMargin
      .bind(heightParams)
      .update()
      .apply()

    sql"""with latest_safe_balances as (select address, asset_id, max(height) height from asset_balances where height <= ?),
         |balances_to_remove as (
         |    select ab.address, ab.asset_id, ab.height
         |    from latest_safe_balances lsb
         |    inner join asset_balances ab using (address, asset_id) where ab.height < lsb.height
         |)
         |delete from asset_balances where (address, asset_id, height) in balances_to_remove""".stripMargin
      .bind(heightParams)
      .update()
      .apply()

    sql"""with latest_safe_fills as (select order_id, max(height) height from filled_quantity where height <= ?),
         |fills_to_remove as (
         |    select fq.order_id, fq.height
         |    from latest_safe_fills lsf
         |    inner join filled_quantity fq using (order_id) where fq.height < lsf.height
         |)
         |delete from filled_quantity where (order_id, height) in fills_to_remove""".stripMargin
      .bind(heightParams)
      .update()
      .apply()
  }


  private def storeWavesBalances(wavesBalances: Map[Address, Long])(implicit session: DBSession): Unit = {
    sql"insert into waves_balances (address, regular_balance, height) values(?, ?, ?)"
      .batch((for {
        (address, balance) <- wavesBalances
      } yield Seq(address.bytes.arr, balance, height)).toSeq: _*)
      .apply()
  }

  private def storeAssetBalances(assetBalances: Map[Address, Map[ByteStr, Long]])(implicit session: DBSession): Unit = {
    val assetBalanceParams = (for {
      (address, portfolio) <- assetBalances
      (assetId, balance) <- portfolio
    } yield Seq(address.bytes.arr, assetId.arr, balance, height): Seq[Any]).toSeq

    sql"insert into asset_balances (address, asset_id, balance, height) values (?,?,?,?)"
      .batch(assetBalanceParams: _*)
      .apply()
  }

  private def storeLeaseBalances(leaseBalances: Map[Address, LeaseBalance])(implicit session: DBSession): Unit = {
    val changedLeaseBalances = (for {
      (address, lb) <- leaseBalances
    } yield Seq(address.bytes.arr, lb.in, lb.out, height): Seq[Any]).toSeq

    sql"insert into lease_balances (address, lease_in, lease_out, height) values(?,?,?,?)"
      .batch(changedLeaseBalances: _*)
      .apply()
  }

  private def storeLeaseInfo(leaseTransactions: Seq[LeaseTransaction])(implicit session: DBSession) = {
    sql"insert into lease_info (lease_id, sender_public_key, sender_address, recipient, amount, height) values (?,?,?,?,?,?)"
      .batch(leaseTransactions.map { lt =>
        Seq(lt.id().arr, lt.sender.publicKey, lt.sender.toAddress.bytes.arr, lt.recipient.bytes.arr, lt.amount, height)
      }: _*)
      .apply()
  }

  private def storeAddressTransactionIds(transactions: Map[ByteStr, (Transaction, Set[Address])])
                                        (implicit session: DBSession) = {
    val params = (for {
      (id, (_, addresses)) <- transactions
      address <- addresses
    } yield Seq(address.bytes.arr, id.arr, height): Seq[Any]).toSeq

    sql"insert into address_transaction_ids (address, tx_id, height) values (?,?,?)".batch(params: _*).apply()
  }

  private def storeFilledQuantity(filledQuantity: Map[ByteStr, VolumeAndFee])(implicit session: DBSession) = {
    val params: Seq[Seq[Any]] = (for {
      (orderId, fill) <- filledQuantity
    } yield Seq(orderId.arr, fill.volume, fill.fee, height): Seq[Any]).toSeq

    sql"insert into filled_quantity(order_id, filled_quantity, fee, height) values (?,?,?,?)".stripMargin
      .batch(params: _*)
      .apply()
  }

  private def storeReissuedAssets(reissuedAssets: Map[ByteStr, AssetInfo])(implicit session: DBSession): Unit = {
    sql"insert into asset_quantity (asset_id, quantity_change, reissuable, height) values (?,?,?,?)"
      .batch(reissuedAssets.map { case (id, ai) => Seq(id.arr, ai.volume, ai.isReissuable, height) }.toSeq: _*)
      .apply()
  }

  private def storeIssuedAssets(issueTransactions: Seq[IssueTransaction])(implicit session: DBSession): Unit = {
    val issuedAssetParams: Seq[Seq[Any]] = issueTransactions.map { i =>
        Seq(i.assetId().arr, i.sender.publicKey, i.decimals, i.name, i.description, height): Seq[Any]
    }

    sql"insert into asset_info(asset_id, issuer, decimals, name, description, height) values (?,?,?,?,?,?)"
      .batch(issuedAssetParams: _*)
      .apply()
  }

  private def storeBlock(block: Block)(implicit session: DBSession): Unit = {
    sql"""insert into blocks (
         |height, block_id, reference, version, block_timestamp, generator_address, generator_public_key,
         |base_target, generation_signature, block_data_bytes, cumulative_score)
         |values (?,?,?,?,?,?,?,?,?,?,?)""".stripMargin
      .bind(height, block.uniqueId.arr, block.reference.arr, block.version, new Timestamp(block.timestamp),
        block.signerData.generator.toAddress.bytes.arr, block.signerData.generator.publicKey,
        block.consensusData.baseTarget, block.consensusData.generationSignature.arr, block.bytes(), score)
      .update()
      .apply()

    sql"insert into feature_votes (height, feature_id) values (?,?)"
      .batch(block.featureVotes.map(Seq(height, _)).toSeq: _*)
      .apply()
  }

  private def storeTransactions(transactions: Map[ByteStr, (Transaction, Set[Address])])(implicit session: DBSession): Unit = {
    val transactionParams = (for {
      (id, (tx, _)) <- transactions
    } yield Seq(id.arr, tx.transactionType.id, tx.bytes(), height): Seq[Any]).toSeq

    sql"insert or replace into transactions (tx_id, tx_type, tx_bytes, height) values (?,?,?,?)"
      .batch(transactionParams: _*)
      .apply()
  }
}
