package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.syntax.all._
import cats.kernel.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import play.api.libs.json.JsObject
import scorex.account.{Account, Alias}
import scorex.block.Block
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.state.{AccState, AddressString, Reasons}
import scorex.utils.ScorexLogging

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

class StateWriterAdapter(persisted: StateWriter with StateReader, settings: FunctionalitySettings, bc: History) extends State with ScorexLogging {



  private val MinInMemDiff = 100
  private val MaxInMemDiff = 200
  @volatile var inMemoryDiff: BlockDiff = {

    log.debug("Blockchain height: " + bc.height())
    log.debug("Persisted state height: " + persisted.height)

    val storedBlocks = bc.height()
    val statedBlocks = persisted.height
    if (statedBlocks > storedBlocks) {
      throw new IllegalArgumentException(s"storedBlocks = $storedBlocks, statedBlocks=$statedBlocks")
    } else if (statedBlocks == storedBlocks) {
      Monoid[BlockDiff].empty
    } else {
      log.debug("rebuilding diff...")
      val r = rebuildDiff(statedBlocks + 1, storedBlocks + 1)
      log.debug("diff rebuilt")
      r
    }
  }

  private def composite: StateReader = new CompositeStateReader(persisted, inMemoryDiff)

  private def rebuildDiff(from: Int, to: Int): BlockDiff =
    Range(Math.max(1, from), to).foldLeft(Monoid[BlockDiff].empty) { (diff, h) =>
      val block = bc.blockAt(h).get
      val blockDiff = BlockDiffer(settings)(new CompositeStateReader(persisted, diff), block).right.get
      Monoid[BlockDiff].combine(diff, blockDiff)
    }

  override def processBlock(block: Block): Try[State] = Try {
    val updatedInMemoryDiff =
      if (inMemoryDiff.heightDiff >= MaxInMemDiff) {
        val compositeHeight = composite.height
        val diffToBePersisted = rebuildDiff(persisted.height + 1, compositeHeight - MinInMemDiff + 1)
        persisted.applyBlockDiff(diffToBePersisted)
        rebuildDiff(compositeHeight - MinInMemDiff + 1, compositeHeight + 1)
      } else {
        inMemoryDiff
      }

    BlockDiffer(settings)(composite, block) match {
      case Right(blockDiff) =>
        bc.appendBlock(block).map(_ =>
          inMemoryDiff = Monoid[BlockDiff].combine(updatedInMemoryDiff, blockDiff))
        this
      case Left(m) =>
        throw new Exception(s"Block $block is not valid: $m")
    }
  }


  override def rollbackTo(height: Int): State = {
    if (height < persisted.height) {
      throw new IllegalArgumentException(s"cannot rollback to a block with height=$height, which is older than writer.height=${persisted.height}")
    } else {
      while (bc.height > height) {
        bc.discardBlock()
      }
      if (composite.height == height) {
      } else {
        inMemoryDiff = rebuildDiff(persisted.height + 1, height + 1)
      }
      this
    }
  }


  // legacy

  override def findPreviousExchangeTxs(order: Order): Set[ExchangeTransaction] =
      composite.findPreviousExchangeTxs(order)

  override def included(signature: Array[Byte]): Option[Int] = composite.transactionInfo(EqByteArray(signature)).map(_._1)

  override def findTransaction[T <: Transaction](signature: Array[Byte])(implicit ct: ClassTag[T]): Option[T]
  = composite.findTransaction(signature)

  override def accountTransactions(account: Account, limit: Int): Seq[_ <: Transaction] =
    composite.accountTransactionIds(account).flatMap(composite.transactionInfo).map(_._2)

  override def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction] = None // not needed

  override def balance(account: Account): Long = composite.accountPortfolio(account).balance

  override def assetBalance(account: AssetAcc): Long = {
    val accountPortfolio = composite.accountPortfolio(account.account)
    account.assetId match {
      case Some(assetId) => accountPortfolio.assets.getOrElse(EqByteArray(assetId), 0)
      case None => accountPortfolio.balance
    }
  }

  override def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] =
    composite.accountPortfolio(account).assets.map { case (id, amt) =>
      val assetInfo = composite.assetInfo(id).get
      id.arr -> (amt, assetInfo.isReissuable, assetInfo.volume, findTransaction[IssueTransaction](id.arr).get)
    }

  override def assetDistribution(assetId: Array[Byte]): Map[String, Long] =
    composite.assetDistribution(EqByteArray(assetId))
      .map { case (acc, amt) => (acc.address, amt) }

  override def effectiveBalance(account: Account): Long = composite.accountPortfolio(account).effectiveBalance

  override def getLeasedSum(address: AddressString): Long = {
    val portfolio = composite.accountPortfolio(Account.fromString(address).right.get)
    portfolio.effectiveBalance - portfolio.balance
  }

  override def isReissuable(id: Array[Byte]): Boolean =
    composite.assetInfo(EqByteArray(id)).get.isReissuable

  override def totalAssetQuantity(assetId: AssetId): Long =
    composite.assetInfo(EqByteArray(assetId)).get.volume

  override def balanceWithConfirmations(account: Account, confirmations: Int): Long = ???

  override def wavesDistributionAtHeight(height: Int): Seq[(AddressString, Long)] = ???

  override def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, height: Int): Long =
    composite.effectiveBalanceAtHeightWithConfirmations(account, height, confirmations)

  override def resolveAlias(a: Alias): Option[Account] = composite.resolveAlias(a)

  override def getAlias(a: Account): Option[Alias] = composite.aliasesOfAddress(a).headOption

  override def stateHeight: Int = composite.height

  override def toJson(heightOpt: Option[Int]): JsObject = ???

  override def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)], blockTs: Long): Unit = ???

  override def calcNewBalances(trans: Seq[Transaction], fees: Map[AssetAcc, (AccState, Reasons)], allowTemporaryNegative: Boolean): Map[AssetAcc, (AccState, Reasons)] = ???

  override def addAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long, reissuable: Boolean): Unit = ???

  override def burnAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long): Unit = ???

  override def assetRollbackTo(assetId: Array[Byte], height: Int, newReissuable: Option[Boolean] = None): Unit = ???

}
