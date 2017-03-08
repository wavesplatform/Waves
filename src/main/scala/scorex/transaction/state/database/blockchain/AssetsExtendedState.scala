package scorex.transaction.state.database.blockchain

import com.google.common.base.Charsets
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction._
import scorex.transaction.assets.{AssetIssuance, BurnTransaction, IssueTransaction, ReissueTransaction}
import scorex.transaction.state.database.state.extension.{Processor, Validator}
import scorex.transaction.state.database.state.storage.{AssetsExtendedStateStorageI, StateStorageI}
import scorex.utils.ScorexLogging

class AssetsExtendedState(storage: StateStorageI with AssetsExtendedStateStorageI) extends ScorexLogging
  with Validator with Processor {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = {

    def isIssuerAddress(assetId: Array[Byte], tx: SignedTransaction): Either[StateValidationError, SignedTransaction] = {
      storage.getTransaction(assetId) match {
        case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
        case Some(it: IssueTransaction) =>
          if (it.sender.address == tx.sender.address) Right(tx)
          else Left(TransactionValidationError(tx, "Asset was issued by other address"))
        case _ => Left(TransactionValidationError(tx, "Referenced transaction is not IssueTransaction"))
      }
    }

    tx match {
      case tx: ReissueTransaction =>
        isIssuerAddress(tx.assetId, tx).flatMap(t =>
          if (isReissuable(tx.assetId)) Right(t) else Left(TransactionValidationError(tx, "Asset is not reissuable")))
      case tx: BurnTransaction =>
        isIssuerAddress(tx.assetId, tx)
      case _ => Right(tx)
    }
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = tx match {
    case tx: AssetIssuance =>
      addAsset(tx.assetId, height, tx.id, tx.quantity, tx.reissuable)
    case tx: BurnTransaction =>
      burnAsset(tx.assetId, height, tx.id, -tx.amount)
    case _ =>
  }

  private[blockchain] def addAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long, reissuable: Boolean): Unit = {
    val asset = Base58.encode(assetId)
    val transaction = Base58.encode(transactionId)
    val assetAtHeight = s"$asset@$height"
    val assetAtTransaction = s"$asset@$transaction"

    if (!isIssueExists(assetId) ||
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

  private[blockchain] def burnAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long): Unit = {
    require(quantity <= 0, "Quantity of burned asset should be negative")

    val asset = Base58.encode(assetId)
    val transaction = Base58.encode(transactionId)
    val assetAtHeight = s"$asset@$height"
    val assetAtTransaction = s"$asset@$transaction"

    storage.addHeight(asset, height)
    storage.addTransaction(assetAtHeight, transaction)
    storage.setQuantity(assetAtTransaction, quantity)
  }

  def rollbackTo(assetId: AssetId, height: Int): Unit = {
    val asset = Base58.encode(assetId)

    val heights = storage.getHeights(asset)
    val heightsToRemove = heights.filter(h => h > height)
    storage.setHeight(asset, heights -- heightsToRemove)

    val transactionsToRemove: Seq[String] = heightsToRemove.foldLeft(Seq.empty[String]) { (result, h) =>
      result ++ storage.getTransactions(s"$asset@$h")
    }

    val keysToRemove = transactionsToRemove.map(t => s"$asset@$t")

    keysToRemove.foreach { key =>
      storage.removeKey(key)
    }
  }

  def getAssetQuantity(assetId: AssetId): Long = {
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

  def isIssueExists(assetId: AssetId): Boolean = {
    storage.getHeights(Base58.encode(assetId)).nonEmpty
  }

  def getAssetName(assetId: AssetId): String = {
    storage.getTransaction(assetId).flatMap {
      case tx: IssueTransaction => Some(tx.asInstanceOf[IssueTransaction])
      case _ => None
    }.map(tx => new String(tx.name, Charsets.UTF_8)).getOrElse("Unknown")
  }
}
