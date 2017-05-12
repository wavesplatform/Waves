package scorex.transaction.state.database.blockchain

import com.google.common.base.Charsets
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction._
import scorex.transaction.assets.{AssetIssuance, BurnTransaction, IssueTransaction, ReissueTransaction}
import scorex.transaction.state.database.state.extension.Validator
import scorex.transaction.state.database.state.storage.{AssetsExtendedStateStorageI, StateStorageI}
import scorex.utils.ScorexLogging

//TODO move to state.extension package
class AssetsExtendedState(storage: StateStorageI with AssetsExtendedStateStorageI) extends ScorexLogging
  with Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case tx: ReissueTransaction =>
      isIssuerAddress(tx.assetId, tx).flatMap(t =>
        if (isReissuable(tx.assetId)) Right(t) else Left(StateValidationError("Asset is not reissuable")))
    case tx: BurnTransaction =>
      isIssuerAddress(tx.assetId, tx)
    case _ => Right(tx)
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = tx match {
    case tx: AssetIssuance =>
      addAsset(tx.assetId, height, tx.id, tx.quantity, tx.reissuable)
    case tx: BurnTransaction =>
      burnAsset(tx.assetId, height, tx.id, -tx.amount)
    case _ =>
  }

  override def validateWithBlockTxs(storedState: StoredState,
                                    tx: Transaction, blockTxs: Seq[Transaction], height: Int): Either[StateValidationError, Transaction] = Right(tx)

  private def isIssuerAddress(assetId: Array[Byte], tx: SignedTransaction): Either[StateValidationError, SignedTransaction] = {
    storage.getTransaction(assetId) match {
      case None => Left(StateValidationError("Referenced assetId not found"))
      case Some(it: IssueTransaction) =>
        if (it.sender.address == tx.sender.address) Right(tx)
        else Left(StateValidationError("Asset was issued by other address"))
      case _ => Left(StateValidationError("Referenced transaction is not IssueTransaction"))
    }
  }

  private[blockchain] def addAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long, reissuable: Boolean): Unit = {
    val asset = Base58.encode(assetId)
    val transaction = Base58.encode(transactionId)
    val assetAtHeight = s"$asset@$height"
    val assetAtTransaction = s"$asset@$transaction"

    storage.setReissuable(asset, reissuable)
    storage.addHeight(asset, height)
    storage.addTransaction(assetAtHeight, transaction)
    storage.setQuantity(assetAtTransaction, quantity)
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

  def deleteAtHeight(burn: BurnTransaction, height: Int): Unit = {
    removeAtAndAfterHeight(burn.assetId, height)
  }

  def deleteAtHeight(issuance: AssetIssuance, height: Int): Unit = {
    val asset = Base58.encode(issuance.assetId)
    removeAtAndAfterHeight(issuance.assetId, height, Some(true))
  }

  private[blockchain] def removeAtAndAfterHeight(assetId: Array[Byte], height: Int, newReissuable: Option[Boolean] = None): Unit = {
    val asset = Base58.encode(assetId)

    val heights = storage.getHeights(asset)
    val heightsToRemove = heights.filter(h => h >= height)
    storage.setHeight(asset, heights -- heightsToRemove)

    val transactionsToRemove: Seq[String] = heightsToRemove.foldLeft(Seq.empty[String]) { (result, h) =>
      result ++ storage.getIssuanceTransactionsIds(s"$asset@$h")
    }

    newReissuable.foreach(newValue => storage.setReissuable(asset, newValue))

    val keysToRemove = transactionsToRemove.map(t => s"$asset@$t")

    keysToRemove.foreach { key =>
      storage.removeQuantities(key)
    }
  }

  def getAssetQuantity(assetId: AssetId): Long = {
    val asset = Base58.encode(assetId)
    val heights = storage.getHeights(asset)

    val sortedHeights = heights.toSeq.sorted
    val transactions: Seq[String] = sortedHeights.foldLeft(Seq.empty[String]) { (result, h) =>
      result ++ storage.getIssuanceTransactionsIds(s"$asset@$h")
    }

    transactions.foldLeft(0L) { (result, transaction) =>
      result + storage.getQuantity(s"$asset@$transaction")
    }
  }

  def isReissuable(assetId: AssetId): Boolean = {
    storage.isReissuable(Base58.encode(assetId))
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
