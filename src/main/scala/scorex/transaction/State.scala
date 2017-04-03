package scorex.transaction

import com.google.common.base.Charsets
import play.api.libs.json.JsObject
import scorex.account.{Account, Alias}
import scorex.block.Block
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.state.{AccState, AddressString, Reasons}
import scorex.utils.NTP

import scala.reflect.ClassTag
import scala.util.Try

trait State {

  // state reads

  def included(signature: Array[Byte]): Option[Int]

  def balance(account: Account): Long

  def balanceWithConfirmations(account: Account, confirmations: Int): Long

  def accountTransactions(account: Account, limit: Int = State.DefaultLimit): Seq[_ <: Transaction]

  def assetBalance(account: AssetAcc): Long

  def assetDistribution(assetId: Array[Byte]): Map[String, Long]

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)]

  def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, height: Int): Long

  def findPrevOrderMatchTxs(order: Order): Set[ExchangeTransaction]

  def resolveAlias(a: Alias): Option[Account]

  def getAlias(a: Account): Option[Alias]

  def findTransaction[T <: Transaction](signature: Array[Byte])(implicit ct: ClassTag[T]): Option[T]

  def isReissuable(id: Array[Byte]): Boolean

  def getLeasedSum(address: AddressString): Long

  def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction]

  def effectiveBalance(account: Account): Long

  def stateHeight: Int

  def wavesDistributionAtHeight(height: Int): JsObject

  def totalAssetQuantity(assetId: AssetId): Long

  // debug from api

  def toJson(heightOpt: Option[Int]): JsObject

  def hash: Int

  // state writes

  def processBlock(block: Block): Try[State]

  def rollbackTo(height: Int): State

  // outside calls from tests only

  def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)], blockTs: Long = NTP.correctedTime()): Unit

  def calcNewBalances(trans: Seq[Transaction], fees: Map[AssetAcc, (AccState, Reasons)],
                      allowTemporaryNegative: Boolean): Map[AssetAcc, (AccState, Reasons)]

  def addAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long, reissuable: Boolean): Unit

  def burnAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long): Unit

  def assetRollbackTo(assetId: Array[Byte], height: Int, newReissuable: Option[Boolean] = None) : Unit
}

object State {
  private val DefaultLimit = 50

  implicit class StateExt(s: State) {
    def findPrevOrderMatchTxs(om: ExchangeTransaction): Set[ExchangeTransaction] = {
      s.findPrevOrderMatchTxs(om.buyOrder) ++ s.findPrevOrderMatchTxs(om.sellOrder)
    }

    def getAssetName(assetId: AssetId): String = {
      s.findTransaction[IssueTransaction](assetId)
        .map(tx => new String(tx.name, Charsets.UTF_8))
        .getOrElse("Unknown")
    }
  }

}
