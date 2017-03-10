package scorex.transaction

import com.google.common.base.Charsets
import play.api.libs.json.JsObject
import scorex.account.{Account, Alias}
import scorex.block.Block
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.state.{AccState, AddressString, Reasons}
import scorex.utils.NTP

import scala.reflect.ClassTag
import scala.util.Try

trait State {

  // tx lookup

  def included(signature: Array[Byte]): Option[Int]

  def findTransaction[T <: Transaction](signature: Array[Byte])(implicit ct: ClassTag[T]): Option[T]


  // tx history

  def accountTransactions(account: Account, limit: Int = State.DefaultLimit): Seq[_ <: Transaction]

  def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction] // = accountTransactions...

  // account balance and effective balance

  def balance(account: Account): Long

  def assetBalance(account: AssetAcc): Long

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)]

  def assetDistribution(assetId: Array[Byte]): Map[String, Long]

  def effectiveBalance(account: Account): Long

  def getLeasedSum(address: AddressString): Long // = balance - effectiveBalance

  // asset info
  def isReissuable(id: Array[Byte]): Boolean

  def totalAssetQuantity(assetId: AssetId): Long

  // height-related queries backed by Map((acc,height) -> (balance, eff_balance)).
  // Which gets filled only on change
  // Cleaned up sometimes

  def balanceWithConfirmations(account: Account, confirmations: Int): Long

  def wavesDistributionAtHeight(height: Int): JsObject

  def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, height: Int): Long

  // other

  // Cleaned up sometimes
  def findPrevOrderMatchTxs(order: Order): Set[ExchangeTransaction]

  def resolveAlias(a: Alias): Option[Account]

  def getAlias(a: Account): Option[Alias]

  def stateHeight: Int

  def toJson(heightOpt: Option[Int]): JsObject

  // state writes

  def processBlock(block: Block): Try[State]

  def rollbackTo(height: Int): State

  // outside calls from tests only


  // StoredStateUnitTests: 42
  def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)], blockTs: Long = NTP.correctedTime()): Unit

  // StoredStateUnitTests: 36
  def calcNewBalances(trans: Seq[Transaction], fees: Map[AssetAcc, (AccState, Reasons)],
                      allowTemporaryNegative: Boolean): Map[AssetAcc, (AccState, Reasons)]

  // AssetExtendedStateSpecification: 31
  def addAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long, reissuable: Boolean): Unit

  // AssetExtendedStateSpecification: 1
  def burnAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long): Unit

  // AssetExtendedStateSpecification: 5
  def assetRollbackTo(assetId: AssetId, height: Int): Unit
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

    def hash: Int = {
      (BigInt(FastCryptographicHash(s.toJson(None).toString().getBytes)) % Int.MaxValue).toInt
    }

  }

}
