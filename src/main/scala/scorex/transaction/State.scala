package scorex.transaction

import play.api.libs.json.JsObject
import scorex.account.{Account, Alias}
import scorex.block.Block
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.state.{AccState, AddressString, Reasons}
import scorex.transaction.state.database.state.extension.ExchangeTransactionValidator
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

  def findPrevOrderMatchTxs(om: ExchangeTransaction): Set[ExchangeTransaction]

  def getAssetName(assetId: AssetId): String

  def resolveAlias(a: Alias): Option[Account]

  def getAlias(a: Account): Option[Alias]

  def persistAlias(ac: Account, al: Alias): Unit

  def findTransaction[T <: Transaction](signature: Array[Byte])(implicit ct: ClassTag[T]): Option[T]

  def isReissuable(id: Array[Byte]): Boolean


  // debug from api

  def toWavesJson(height: Int): JsObject

  def toJson(heightOpt: Option[Int]): JsObject

  def hash: Int

  def stateHeight: Int

  // state writes

  def processBlock(block: Block): Try[State]

  def rollbackTo(height: Int): State

  // outside calls from tests only

  def validateAgainstState(transaction: Transaction, height: Int): Either[ValidationError, Transaction]

  def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)], blockTs: Long = NTP.correctedTime()): Unit

  def calcNewBalances(trans: Seq[Transaction], fees: Map[AssetAcc, (AccState, Reasons)],
                      allowTemporaryNegative: Boolean): Map[AssetAcc, (AccState, Reasons)]

  def totalAssetQuantity(assetId: AssetId): Long

  def totalBalance: Long

  def effectiveBalance(account: Account): Long

  def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction]

  def getLeasedSum(address: AddressString): Long

  def effectiveBalanceWithConfirmations(account: Account, confirmations: Int): Long

}

object State {
  private val DefaultLimit = 50
}
