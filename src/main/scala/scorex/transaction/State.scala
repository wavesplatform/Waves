package scorex.transaction

import play.api.libs.json.JsObject
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.state.database.blockchain.AssetsExtendedState
import scorex.transaction.state.database.state.{AccState, Reasons}
import scorex.transaction.state.database.state.extension.{IncrementingTimestampValidator, OrderMatchStoredState}
import scorex.utils.NTP

import scala.util.Try

trait State {

  // validation

  def isValid(tx: Transaction, blockTime: Long): Boolean = isValid(Seq(tx), blockTime = blockTime)

  def isValid(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Boolean = validate(txs, height, blockTime).size == txs.size

  def validate(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Seq[Transaction]

  // state reads

  def included(signature: Array[Byte]): Option[Int]

  def balance(account: Account, height: Option[Int] = None): Long

  def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int] = None): Long

  def accountTransactions(account: Account, limit: Int = State.DefaultLimit): Seq[_ <: Transaction]

  def assetBalance(account: AssetAcc, atHeight: Option[Int] = None): Long

  def assetDistribution(assetId: Array[Byte]): Map[String, Long]

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)]

  // exposing 'extensions'

  def orderMatchStoredState: OrderMatchStoredState

  def assetsExtension: AssetsExtendedState

  def incrementingTimestampValidator : IncrementingTimestampValidator

  // debug from api

  def toWavesJson(heightOpt: Int): JsObject

  def toJson(heightOpt: Option[Int] = None): JsObject

  def hash: Int

  def stateHeight: Int

  // state writes

  def processBlock(block: Block): Try[State]

  def rollbackTo(height: Int): State

  // calls from tests only

  def validateAgainstState(transaction: Transaction, height: Int): Either[ValidationError, Transaction]

  def applyChanges(changes: Map[AssetAcc, (AccState, Reasons)], blockTs: Long = NTP.correctedTime()): Unit

  def calcNewBalances(trans: Seq[Transaction], fees: Map[AssetAcc, (AccState, Reasons)],
                      allowTemporaryNegative: Boolean): Map[AssetAcc, (AccState, Reasons)]

  def totalAssetQuantity(assetId: AssetId): Long

  def totalBalance: Long
}

object State {
  private val DefaultLimit = 50
}
