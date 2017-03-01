package scorex.transaction

import play.api.libs.json.JsObject
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.blockchain.{AssetsExtendedState, LeaseExtendedState}
import scorex.transaction.state.database.state.{AccState, Reasons}
import scorex.transaction.state.database.state.extension.{IncrementingTimestampValidator, OrderMatchStoredState}
import scorex.utils.NTP

import scala.util.Try

trait State {

  // validation

  def validate(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): (Seq[ValidationError], Seq[Transaction])

  // state reads

  def included(signature: Array[Byte]): Option[Int]

  def balance(account: Account, height: Option[Int] = None): Long

  def balanceWithConfirmations(account: Account, confirmations: Int): Long

  def accountTransactions(account: Account, limit: Int = State.DefaultLimit): Seq[_ <: Transaction]

  def assetBalance(account: AssetAcc): Long

  def assetDistribution(assetId: Array[Byte]): Map[String, Long]

  def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)]

  def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int] = None): Long

  def findPrevOrderMatchTxs(order: Order): Set[ExchangeTransaction]

  def getAssetQuantity(assetId: AssetId): Long

  def getAssetName(assetId: AssetId): String


  // debug from api

  def toWavesJson(heightOpt: Int): JsObject

  def toJson(heightOpt: Option[Int] = None): JsObject

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

  def incrementingTimestampValidator: IncrementingTimestampValidator

  def leaseExtendedState: LeaseExtendedState

}

object State {
  private val DefaultLimit = 50

  implicit class StateExt(s: State) {

    // validation

    def validate[T <: Transaction](tx: T, blockTime: Long): Either[ValidationError, T] = s.validate(Seq(tx), None, blockTime) match {
      case (_, Seq(t)) => Right(t.asInstanceOf[T])
      case (Seq(err), _) => Left(err)
    }

    // calls from test only

    def isValid(tx: Transaction, blockTime: Long): Boolean = validate(tx, blockTime).isRight

    def isValid(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Boolean = s.validate(txs, height, blockTime)._2.size == txs.size
  }

}
