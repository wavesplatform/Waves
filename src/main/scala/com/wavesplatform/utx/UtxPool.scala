package com.wavesplatform.utx

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction._

trait UtxPool extends AutoCloseable {
  self =>

  def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)]

  def removeAll(txs: Traversable[Transaction]): Unit

  def spendableBalance(addr: Address, assetId: Asset): Long

  def pessimisticPortfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint)

}
