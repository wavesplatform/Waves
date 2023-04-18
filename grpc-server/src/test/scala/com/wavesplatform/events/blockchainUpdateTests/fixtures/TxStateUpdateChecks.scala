package com.wavesplatform.events.blockchainUpdateTests.fixtures

import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.lang.v1.traits.Environment.AssetId
import org.scalatest.matchers.should.Matchers

case class TxStateUpdateChecks(append: Append, index: Int = 0) extends Matchers {
  val txStateUpdates: Seq[StateUpdate] = append.transactionStateUpdates

  def balancesCheckers(bIndex: Int, address: Address, before: Long, after: Long, assetId: AssetId): Unit = {
    val balances = txStateUpdates.apply(index).balances.apply(bIndex)

    balances.address.toByteArray shouldBe address
    balances.amountBefore shouldEqual before
    balances.amountAfter.get.amount shouldEqual after
    balances.amountAfter.get.assetId shouldBe assetId
  }
}
