package com.wavesplatform.it.sync.network

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.it.api.AsyncNetworkApi.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.network.{RawBytes, TransactionSpec}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

class SimpleTransactionsSuite extends BaseTransactionSuite {
  import com.wavesplatform.it.NodeConfigs.*
  override val nodeConfigs: Seq[Config] = Seq(BiggestMiner.quorum(0))

  private def node = nodes.head

  test("valid tx send by network to node should be in blockchain") {
    val tx = TxHelpers.transfer(node.keyPair, Address.fromString(node.address).explicitGet(), 1L, Waves, minFee, Waves)

    node.sendByNetwork(RawBytes.fromTransaction(tx))
    node.waitForTransaction(tx.id().toString)

  }

  test("invalid tx send by network to node should be not in UTX or blockchain") {
    val tx = TxHelpers.transfer(
      node.keyPair,
      Address.fromString(node.address).explicitGet(),
      1L,
      Waves,
      minFee,
      Waves,
      timestamp = System.currentTimeMillis() + (1 days).toMillis
    )

    node.sendByNetwork(RawBytes.fromTransaction(tx))
    val maxHeight = nodes.map(_.height).max
    nodes.waitForHeight(maxHeight + 1)
    node.ensureTxDoesntExist(tx.id().toString)
  }

  test("should blacklist senders of non-parsable transactions") {
    val blacklistBefore = node.blacklistedPeers
    node.sendByNetwork(RawBytes(TransactionSpec.messageCode, "foobar".getBytes(StandardCharsets.UTF_8)))
    node.waitForBlackList(blacklistBefore.size)
  }
}
