package com.wavesplatform.events

import com.wavesplatform.TestValues.fee
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.api.grpc.protobuf.GetBlockUpdatesRangeRequest
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class BlockchainUpdatesGetBlockUpdatesRangeSpec extends BlockchainUpdatesTestBase {
  "BlockchainUpdates getBlockUpdateRange tests" - {
    "BU- . Return correct data for alias" in {
      val aliasTx = TxHelpers.createAlias("test", firstTxParticipant, fee = customFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 1),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendBlock(aliasTx)) { getBlockUpdateRange =>
        val append = getBlockUpdateRange(0).getAppend
        checkingAlias(append, aliasTx)
      }
    }
  }
}
