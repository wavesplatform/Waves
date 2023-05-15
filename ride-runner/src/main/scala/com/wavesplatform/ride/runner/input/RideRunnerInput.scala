package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.Address
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.state.{BalanceSnapshot, LeaseBalance}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import play.api.libs.json.*

case class RideRunnerInput(
    address: Address,
    request: JsObject,
    trace: Boolean = false,
    accounts: Map[Address, RunnerAccountState] = Map.empty,
    height: Int = 3296626,
    extraFeatures: Set[Short] = Set.empty,
    assets: Map[IssuedAsset, RunnerAssetInfo] = Map.empty,
    blocks: Map[Int, RunnerBlockInfo] = Map.empty,
    transactions: Map[ByteStr, RunnerTransactionInfo] = Map.empty
) {
  lazy val accountScript: Map[Address, RunnerScriptInfo] = for {
    (addr, state) <- accounts
    scriptInfo    <- state.scriptInfo
  } yield addr -> scriptInfo

  lazy val accountData: Map[Address, Map[String, DataEntry]] = for {
    (addr, state) <- accounts
    data          <- state.data
  } yield addr -> data.map { case (key, entry) => key -> entry.toDataEntry(key) }

  lazy val hasData: Map[Address, Boolean] = accountStateLens(_.data.nonEmpty)

  lazy val balance: Map[Address, Map[Asset, Long]] = accountStateLens(_.balance)

  lazy val balanceSnapshots: Map[Address, Seq[BalanceSnapshot]] = for {
    (addr, state) <- accounts
  } yield {
    val generatingBalance = state.generatingBalance.orElse(state.balance.get(Waves)).getOrElse(0L)
    addr -> Seq(BalanceSnapshot(height, generatingBalance, 0, 0))
  }

  lazy val leaseBalance: Map[Address, LeaseBalance] = for {
    (addr, state) <- accounts
    lease         <- state.leasing
  } yield addr -> LeaseBalance(lease.in, lease.out)

  private def accountStateLens[T](f: RunnerAccountState => T): Map[Address, T] = for {
    (addr, state) <- accounts
  } yield addr -> f(state)

  lazy val blockHeader: Map[Int, SignedBlockHeader] = for {
    (height, blockInfo) <- blocks
  } yield height -> SignedBlockHeader(
    header = BlockHeader(
      version = 5,
      timestamp = blockInfo.timestamp,
      reference = ByteStr(Array.emptyByteArray),
      baseTarget = blockInfo.baseTarget,
      generationSignature = blockInfo.generationSignature,
      generator = blockInfo.generatorPublicKey,
      featureVotes = Nil,
      rewardVote = -1,
      transactionsRoot = ByteStr(Array.emptyByteArray)
    ),
    signature = ByteStr(Array.emptyByteArray)
  )

  lazy val hitSource: Map[Int, ByteStr] = for {
    (height, blockInfo) <- blocks
    vrf                 <- blockInfo.VRF
  } yield height -> vrf
}
