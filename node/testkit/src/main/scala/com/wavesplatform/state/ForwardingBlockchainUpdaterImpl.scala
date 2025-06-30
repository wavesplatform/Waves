package com.wavesplatform.state

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.transaction.BlockchainUpdater

class ForwardingBlockchainUpdaterImpl(delegate: CompleteBlockchainUpdater) extends Blockchain with BlockchainUpdater with NG {
  export delegate.{
    settings,
    height,
    score,
    blockHeader,
    hitSource,
    carryFee,
    heightOf,
    approvedFeatures,
    activatedFeatures,
    featureVotes,
    blockReward,
    blockRewardVotes,
    wavesAmount,
    transferById,
    transactionInfo,
    transactionInfos,
    transactionMeta,
    transactionSnapshot,
    containsTransaction,
    assetDescription,
    resolveAlias,
    leaseDetails,
    filledVolumeAndFee,
    balanceAtHeight,
    balanceSnapshots,
    accountScript,
    hasAccountScript,
    assetScript,
    accountData,
    hasData,
    leaseBalance,
    leaseBalances,
    balance,
    balances,
    wavesBalances,
    effectiveBalanceBanHeights,
    resolveERC20Address,
    lastStateHash,
    processBlock,
    processMicroBlock,
    computeNextReward,
    removeAfter,
    lastBlockInfo,
    isLastBlockId,
    referencedBlockchain,
    shutdown,
    microBlock,
    bestLastBlockInfo,
    microblockIds,
    liquidBlock,
    liquidBlockSnapshot,
    microBlockSnapshot,
    liquidTransactions,
    liquidBlockMeta,
    bestLiquidSnapshot,
    bestLiquidSnapshotAndFees,
    snapshotBlockchain
  }
}
