package com.wavesplatform.database

import com.google.common.primitives.Shorts

object KeyTags extends Enumeration {
  type KeyTag = Value
  val Version,
  Height,
  HeightOf,
  WavesBalance,
  WavesBalanceHistory,
  AssetBalanceHistory,
  AssetBalance,
  AssetDetailsHistory,
  AssetDetails,
  LeaseBalanceHistory,
  LeaseBalance,
  LeaseDetailsHistory,
  LeaseDetails,
  FilledVolumeAndFeeHistory,
  FilledVolumeAndFee,
  ChangedAddresses,
  ChangedAssetBalances,
  ChangedDataKeys,
  AddressIdOfAlias,
  LastAddressId,
  AddressId,
  IdToAddress,
  AddressScriptHistory,
  AddressScript,
  ApprovedFeatures,
  ActivatedFeatures,
  DataHistory,
  Data,
  SponsorshipHistory,
  Sponsorship,
  CarryFee,
  AssetScriptHistory,
  AssetScript,
  SafeRollbackHeight,
  BlockInfoAtHeight,
  NthTransactionInfoAtHeight,
  AddressTransactionSeqNr,
  AddressTransactionHeightTypeAndNums,
  TransactionMetaById,
  InvokeScriptResult,
  DisabledAliases,
  AssetStaticInfo,
  NftCount,
  NftPossession,
  IssuedAssets,
  UpdatedAssets,
  SponsoredAssets,
  StateHash,
  EthereumTransactionMeta,
  NthTransactionStateSnapshotAtHeight,
  MaliciousMinerBanHeights,
  BlockStateHash,
  AddressLeaseInfoSeqNr,
  AddressLeaseInfoSeq,
  LastCleanupHeight = Value

  final implicit class KeyTagExt(val t: KeyTag) extends AnyVal {
    @inline def prefixBytes: Array[Byte] = Shorts.toByteArray(t.id.toShort)
  }
}
