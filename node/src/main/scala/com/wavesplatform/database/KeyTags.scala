package com.wavesplatform.database

import com.google.common.primitives.Shorts

object KeyTags extends Enumeration {
  type KeyTag = Value
  val Version,
  Height,
  Score,
  HeightOf,
  WavesBalanceHistory,
  WavesBalance,
  AssetBalanceHistory,
  AssetBalance,
  AssetDetailsHistory,
  AssetDetails,
  LeaseBalanceHistory,
  LeaseBalance,
  LeaseStatusHistory,
  LeaseStatus,
  FilledVolumeAndFeeHistory,
  FilledVolumeAndFee,
  ChangedAddresses,
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
  ChangedDataKeys,
  BlockInfoAtHeight,
  NthTransactionInfoAtHeight,
  AddressTransactionSeqNr,
  AddressTransactionHeightTypeAndNums,
  TransactionHeightAndNumsById,
  BlockTransactionsFee,
  InvokeScriptResult,
  BlockReward,
  WavesAmount,
  HitSource,
  DisabledAliases,
  AssetStaticInfo,
  NftCount,
  NftPossession,
  BloomFilterChecksum,
  IssuedAssets,
  UpdatedAssets,
  SponsorshipAssets = Value

  final implicit class KeyTagExt(val t: KeyTag) extends AnyVal {
    @inline def prefixBytes: Array[Byte] = Shorts.toByteArray(t.id.toShort)
  }
}
