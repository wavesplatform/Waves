package com.wavesplatform.state.diffs.invoke
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset

case class StructuredCallableActions(list: List[CallableAction], blockchain: Blockchain) {
  private val actionsByType =
    list
      .groupBy(a => if (classOf[DataOp].isAssignableFrom(a.getClass)) classOf[DataOp] else a.getClass)
      .withDefaultValue(Nil)

  val transferList    = actionsByType(classOf[AssetTransfer]).asInstanceOf[List[AssetTransfer]]
  val issueList       = actionsByType(classOf[Issue]).asInstanceOf[List[Issue]]
  val reissueList     = actionsByType(classOf[Reissue]).asInstanceOf[List[Reissue]]
  val burnList        = actionsByType(classOf[Burn]).asInstanceOf[List[Burn]]
  val sponsorFeeList  = actionsByType(classOf[SponsorFee]).asInstanceOf[List[SponsorFee]]
  val leaseList       = actionsByType(classOf[Lease]).asInstanceOf[List[Lease]]
  val leaseCancelList = actionsByType(classOf[LeaseCancel]).asInstanceOf[List[LeaseCancel]]
  val dataEntries     = actionsByType(classOf[DataOp]).asInstanceOf[List[DataOp]].map(InvokeDiffsCommon.dataItemToEntry)

  private val assets =
    transferList.flatMap(_.assetId).map(IssuedAsset(_)) ++
      reissueList.map(r => IssuedAsset(r.assetId)) ++
      burnList.map(b => IssuedAsset(b.assetId)) ++
      sponsorFeeList.map(sf => IssuedAsset(sf.assetId))

  val complexities = assets.flatMap(blockchain.assetScript(_).map(_.complexity))
}
