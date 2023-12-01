package com.wavesplatform.state.patch

import cats.implicits.{catsSyntaxSemigroup, toFoldableOps}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{Blockchain, LeaseBalance, LeaseDetails, Portfolio, StateSnapshot}
import play.api.libs.json.{Json, Reads}

case object CancelLeasesToDisabledAliases extends PatchOnFeature(BlockchainFeatures.SynchronousCalls, Set('W')) {
  private[this] case class CancelDetails(
      id: String,
      amount: Long,
      senderPublicKey: String,
      recipientAddress: String,
      recipientAlias: String,
      height: Int
  )

  def patchData: Map[ByteStr, (Map[Address, Portfolio], Address)] = {
    implicit val cancelDetailsReads: Reads[CancelDetails] = Json.reads

    readPatchData[Seq[CancelDetails]]().map { cancelDetails =>
      val leaseId          = ByteStr(Base58.decode(cancelDetails.id))
      val sender           = PublicKey(Base58.decode(cancelDetails.senderPublicKey))
      val recipientAddress = Address.fromString(cancelDetails.recipientAddress).explicitGet()
      leaseId -> (Portfolio
        .combine(
          Map(sender.toAddress -> Portfolio(lease = LeaseBalance(0, -cancelDetails.amount))),
          Map(recipientAddress -> Portfolio(lease = LeaseBalance(-cancelDetails.amount, 0)))
        )
        .explicitGet(),
      recipientAddress)
    }.toMap
  }

  override def apply(blockchain: Blockchain): StateSnapshot = {
    val (leaseBalances, leaseStates) =
      patchData.toSeq.map { case (id, (pf, _)) =>
        (
          pf,
          StateSnapshot(
            cancelledLeases = Map(id -> LeaseDetails.Status.Expired(blockchain.height))
          )
        )
      }.unzip
    val combinedLeaseBalances = leaseBalances.reduce(Portfolio.combine(_, _).explicitGet())
    val leaseBalancesSnapshot = StateSnapshot.ofLeaseBalances(combinedLeaseBalances.view.mapValues(_.lease).toMap, blockchain)
    leaseBalancesSnapshot.explicitGet() |+| leaseStates.combineAll
  }
}
