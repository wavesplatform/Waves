package com.wavesplatform.state.patch

import cats.implicits.{catsSyntaxAlternativeSeparate, catsSyntaxSemigroup, toFoldableOps}
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{Blockchain, LeaseBalance, LeaseSnapshot, Portfolio, StateSnapshot}
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

  def patchData: Map[ByteStr, (LeaseSnapshot, Address)] = {
    implicit val cancelDetailsReads: Reads[CancelDetails] = Json.reads

    readPatchData[Seq[CancelDetails]]().map { cancelDetails =>
      val leaseId          = ByteStr(Base58.decode(cancelDetails.id))
      val sender           = PublicKey(Base58.decode(cancelDetails.senderPublicKey))
      val recipientAlias   = Alias.fromString(cancelDetails.recipientAlias).explicitGet()
      val recipientAddress = Address.fromString(cancelDetails.recipientAddress).explicitGet()
      leaseId -> (LeaseSnapshot(
        sender,
        recipientAlias,
        cancelDetails.amount,
        LeaseDetails.Status.Expired(0)
      ) -> recipientAddress)
    }.toMap
  }

  override def apply(blockchain: Blockchain): StateSnapshot = {
    val (leaseBalances, leaseStates) =
      patchData.toSeq.map { case (id, (ld, recipientAddress)) =>
        (
          Portfolio
            .combine(
              Map(ld.sender.toAddress -> Portfolio(lease = LeaseBalance(0, -ld.amount))),
              Map(recipientAddress    -> Portfolio(lease = LeaseBalance(-ld.amount, 0)))
            )
            .explicitGet(),
          StateSnapshot(
            leaseStates = Map(id -> ld.copy(status = LeaseDetails.Status.Expired(blockchain.height)))
          )
        )
      }.separate
    val combinedLeaseBalances = leaseBalances.reduce(Portfolio.combine(_, _).explicitGet())
    val leaseBalancesSnapshot = StateSnapshot.ofLeaseBalances(combinedLeaseBalances.view.mapValues(_.lease).toMap, blockchain)
    leaseBalancesSnapshot.explicitGet() |+| leaseStates.combineAll
  }
}
