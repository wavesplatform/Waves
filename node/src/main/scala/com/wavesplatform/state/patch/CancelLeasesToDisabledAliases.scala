package com.wavesplatform.state.patch

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio, StateSnapshot}
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

  def patchData: Map[ByteStr, (LeaseDetails, Address)] = {
    implicit val cancelDetailsReads: Reads[CancelDetails] = Json.reads

    readPatchData[Seq[CancelDetails]]().map { cancelDetails =>
      val leaseId          = ByteStr(Base58.decode(cancelDetails.id))
      val sender           = PublicKey(Base58.decode(cancelDetails.senderPublicKey))
      val recipientAlias   = Alias.fromString(cancelDetails.recipientAlias).explicitGet()
      val recipientAddress = Address.fromString(cancelDetails.recipientAddress).explicitGet()
      leaseId -> (LeaseDetails(
        sender,
        recipientAlias,
        cancelDetails.amount,
        LeaseDetails.Status.Expired(0),
        leaseId,
        cancelDetails.height
      ) -> recipientAddress)
    }.toMap
  }

  import cats.implicits.*

  override def apply(blockchain: Blockchain): StateSnapshot = {
    val snapshots = patchData
      .map { case (id, (ld, recipientAddress)) =>
        val leaseBalances =
          Diff
            .combine(
              Map(ld.sender.toAddress -> Portfolio(lease = LeaseBalance(0, -ld.amount))),
              Map(recipientAddress    -> Portfolio(lease = LeaseBalance(-ld.amount, 0)))
            )
            .getOrElse(Map.empty)
            .view
            .mapValues(_.lease)
            .toMap
        val balancesSnapshot = StateSnapshot.ofLeaseBalances(leaseBalances, blockchain)
        balancesSnapshot |+| StateSnapshot(
          aliases = ld.recipient match {
            case alias: Alias => Map(alias -> recipientAddress)
            case _            => Map()
          },
          leaseStates = Map(
            id -> ld.copy(status = LeaseDetails.Status.Expired(blockchain.height))
          )
        )
      }
    Monoid.combineAll(snapshots)
  }
}
