package com.wavesplatform.state.patch

import cats.instances.map._
import cats.syntax.semigroup._
import com.wavesplatform.account.{Address, AddressScheme, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.state.reader.LeaseDetails
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

  lazy val patchData: Map[ByteStr, (LeaseDetails, Address)] = {
    implicit val cancelDetailsReads: Reads[CancelDetails] = Json.reads
    if (AddressScheme.current.chainId == 'W') readPatchData[Seq[CancelDetails]]().map { cancelDetails =>
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
    }.toMap else Map.empty
  }

  override def apply(blockchain: Blockchain): Diff =
    patchData
      .map {
        case (id, (ld, recipientAddress)) =>
          Diff(
            leaseState = Map(
              id -> ld.copy(status = LeaseDetails.Status.Expired(blockchain.height))
            ),
            portfolios =
              Map(ld.sender.toAddress -> Portfolio(lease = LeaseBalance(0, -ld.amount))) |+|
                Map(recipientAddress  -> Portfolio(lease = LeaseBalance(-ld.amount, 0)))
          )
      }
      .reduceLeft(_ |+| _)
}
