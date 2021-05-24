package com.wavesplatform.state.patch

import cats.instances.map._
import cats.syntax.semigroup._
import com.wavesplatform.account.{Address, AddressScheme, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import play.api.libs.json.{Json, Reads}

case object CancelLeasesToDisabledAliases extends PatchDataLoader with DiffPatchFactory {
  private case class CancelDetails(id: String, amount: Long, senderPublicKey: String, recipientAddress: String, recipientAlias: String, height: Int)
  private implicit val reads: Reads[CancelDetails] = Json.reads

  override def isDefinedAt(blockchain: Blockchain): Boolean =
    AddressScheme.current.chainId == 'W' &&
      blockchain.featureActivationHeight(BlockchainFeatures.SynchronousCalls.id).contains(blockchain.height)

  lazy val patchData: Map[ByteStr, (LeaseDetails, Address)] = readPatchData[Seq[CancelDetails]]().map { cd =>
    ByteStr(Base58.decode(cd.id)) -> (LeaseDetails(
      PublicKey(Base58.decode(cd.senderPublicKey)),
      Alias.fromString(cd.recipientAlias).explicitGet(),
      cd.amount,
      LeaseDetails.Status.Cancelled(0, ByteStr.empty),
      ByteStr(Base58.decode(cd.id)),
      cd.height
    ) -> Address.fromString(cd.recipientAddress).explicitGet())
  }.toMap

  override def apply(blockchain: Blockchain): Diff =
    patchData
      .map {
        case (id, (ld, recipientAddress)) =>
          Diff(
            leaseState = Map(
              id -> ld.copy(status = LeaseDetails.Status.Cancelled(blockchain.height, ByteStr.empty))
            ),
            portfolios =
              Map(ld.sender.toAddress -> Portfolio(lease = LeaseBalance(0, -ld.amount))) |+|
                Map(recipientAddress  -> Portfolio(lease = LeaseBalance(-ld.amount, 0)))
          )
      }
      .reduceLeft(_ |+| _)
}
