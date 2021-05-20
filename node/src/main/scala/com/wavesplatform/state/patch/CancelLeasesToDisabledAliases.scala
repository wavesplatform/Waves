package com.wavesplatform.state.patch

import cats.instances.map._
import cats.syntax.semigroup._
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import play.api.libs.json.{Json, Reads}

case object CancelLeasesToDisabledAliases extends DiffPatchFactory {
  private case class CancelDetails(id: String, amount: Long, senderPublicKey: String, recipientAddress: String, recipientAlias: String, height: Int)
  private implicit val reads: Reads[CancelDetails] = Json.reads

  override def isDefinedAt(blockchain: Blockchain): Boolean =
    blockchain.featureActivationHeight(BlockchainFeatures.SynchronousCalls.id).contains(blockchain.height)

  override def apply(blockchain: Blockchain): Diff =
    readPatchData[Seq[CancelDetails]]()
      .map { cd =>
        val recipientAddress = Address.fromString(cd.recipientAddress).explicitGet()
        val senderPublicKey  = PublicKey(Base58.decode(cd.senderPublicKey))
        Diff(
          leaseState = Map(
            ByteStr(Base58.decode(cd.id)) -> LeaseDetails(
              senderPublicKey,
              Alias.fromString(cd.recipientAlias).explicitGet(),
              recipientAddress,
              cd.amount,
              LeaseDetails.Status.Cancelled(blockchain.height, ByteStr.empty),
              ByteStr(Base58.decode(cd.id)),
              cd.height
            )
          ),
          portfolios =
            Map(senderPublicKey.toAddress -> Portfolio(lease = LeaseBalance(0, -cd.amount))) |+|
              Map(recipientAddress        -> Portfolio(lease = LeaseBalance(-cd.amount, 0)))
        )
      }
      .reduceLeft(_ |+| _)
}
