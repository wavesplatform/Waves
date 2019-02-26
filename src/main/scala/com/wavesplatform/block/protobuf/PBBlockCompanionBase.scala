package com.wavesplatform.block.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.protobuf.{ChainId, PBSignedTransaction}

trait PBBlockCompanionBase extends PBBlockImplicits {
  def create(chainId: ChainId = ChainId.empty,
             reference: com.wavesplatform.common.state.ByteStr = ByteStr.empty,
             baseTarget: Long = 0L,
             generationSignature: com.wavesplatform.common.state.ByteStr = ByteStr.empty,
             featureVotes: Set[Int] = scala.collection.immutable.Set.empty,
             timestamp: Long = 0L,
             version: Int = 0,
             generator: com.wavesplatform.account.PublicKeyAccount = PublicKeyAccount.empty,
             signature: com.wavesplatform.common.state.ByteStr = ByteStr.empty,
             transactions: Seq[PBSignedTransaction] = Nil) = {
    new PBBlock(
      chainId,
      PBBlock.SignedHeader(PBBlock.Header(reference, baseTarget, generationSignature, featureVotes, timestamp, version, generator), signature),
      transactions
    )
  }

  def unapply(block: PBBlock): Option[(ChainId, ByteStr, Long, ByteStr, Set[Int], Long, Int, PublicKeyAccount, ByteStr, Seq[PBSignedTransaction])] = {
    Some(
      (block.chainId,
       block.header.reference,
       block.header.baseTarget,
       block.header.generationSignature,
       block.header.featureVotes,
       block.header.timestamp,
       block.header.version,
       block.header.generator,
       block.header.signature,
       block.transactions))
  }
}
