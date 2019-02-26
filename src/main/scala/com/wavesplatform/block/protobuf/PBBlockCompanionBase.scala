package com.wavesplatform.block.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.protobuf.PBTransaction

trait PBBlockCompanionBase extends PBBlockImplicits {
  def create(reference: com.wavesplatform.common.state.ByteStr = ByteStr.empty,
             baseTarget: _root_.scala.Long = 0L,
             generationSignature: com.wavesplatform.common.state.ByteStr = ByteStr.empty,
             featureVotes: scala.collection.immutable.Set[_root_.scala.Int] = scala.collection.immutable.Set.empty,
             timestamp: _root_.scala.Long = 0L,
             version: _root_.scala.Int = 0,
             generator: com.wavesplatform.account.PublicKeyAccount = PublicKeyAccount.empty,
             signature: com.wavesplatform.common.state.ByteStr = ByteStr.empty,
             transactions: Seq[PBTransaction] = Nil) = {
    new PBBlock(PBBlock.Header(reference, baseTarget, generationSignature, featureVotes, timestamp, version, generator, signature), transactions)
  }

  def unapply(block: PBBlock): Option[(ByteStr, Long, ByteStr, Set[Int], Long, Int, PublicKeyAccount, ByteStr, Seq[PBTransaction])] = {
    Some(
      (block.header.reference,
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
