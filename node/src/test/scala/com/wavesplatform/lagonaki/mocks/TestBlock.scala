package com.wavesplatform.lagonaki.mocks

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.Transaction

import scala.util.{Random, Try}

object TestBlock {
  val defaultSigner = KeyPair(Array.fill(KeyLength)(0: Byte))

  val random: Random = new Random()

  def randomOfLength(length: Int): ByteStr = ByteStr(Array.fill(length)(random.nextInt().toByte))

  def randomSignature(): ByteStr = randomOfLength(SignatureLength)

  def sign(signer: KeyPair, b: Block): Block = {
    val x = Block
      .buildAndSign(
        version = b.header.version,
        timestamp = b.header.timestamp,
        reference = b.header.reference,
        baseTarget = b.header.baseTarget,
        generationSignature = b.header.generationSignature,
        txs = b.transactionData,
        signer = signer,
        featureVotes = b.header.featureVotes,
        rewardVote = b.header.rewardVote
      )

      x.explicitGet()
  }

  def create(txs: Seq[Transaction]): Block = create(defaultSigner, txs)

  def create(signer: KeyPair, txs: Seq[Transaction]): Block =
    create(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs, signer = signer)

  def create(signer: KeyPair, txs: Seq[Transaction], features: Set[Short]): Block =
    create(time = Try(txs.map(_.timestamp).max).getOrElse(0), ref = randomSignature(), txs = txs, signer = signer, version = 3, features = features)

  def create(time: Long, txs: Seq[Transaction]): Block = create(time, randomSignature(), txs, defaultSigner)

  def create(time: Long, txs: Seq[Transaction], signer: KeyPair): Block = create(time, randomSignature(), txs, signer)

  def create(
      time: Long,
      ref: ByteStr,
      txs: Seq[Transaction],
      signer: KeyPair = defaultSigner,
      version: TxVersion = 2,
      features: Set[Short] = Set.empty[Short],
      rewardVote: Long = -1L
  ): Block =
    sign(
      signer,
      Block(
        BlockHeader(
          timestamp = time,
          version = version,
          reference = ref,
          baseTarget = 2L,
          generationSignature = ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte)),
          generator = signer,
          featureVotes = features,
          rewardVote = rewardVote
        ),
        transactionData = txs,
        signature = ByteStr.empty
      )
    )

  def withReference(ref: ByteStr): Block =
    sign(
      defaultSigner,
      Block(
        BlockHeader(
          1,
          0,
          ref,
          2L,
          randomOfLength(Block.GeneratorSignatureLength),
          defaultSigner,
          Set.empty,
          -1L
        ),
        ByteStr.empty,
        Seq.empty
      )
    )

  def withReferenceAndFeatures(ref: ByteStr, features: Set[Short]): Block =
    sign(
      defaultSigner,
      Block(
        BlockHeader(
          3,
          0,
          ref,
          2L,
          randomOfLength(Block.GeneratorSignatureLength),
          defaultSigner,
          features,
          -1L
        ),
        ByteStr.empty,
        Seq.empty
      )
    )
}
