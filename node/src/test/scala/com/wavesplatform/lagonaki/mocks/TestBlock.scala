package com.wavesplatform.lagonaki.mocks

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.*
import com.wavesplatform.transaction.Transaction

import scala.util.{Random, Try}

object TestBlock {
  val defaultSigner: KeyPair = KeyPair(ByteStr(new Array[Byte](KeyLength)))

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
        rewardVote = b.header.rewardVote,
        stateHash = b.header.stateHash,
        challengedHeader = b.header.challengedHeader
      )

    x.explicitGet()
  }

  def create(txs: Seq[Transaction]): Block = create(defaultSigner, txs)

  def create(txs: Seq[Transaction], version: Byte): Block =
    create(time = Try(txs.map(_.timestamp).max).getOrElse(0), ref = randomSignature(), txs = txs, version = version)

  def create(signer: KeyPair, txs: Seq[Transaction]): Block =
    create(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs, signer = signer)

  def create(signer: KeyPair, txs: Seq[Transaction], features: Seq[Short]): Block =
    create(time = Try(txs.map(_.timestamp).max).getOrElse(0), ref = randomSignature(), txs = txs, signer = signer, version = 3, features = features)

  def create(time: Long, txs: Seq[Transaction]): Block = create(time, randomSignature(), txs, defaultSigner)

  def create(time: Long, txs: Seq[Transaction], signer: KeyPair): Block = create(time, randomSignature(), txs, signer)

  def create(
      time: Long,
      ref: ByteStr,
      txs: Seq[Transaction],
      signer: KeyPair = defaultSigner,
      version: Byte = 2,
      features: Seq[Short] = Seq.empty[Short],
      rewardVote: Long = -1L,
      stateHash: Option[ByteStr] = None,
      baseTarget: Long = 2L,
      challengedHeader: Option[ChallengedHeader] = None
  ): Block =
    sign(
      signer,
      Block.create(
        timestamp = time,
        version = version,
        reference = ref,
        baseTarget = baseTarget,
        generationSignature =
          if (version < Block.ProtoBlockVersion) ByteStr(Array.fill(Block.GenerationSignatureLength)(0: Byte))
          else ByteStr(Array.fill(Block.GenerationVRFSignatureLength)(0: Byte)),
        generator = signer.publicKey,
        featureVotes = features,
        rewardVote = rewardVote,
        transactionData = txs,
        stateHash = stateHash,
        challengedHeader = challengedHeader
      )
    )

  def withReference(ref: ByteStr): Block =
    sign(
      defaultSigner,
      Block(
        BlockHeader(
          1.toByte,
          0,
          ref,
          2L,
          randomOfLength(Block.GenerationSignatureLength),
          defaultSigner.publicKey,
          Seq.empty,
          -1L,
          ByteStr.empty,
          None,
          None
        ),
        ByteStr.empty,
        Seq.empty
      )
    )

  def withReferenceAndFeatures(ref: ByteStr, features: Seq[Short]): Block =
    sign(
      defaultSigner,
      Block.create(
        3.toByte,
        0,
        ref,
        2L,
        randomOfLength(Block.GenerationSignatureLength),
        defaultSigner.publicKey,
        features,
        -1L,
        Seq.empty,
        None,
        None
      )
    )
}
