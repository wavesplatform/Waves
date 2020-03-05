package com.wavesplatform.block.validation

import cats.syntax.either._
import com.wavesplatform.block.Block.{GenerationSignatureLength, GenerationVRFSignatureLength, MaxFeaturesInBlock, ProtoBlockVersion}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.crypto
import com.wavesplatform.crypto.KeyLength
import com.wavesplatform.mining.Miner.MaxTransactionsPerMicroblock
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

object Validators {
  type Validation[A] = Either[GenericError, A]

  def validateBlock(b: Block): Validation[Block] =
    (for {
      _ <- Either.cond(Block.validateReferenceLength(b.header.version, b.header.reference.length), (), "Incorrect reference")
      genSigLength = if (b.header.version < ProtoBlockVersion) GenerationSignatureLength else GenerationVRFSignatureLength
      _ <- Either.cond(b.header.generationSignature.arr.length == genSigLength, (), "Incorrect generationSignature")
      _ <- Either.cond(b.header.generator.length == KeyLength, (), "Incorrect signer")
      _ <- Either.cond(
        b.header.version > 2 || b.header.featureVotes.isEmpty,
        (),
        s"Block version ${b.header.version} could not contain feature votes"
      )
      _ <- Either.cond(b.header.featureVotes.distinct.size == b.header.featureVotes.size, (), s"Duplicates in feature votes")
      _ <- Either.cond(b.header.featureVotes.size <= MaxFeaturesInBlock, (), s"Block could not contain more than $MaxFeaturesInBlock feature votes")
    } yield b).leftMap(GenericError(_))

  def validateGenesisBlock(block: Block, genesisSettings: GenesisSettings): Validation[Block] =
    for {
      // Common validation
      _ <- validateBlock(block)
      // Verify signature
      _ <- Either.cond(
        crypto.verify(block.signature, block.bytesWithoutSignature(), block.header.generator),
        (),
        GenericError("Passed genesis signature is not valid")
      )
      // Verify initial balance
      txsSum = block.transactionData.collect { case tx: GenesisTransaction => tx.amount }.reduce(Math.addExact(_: Long, _: Long))
      _ <- Either.cond(
        txsSum == genesisSettings.initialBalance,
        (),
        GenericError(s"Initial balance ${genesisSettings.initialBalance} did not match the distributions sum $txsSum")
      )
    } yield block

  def validateMicroBlock(mb: MicroBlock): Validation[MicroBlock] =
    (for {
      _ <- Either.cond(
        Block.validateReferenceLength(mb.version, mb.prevResBlockRef.length),
        (),
        s"Incorrect prevResBlockSig: ${mb.prevResBlockRef.length}"
      )
      _ <- Either.cond(
        Block.validateReferenceLength(mb.version, mb.totalResBlockRef.length),
        (),
        s"Incorrect totalResBlockSig: ${mb.totalResBlockRef.length}"
      )
      _ <- Either.cond(mb.sender.length == KeyLength, (), s"Incorrect generator.publicKey: ${mb.sender.length}")
      _ <- Either.cond(mb.transactionData.nonEmpty, (), "cannot create empty MicroBlock")
      _ <- Either.cond(
        mb.transactionData.size <= MaxTransactionsPerMicroblock,
        (),
        s"too many txs in MicroBlock: allowed: $MaxTransactionsPerMicroblock, actual: ${mb.transactionData.size}"
      )
    } yield mb).leftMap(GenericError(_))
}
