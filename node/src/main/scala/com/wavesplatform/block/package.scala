package com.wavesplatform

import cats.syntax.either._
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.Block.{GenerationSignatureLength, GenerationVRFSignatureLength, MaxFeaturesInBlock, ProtoBlockVersion}
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.mining.Miner.MaxTransactionsPerMicroblock
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.Try

package object block {
  type Validation[A] = Either[GenericError, A]

  private[block] def validateBlock(b: Block): Validation[Block] =
    (for {
      _ <- Either.cond(b.header.reference.arr.length == SignatureLength, (), "Incorrect reference")
      genSigLength = if (b.header.version < ProtoBlockVersion) GenerationSignatureLength else GenerationVRFSignatureLength
      _ <- Either.cond(b.header.generationSignature.arr.length == genSigLength, (), "Incorrect generationSignature")
      _ <- Either.cond(b.header.generator.length == KeyLength, (), "Incorrect signer")
      _ <- Either.cond(b.header.version > 2 || b.header.featureVotes.isEmpty,(),s"Block version ${b.header.version} could not contain feature votes")
      _ <- Either.cond(b.header.featureVotes.size <= MaxFeaturesInBlock, (), s"Block could not contain more than $MaxFeaturesInBlock feature votes")
    } yield b).leftMap(GenericError(_))

  def validateGenesisBlock(block: Block, genesisSettings: GenesisSettings): Validation[Block] =
    for {
      // Common validation
      _ <- validateBlock(block)
      // Verify signature
      _ <- Either.cond(crypto.verify(block.signature, block.bytesWithoutSignature(), block.header.generator), (), GenericError("Passed genesis signature is not valid"))
      // Verify initial balance
      txsSum = block.transactionData.collect { case tx: GenesisTransaction => tx.amount }.reduce(Math.addExact(_: Long, _: Long))
      _ <- Either.cond(txsSum == genesisSettings.initialBalance, (), GenericError(s"Initial balance ${genesisSettings.initialBalance} did not match the distributions sum $txsSum"))
    } yield block

  def validateMicroBlock(mb: MicroBlock): Validation[MicroBlock] =
    (for {
      _ <- Either.cond(mb.prevResBlockSig.arr.length == SignatureLength, (), s"Incorrect prevResBlockSig: ${mb.prevResBlockSig.arr.length}")
      _ <- Either.cond(mb.totalResBlockSig.arr.length == SignatureLength, (), s"Incorrect totalResBlockSig: ${mb.totalResBlockSig.arr.length}")
      _ <- Either.cond(mb.sender.length == KeyLength, (), s"Incorrect generator.publicKey: ${mb.sender.length}")
      _ <- Either.cond(mb.transactionData.nonEmpty, (), "cannot create empty MicroBlock")
      _ <- Either.cond(mb.transactionData.size <= MaxTransactionsPerMicroblock, (), s"too many txs in MicroBlock: allowed: $MaxTransactionsPerMicroblock, actual: ${mb.transactionData.size}")
    } yield mb).leftMap(GenericError(_))

  private[block] implicit class BlockOps(block: Block) {
    def sign(signer: PrivateKey): Block                         = block.copy(signature = crypto.sign(signer, block.bytesWithoutSignature()))
    def validate: Validation[Block]                             = validateBlock(block)
    def validateToTry: Try[Block]                               = toTry(validateBlock(block))
    def validateGenesis(gs: GenesisSettings): Validation[Block] = validateGenesisBlock(block, gs)
  }

  private[block] implicit class MicroBlockOps(microBlock: MicroBlock) {
    def sign(signer: PrivateKey): MicroBlock = microBlock.copy(signature = crypto.sign(signer, microBlock.bytesWithoutSignature()))
    def validate: Validation[MicroBlock]     = validateMicroBlock(microBlock)
    def validateToTry: Try[MicroBlock]       = toTry(validateMicroBlock(microBlock))
  }

  private def toTry[A](result: Validation[A]): Try[A] = result.leftMap(ge => new IllegalArgumentException(ge.err)).toTry
}
