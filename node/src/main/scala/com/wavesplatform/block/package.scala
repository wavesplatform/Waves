package com.wavesplatform

import cats.syntax.either._
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.Block.{GenerationSignatureLength, GenerationVRFSignatureLength, MaxFeaturesInBlock, ProtoBlockVersion}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.Try

package object block {

  // format: off
  private[block] def validateBlock(b: Block): Either[GenericError, Block] =
    (for {
      _ <- Either.cond(b.header.reference.arr.length == crypto.SignatureLength, (), "Incorrect reference")
      genSigLength = if (b.header.version < ProtoBlockVersion) GenerationSignatureLength else GenerationVRFSignatureLength
      _ <- Either.cond(b.header.generationSignature.arr.length == genSigLength, (), "Incorrect generationSignature")
      _ <- Either.cond(b.header.generator.length == crypto.KeyLength, (), "Incorrect signer")
      _ <- Either.cond(b.header.version > 2 || b.header.featureVotes.isEmpty,(),s"Block version ${b.header.version} could not contain feature votes")
      _ <- Either.cond(b.header.featureVotes.size <= MaxFeaturesInBlock, (), s"Block could not contain more than $MaxFeaturesInBlock feature votes")
    } yield b).leftMap(GenericError(_))

  def validateGenesisBlock(block: Block, genesisSettings: GenesisSettings): Either[GenericError, Block] =
    for {
      // Common validation
      _ <- validateBlock(block)
      // Verify signature
      _ <- Either.cond(crypto.verify(block.signature, block.bytesWithoutSignature(), block.header.generator), (), GenericError("Passed genesis signature is not valid"))
      // Verify initial balance
      txsSum = block.transactionData.collect { case tx: GenesisTransaction => tx.amount }.reduce(Math.addExact(_: Long, _: Long))
      _ <- Either.cond(txsSum == genesisSettings.initialBalance, (), GenericError(s"Initial balance ${genesisSettings.initialBalance} did not match the distributions sum $txsSum"))
    } yield block
  // format: on

  private[block] implicit class ValidationOps[A](v: Either[ValidationError, A]) {
    def asTry: Try[A] = v.left.map(ve => new IllegalArgumentException(ve.toString)).toTry
  }

  private[block] implicit class BlockOps(block: Block) {

    def sign(signer: PrivateKey): Block =
      block.copy(signature = crypto.sign(signer, block.bytesWithoutSignature()))

    def validate: Either[GenericError, Block] = validateBlock(block)

    def validateToTry: Try[Block] = validateBlock(block).leftMap(ge => new IllegalArgumentException(ge.err)).toTry

    def validateGenesis(genesisSettings: GenesisSettings): Either[GenericError, Block] = validateGenesisBlock(block, genesisSettings)
  }
}
