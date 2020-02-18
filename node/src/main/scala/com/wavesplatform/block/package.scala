package com.wavesplatform

import cats.syntax.either._
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.merkle.Merkle.TransactionProof
import com.wavesplatform.block.validation.Validators._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.Transaction

import scala.util.Try

package object block {

  // Validation
  private[block] implicit class BlockValidationOps(block: Block) {
    def validate: Validation[Block]                             = validateBlock(block)
    def validateToTry: Try[Block]                               = toTry(validateBlock(block))
    def validateGenesis(gs: GenesisSettings): Validation[Block] = validateGenesisBlock(block, gs)
  }

  private[block] implicit class MicroBlockValidationOps(microBlock: MicroBlock) {
    def validate: Validation[MicroBlock] = validateMicroBlock(microBlock)
    def validateToTry: Try[MicroBlock]   = toTry(validateMicroBlock(microBlock))
  }

  private def toTry[A](result: Validation[A]): Try[A] = result.leftMap(ge => new IllegalArgumentException(ge.err)).toTry

  // Sign
  private[block] implicit class BlockSignOps(block: Block) {
    def sign(signer: PrivateKey): Block = block.copy(signature = crypto.sign(signer, ByteStr(block.bytesWithoutSignature())))
  }

  private[block] implicit class MicroBlockSignOps(microBlock: MicroBlock) {
    def sign(signer: PrivateKey): MicroBlock = microBlock.copy(signature = crypto.sign(signer, ByteStr(microBlock.bytesWithoutSignature())))
  }

  // Merkle
  implicit class BlockTransactionsRootOps(block: Block) {

    def transactionProof(transaction: Transaction): Option[TransactionProof] =
      block.transactionsMerkleTree().transactionProof(transaction)

    def verifyTransactionProof(transactionProof: TransactionProof): Boolean =
      block.transactionData
        .lift(transactionProof.transactionIndex)
        .filter(tx => tx.id() == transactionProof.id)
        .exists(tx => transactionProof.valid(tx, block.transactionData.size, block.header.transactionsRoot))
  }
}
