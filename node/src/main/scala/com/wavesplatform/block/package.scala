package com.wavesplatform

import cats.syntax.either._
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.validation.Validators._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.Transaction
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.hash.{CryptographicHash32, Digest32}

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
  private[block] implicit object FastHash extends CryptographicHash32 { // todo: (NODE-1972) Replace with appropriate hash function
    override def hash(input: Message): Digest32 = Digest32(com.wavesplatform.crypto.fastHash(input))
  }

  private[block] val EmptyMerkleTree: MerkleTree[Digest32] = MerkleTree(Seq(LeafData @@ Array.emptyByteArray))

  implicit class BlockMerkleOps(block: Block) {
    def transactionProof(transaction: Transaction): Option[MerkleProof[Digest32]] =
      block.transactionsMerkleTree().proofByElement(transaction.mkMerkleLeaf())
  }

  private[block] implicit class TransactionMerkleOps(transaction: Transaction) {
    def mkMerkleLeaf(): Leaf[Digest32] = Leaf(LeafData @@ PBTransactions.protobuf(transaction).toByteArray)
  }

  /** Creates transactions merkle root */
  private[block] def mkMerkleTree(transactions: Seq[Transaction]): MerkleTree[Digest32] = {
    if (transactions.isEmpty) EmptyMerkleTree else MerkleTree(transactions.map(_.mkMerkleLeaf().data))
  }
}
