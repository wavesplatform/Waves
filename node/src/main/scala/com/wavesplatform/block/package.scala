package com.wavesplatform

import cats.syntax.either._
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.validation.Validators._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.Transaction
import monix.eval.Coeval
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.hash.{CryptographicHash32, Digest32}
import supertagged._

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
    private[block] val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce {
      if (block.header.version < Block.ProtoBlockVersion) block.copy(signature = ByteStr.empty).bytes()
      else PBBlocks.protobuf(block).header.get.toByteArray
    }

    def sign(signer: PrivateKey): Block = block.copy(signature = crypto.sign(signer, block.bytesWithoutSignature()))
  }

  private[block] implicit class MicroBlockSignOps(microBlock: MicroBlock) {
    private[block] val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(microBlock.copy(signature = ByteStr.empty).bytes())

    def sign(signer: PrivateKey): MicroBlock = microBlock.copy(signature = crypto.sign(signer, microBlock.bytesWithoutSignature()))
  }

  // Merkle
  private[block] implicit object FastHash extends CryptographicHash32 { // todo: (NODE-1972) Replace with appropriate hash function
    override def hash(input: Message): Digest32 = Digest32(com.wavesplatform.crypto.fastHash(input))
  }

  private[block] val EmptyMerkleTree: MerkleTree[Digest32] = MerkleTree(Seq(LeafData @@ Array.emptyByteArray))(FastHash)

  private[block] implicit class BlockMerkleOps(block: Block) {
    val merkleTree: Coeval[MerkleTree[Digest32]] = Coeval.evalOnce(mkMerkleTree(block.header.version, block.transactionData))

    val merkleRootValid: Coeval[Boolean] = Coeval.evalOnce {
      block.header.version < Block.ProtoBlockVersion || ((merkleTree().rootHash untag Digest32) sameElements block.header.merkle.arr)
    }

    def merkleProof(transaction: Transaction): Option[MerkleProof[Digest32]] = merkleTree().proofByElement(transaction.merkleLeaf())
  }

  private[block] implicit class TransactionMerkleOps(transaction: Transaction) {
    val merkleLeaf: Coeval[Leaf[Digest32]] = Coeval.evalOnce(Leaf(LeafData @@ PBTransactions.protobuf(transaction).toByteArray))
  }

  private[block] def mkMerkleTree(version: Byte, transactions: Seq[Transaction]): MerkleTree[Digest32] = {
    require(version >= Block.ProtoBlockVersion, s"Merkle should be used only for Block version >= ${Block.ProtoBlockVersion}")
    if (transactions.isEmpty) EmptyMerkleTree else MerkleTree(transactions.map(_.merkleLeaf().data))
  }
}
