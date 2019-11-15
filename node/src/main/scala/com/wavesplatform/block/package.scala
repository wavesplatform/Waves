package com.wavesplatform

import cats.syntax.either._
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.validation.Validators._
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.transaction.Transaction
import monix.eval.Coeval
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{CryptographicHash32, Digest32}

import scala.util.Try

package object block {

  private[block] implicit class BlockValidationOps(block: Block) {
    def validate: Validation[Block]                             = validateBlock(block)
    def validateToTry: Try[Block]                               = toTry(validateBlock(block))
    def validateGenesis(gs: GenesisSettings): Validation[Block] = validateGenesisBlock(block, gs)
  }

  private[block] implicit class MicroBlockValidationOps(microBlock: MicroBlock) {
    def validate: Validation[MicroBlock] = validateMicroBlock(microBlock)
    def validateToTry: Try[MicroBlock]   = toTry(validateMicroBlock(microBlock))
  }

  private[block] implicit class BlockSignOps(block: Block) {
    def sign(signer: PrivateKey): Block = block.copy(signature = crypto.sign(signer, block.bytesWithoutSignature()))
  }

  private[block] implicit class MicroBlockSignOps(microBlock: MicroBlock) {
    def sign(signer: PrivateKey): MicroBlock = microBlock.copy(signature = crypto.sign(signer, microBlock.bytesWithoutSignature()))
  }

  private[block] implicit class BlockMerkleOps(block: Block) {
    val merkleTree: Coeval[MerkleTree[Digest32]] = Coeval.evalOnce {
      MerkleTree(block.transactionData.map(_.merkleLeaf()))(FastHash)
    }
  }

  private[block] implicit class TransactionMerkleOps(transaction: Transaction) {
    val merkleLeaf: Coeval[LeafData] = Coeval.evalOnce {
      LeafData @@ PBTransactions.protobuf(transaction).toByteArray
    }
  }

  private[block] object FastHash extends CryptographicHash32 {
    override def hash(input: FastHash.Message): Digest32 = Digest32(com.wavesplatform.crypto.fastHash(input))
  }

  private def toTry[A](result: Validation[A]): Try[A] = result.leftMap(ge => new IllegalArgumentException(ge.err)).toTry
}
