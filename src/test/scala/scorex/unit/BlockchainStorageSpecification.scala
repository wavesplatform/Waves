package scorex.unit

import org.scalatest.FunSuite
import scorex.block.GenesisBlock
import scorex.database.blockchain.PrunableBlockchainStorage

class BlockchainStorageSpecification extends FunSuite {

  test("genesis block save & find") {
    //todo:fix test
    //assert(PrunableBlockchainStorage.appendBlock(GenesisBlock).heightOf(GenesisBlock.signature).get == 1)
  }

}
