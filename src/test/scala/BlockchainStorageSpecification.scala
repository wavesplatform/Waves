package scorex.test

import org.scalatest.FunSuite
import scorex.block.{GenesisBlock, Block}
import scorex.database.PrunableBlockchainStorage

class BlockchainStorageSpecification extends FunSuite {

  //todo: implement test
  test("genesis block save & find"){
    assert(PrunableBlockchainStorage.appendBlock(GenesisBlock).heightOf(GenesisBlock.signature).get == 1)
  }
}
