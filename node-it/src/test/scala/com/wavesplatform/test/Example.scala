package com.wavesplatform.test

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.test.BlockchainGenerator.{GenBlock, GenTx}
import com.wavesplatform.transaction.TxHelpers

object Example extends App {

  // function argument is path to node config file
  val generator = new BlockchainGenerator("/Users/imashonskii/node-data-devnet/waves-devnet.conf")

  // create transaction sender
  val sender = KeyPair("123".getBytes)
  // create transfer transaction recipient
  val recipient = Address.fromString("3FddHK1Y3vPdcVKZshWCWea4gS5th6G1UE6").getOrElse(sender.toAddress)

  // create blocks to generate in blockchain
  val genBlocks = (1 to 10).map { idx =>
    GenBlock(
      // block transactions = seq of GenTx objects
      (1 to 5).map(txIdx => GenTx(TxHelpers.transfer(sender, recipient, amount = (idx * 10 + txIdx) * 100000000L), Right(sender))),
      // block signer
      signer = sender
    )
  }

  // generate node database in data directory + create binary file for Importer with name blockchain-<height>
  generator.generateBinaryFile(genBlocks)

  // generate only node database in data directory
  // generator.generateDb(genBlocks)
}
