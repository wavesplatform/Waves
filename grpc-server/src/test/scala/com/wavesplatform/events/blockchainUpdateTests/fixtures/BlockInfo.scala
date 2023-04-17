package com.wavesplatform.events.blockchainUpdateTests.fixtures

import com.wavesplatform.protobuf.block.{Block, MicroBlock}

trait BlockInfo {
  var blockInfo: Block
  var microBlockInfo: MicroBlock
}
