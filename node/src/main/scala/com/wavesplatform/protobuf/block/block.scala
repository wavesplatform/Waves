package com.wavesplatform.protobuf

package object block {
  type PBBlock = com.wavesplatform.protobuf.block.Block
  val PBBlock = com.wavesplatform.protobuf.block.Block

  type VanillaBlock = com.wavesplatform.block.Block
  val VanillaBlock = com.wavesplatform.block.Block
}
