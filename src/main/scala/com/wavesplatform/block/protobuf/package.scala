package com.wavesplatform.block

//noinspection TypeAnnotation
package object protobuf {
  type PBBlock = com.wavesplatform.block.protobuf.Block
  val PBBlock = com.wavesplatform.block.protobuf.Block

  type VanillaBlock = com.wavesplatform.block.Block
  val VanillaBlock = com.wavesplatform.block.Block
}
