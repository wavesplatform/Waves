package com.wavesplatform.api.http

import akka.http.scaladsl.server.Directive1
import com.wavesplatform.block.Block
import com.wavesplatform.state.Blockchain

trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(blockchain: Blockchain, encodedSignature: String): Directive1[Block] =
    ???
}
