package com.wavesplatform.api.http

import akka.http.scaladsl.server.Directive1
import com.wavesplatform.api.http.ApiError.{BlockDoesNotExist, InvalidSignature}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain

trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(blockchain: Blockchain, encodedSignature: String): Directive1[Block] =
    if (encodedSignature.length > requests.SignatureStringLength) complete(InvalidSignature)
    else {
      ByteStr
        .decodeBase58(encodedSignature)
        .toOption
        .toRight(InvalidSignature)
        .flatMap(s => blockchain.blockById(s).toRight(BlockDoesNotExist)) match {
        case Right(b) => provide(b)
        case Left(e)  => complete(e)
      }
    }
}
