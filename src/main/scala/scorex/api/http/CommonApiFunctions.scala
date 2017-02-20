package scorex.api.http

import akka.http.scaladsl.server.Directive1
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.transaction.{History, TypedTransaction}


trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(history: History, encodedSignature: String): Directive1[Block] =
  if (encodedSignature.length > TypedTransaction.SignatureStringLength) complete(InvalidSignature) else {
    Base58.decode(encodedSignature).toOption.toRight(InvalidSignature)
        .flatMap(s => history.blockById(s).toRight(BlockNotExists)) match {
      case Right(b) => provide(b)
      case Left(e) => complete(e)
    }
  }
}
