package scorex.api.http

import akka.http.scaladsl.server.Directive1
import com.wavesplatform.state2.ByteStr
import scorex.block.Block
import scorex.transaction.{History, TransactionParser}


trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(history: History, encodedSignature: String): Directive1[Block] =
  if (encodedSignature.length > TransactionParser.SignatureStringLength) complete(InvalidSignature) else {
    ByteStr.decodeBase58(encodedSignature).toOption.toRight(InvalidSignature)
        .flatMap(s => history.blockById(s).toRight(BlockDoesNotExist)) match {
      case Right(b) => provide(b)
      case Left(e) => complete(e)
    }
  }
}
