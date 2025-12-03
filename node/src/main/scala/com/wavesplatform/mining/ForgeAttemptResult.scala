package com.wavesplatform.mining

import com.wavesplatform.block.Block

enum ForgeAttemptResult {
  case TemporaryFailure(reason: String)
  case PermanentFailure(reason: String)
  case Success(newBlock: Block, restConstraint: MiningConstraint)
}

object ForgeAttemptResult {
  extension (self: ForgeAttemptResult) {
    def toEither: Either[String, Success] = self match {
      case TemporaryFailure(reason)      => Left(reason)
      case PermanentFailure(reason)      => Left(reason)
      case x: ForgeAttemptResult.Success => Right(x)
    }
  }
}
