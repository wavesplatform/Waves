package com.wavesplatform.lang.v1.repl.model

trait WithProofs {
  def proofs: List[ByteString]
}
