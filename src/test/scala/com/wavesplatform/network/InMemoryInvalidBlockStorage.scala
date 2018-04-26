package com.wavesplatform.network

import com.wavesplatform.state.ByteStr
import scorex.transaction.validation.ValidationError.GenericError
import scorex.transaction.validation.ValidationError

class InMemoryInvalidBlockStorage extends InvalidBlockStorage {

  var s: Set[ByteStr] = Set.empty[ByteStr]

  override def add(blockId: ByteStr, validationError: ValidationError): Unit = s += blockId

  override def find(blockId: ByteStr): Option[ValidationError] = {
    if (s.contains(blockId)) Some(GenericError("Unknown")) else None
  }

}
