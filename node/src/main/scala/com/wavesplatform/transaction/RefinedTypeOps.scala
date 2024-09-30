package com.wavesplatform.transaction

import cats.syntax.either.*
import com.wavesplatform.lang.ValidationError
import eu.timepit.refined.api.RefinedType

class RefinedTypeOps[FTP, T](implicit rt: RefinedType.AuxT[FTP, T]) extends eu.timepit.refined.api.RefinedTypeOps[FTP, T] with Serializable {
  def apply(t: T)(err: ValidationError): Either[ValidationError, FTP] =
    from(t).leftMap(_ => err)
}
