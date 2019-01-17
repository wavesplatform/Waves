package com.wavesplatform.lang
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Terms._
import scodec.bits.ByteVector

import scala.util.{Left, Right}

object Testing {

  def evaluated(i: Any): Either[String, EVALUATED] = i match {
    case s: String        => Right(CONST_STRING(s))
    case s: Long          => Right(CONST_LONG(s))
    case s: Int           => Right(CONST_LONG(s))
    case s: ByteVector    => Right(CONST_BYTEVECTOR(s))
    case s: CaseObj       => Right(s)
    case s: Boolean       => Right(CONST_BOOLEAN(s))
    case a: IndexedSeq[_] => Right(ARR(a.map(x => evaluated(x).explicitGet())))
    case _                => Left("Bad Assert: unexprected type")
  }
}
