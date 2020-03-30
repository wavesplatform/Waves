package com.wavesplatform.lang
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Terms._

import scala.util.{Left, Right}

object Testing {

  def evaluated(i: Any): Either[String, EVALUATED] = i match {
    case s: String        => CONST_STRING(s)
    case s: Long          => Right(CONST_LONG(s))
    case s: Int           => Right(CONST_LONG(s))
    case s: ByteStr       => CONST_BYTESTR(s)
    case s: CaseObj       => Right(s)
    case s: Boolean       => Right(CONST_BOOLEAN(s))
    case a: Seq[_]        => ARR(a.map(x => evaluated(x).explicitGet()).toIndexedSeq)
    case _                => Left("Bad Assert: unexprected type")
  }
}
