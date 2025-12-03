package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions.*

object Macro {
  def unwrapStrict(blockPos: Pos, strictLetDecs: Seq[LET], strictBody: EXPR): EXPR = {
    val strictLetDec  = strictLetDecs.head
    val otherLets     = strictLetDecs.tail
    val strictPos     = strictLetDec.position
    val strictLetName = strictLetDec.name
    BLOCK(
      blockPos,
      strictLetDec,
      IF(
        blockPos,
        FUNCTION_CALL(strictPos, VALID(strictPos, "=="), List(REF(strictPos, strictLetName), REF(strictPos, strictLetName))),
        otherLets.foldLeft(strictBody)((r, let) => BLOCK(let.position, let, r)),
        FUNCTION_CALL(strictPos, VALID(strictPos, "throw"), List(CONST_STRING(strictPos, VALID(strictPos, "Strict value is not equal to itself."))))
      )
    )
  }
}
