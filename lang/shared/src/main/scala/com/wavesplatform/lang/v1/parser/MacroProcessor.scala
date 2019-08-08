package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions._

object MacroProcessor {
  def unwrapFold(
    pos:   Pos,
    limit: Int,
    list:  REF,
    acc:   REF,
    func:  REF
  ): EXPR = {
    implicit def valid(v: String): VALID[String] = VALID(pos, v)
    implicit def ref(v: String): REF             = REF(pos, v)

    def let(name: String, value: EXPR)              = LET(pos, name, value, Nil)
    def block(decl: Declaration, expr: EXPR)        = BLOCK(pos, decl, expr)
    def call(func: VALID[String], args: List[EXPR]) = FUNCTION_CALL(pos, func, args)

    def foldStepRec(total: Int, current: Int): EXPR =
      current match {
        case total + 1 => call("throw", List(s"List size exceed $total"))
        case n if n >= 0 =>
          block(let("$acc0", acc),
            IF(pos, call("==", List("$size", CONST_LONG(pos, n))),
              "$acc0",
              foldStepRec(total, current + 1)
            )
          )
        case _ => throw new RuntimeException("unexpected negative value")
      }

    if (limit < 1) INVALID(pos, "FOLD limit should be natural")
    else {
      val listL  = let("$list", list)
      val sizeFc = call("size", List("$list"))
      val sizeL  = let("$size", sizeFc)
      block(listL, block(sizeL, foldStepRec(limit, 0)))
    }
  }
}
