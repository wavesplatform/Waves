package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions._

import scala.annotation.tailrec
import scala.util.Random

object Macro {
  def unwrapFold(
    pos:   Pos,
    limit: Int,
    list:  EXPR,
    acc:   EXPR,
    func:  REF
  ): EXPR = {
    val id = s"${pos.start}${pos.end}"

    implicit def valid(v: String): VALID[String] = VALID(pos, v)
    implicit def ref(v: String): REF             = REF(pos, "$" + v + id)
    implicit def long(l: Long): CONST_LONG       = CONST_LONG(pos, l)

    def let(name: String, value: EXPR)              = LET(pos, "$" + name + id, value, Nil)
    def block(decl: Declaration, expr: EXPR)        = BLOCK(pos, decl, expr)
    def call(func: PART[String], args: List[EXPR])  = FUNCTION_CALL(pos, func, args)

    @tailrec def stepRec(total: Int, current: Int, steps: EXPR => EXPR): EXPR =
      current match {
        case n if n == total + 1 =>
          steps(
            call("throw", List(CONST_STRING(pos, s"List size exceed $total")))
          )
        case n if n >= 0 =>
          val next = (expr: EXPR) => {
            val nextAcc = let(
              "acc" + (n + 1),
              call(func.key, List("acc" + n, call("getElement", List("list", n))))
            )
            IF(pos,
              call("==", List("size", n)),
              "acc" + n,
              block(nextAcc, expr)
            )
          }
          stepRec(total, n + 1, steps compose next)
        case _ => throw new RuntimeException("Unexpected negative value")
      }

    if (limit < 1) INVALID(pos, "FOLD limit should be natural")
    else {
      val listL = let("list", list)
      val sizeL = let("size", call("size", List("list")))
      val accL  = let("acc0", acc)
      block(listL, block(sizeL, block(accL, stepRec(limit, 0, identity))))
    }
  }
}
