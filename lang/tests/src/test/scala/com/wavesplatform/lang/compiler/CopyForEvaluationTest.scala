package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.test.PropSpec
import org.scalacheck.Gen

class CopyForEvaluationTest extends PropSpec {
  private val names = Gen.oneOf("a", "b", "c")

  private def declaration(depth: Int): Gen[DECLARATION] =
    Gen.oneOf(
      expr(depth).flatMap(v => names.map(LET(_, v))),
      for {
        name <- names
        args <- Gen.listOfN(2, names)
        body <- expr(depth)
      } yield FUNC(name, args, body),
      Gen.const(FAILED_DEC())
    )

  private def expr(depth: Int): Gen[EXPR] =
    if (depth <= 0) Gen.oneOf(Gen.const(CONST_LONG(1)), Gen.const(CONST_BOOLEAN(true)), names.map(REF(_)), Gen.const(FAILED_EXPR()))
    else
      Gen.lzy(
        Gen.oneOf(
          expr(depth - 1),
          expr(depth - 1).flatMap(e => names.map(GETTER(e, _))),
          for { v <- expr(depth - 1); n <- names; b <- expr(depth - 1) } yield LET_BLOCK(LET(n, v), b),
          for { d <- declaration(depth - 1); b <- expr(depth - 1) } yield BLOCK(d, b),
          for { c <- expr(depth - 1); t <- expr(depth - 1); f <- expr(depth - 1) } yield IF(c, t, f),
          for {
            header <- Gen.oneOf(Native(100), User("f"))
            args   <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, expr(depth - 1)))
          } yield FUNCTION_CALL(header, args)
        )
      )

  // The evaluator writes into these nodes before it reaches them, so a copy must not share any of them with the original. Function bodies
  // and LET values are left out: the evaluator copies those itself when it gets to them.
  private def mutableNodes(e: EXPR): List[AnyRef] = e match {
    case g: GETTER     => g :: mutableNodes(g.expr)
    case lb: LET_BLOCK => lb :: lb.let :: mutableNodes(lb.body)
    case b: BLOCK =>
      b :: (b.dec match {
        case l: LET => List(l)
        case _      => Nil
      }) ++ mutableNodes(b.body)
    case i: IF             => i :: mutableNodes(i.cond) ++ mutableNodes(i.ifTrue) ++ mutableNodes(i.ifFalse)
    case fc: FUNCTION_CALL => fc :: fc.args.flatMap(mutableNodes)
    case _                 => Nil
  }

  property("copies a tree into an equal one that shares no node the evaluator writes into before reaching it") {
    forAll(expr(6)) { e =>
      val copy = Terms.copyForEvaluation(e)
      copy shouldBe e.deepCopy.value
      mutableNodes(e).zip(mutableNodes(copy)).foreach { case (original, copied) => copied shouldNot be theSameInstanceAs original }
    }
  }

  property("marks every copied LET whose value still needs copying") {
    forAll(expr(6)) { e =>
      val copiedLets = mutableNodes(Terms.copyForEvaluation(e)).collect { case l: LET => l }
      copiedLets.foreach { l =>
        val needsCopy = l.value match {
          case _: REF | _: FAILED_EXPR | _: EVALUATED => false
          case _                                      => true
        }
        l.valueShared shouldBe needsCopy
      }
    }
  }

  property("copies a tree nested far deeper than a recursive copy could go") {
    val deep = (1 to 100_000).foldLeft[EXPR](REF("x"))((body, i) => BLOCK(LET(s"v$i", CONST_LONG(i)), body))
    Terms.copyForEvaluation(deep) shouldBe deep
  }
}
