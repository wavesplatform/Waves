package com.wavesplatform.lang.evaluator
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN

class NestedPatternsTest extends EvaluatorSpec {
  implicit val v: StdLibVersion = V5

  property("nested typed tuples") {
    eval(
      s"""
         | func sum(arg: Any) = {
         |   let (x, y) = match arg {
         |     case (x: Int, y: Int)               => (x, y)
         |     case (x: String, y: String)         => (x.size(), y.size())
         |     case (x: ByteVector, y: ByteVector) => (x.size(), y.size())
         |     case (x: Boolean, y: Boolean)       => (if (x) then 1 else 0, if (y) then 1 else 0)
         |     case _                              => throw("unexpected")
         |   }
         |   x + y
         | }
         |
         | sum((1, 2)) == 3                     &&
         | sum(("aaa", "bbb")) == 6             &&
         | sum((base58'aaa', base58'bbb')) == 6 &&
         | sum((true, false)) == 1
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("nested typed tuples with constants") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case (x: Int, "a")       => x
         |     case (x: Int, "b" | "c") => x * 2
         |     case (x: Int, _)         => x * 3
         |     case _                   => throw("unexpected")
         |   }
         |
         | f((1, "a")) == 1 &&
         | f((1, "b")) == 2 &&
         | f((1, "c")) == 2 &&
         | f((1, "d")) == 3
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  ignore("deep nested typed tuples") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case (_, (x: Int, "a"))            => x
         |     case ((x: Int, "b" | "c"), y: Int) => x + y
         |     case ((x: Int, "b" | "c"), _)      => x * 2
         |     case _                   => throw("unexpected")
         |   }
         |
         | f((1, (1, "a")))   == 1 &&
         | f(("r", (1, "a"))) == 1 &&
         | f(((1, "b"), 2))   == 3 &&
         | f(((1, "c"), "r")) == 2
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }
}
