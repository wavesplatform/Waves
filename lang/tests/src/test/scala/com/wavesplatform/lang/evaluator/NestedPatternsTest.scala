package com.wavesplatform.lang.evaluator
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN

class NestedPatternsTest extends EvaluatorSpec {
  implicit val v: StdLibVersion = V5

  property("typed tuples") {
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

  property("typed tuples with constants") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case (x: Int, "a")     => x
         |     case (x: Int, "b" | 7) => x * 2
         |     case (x: Int, _)       => x * 3
         |     case _                 => throw("unexpected")
         |   }
         |
         | f((1, "a")) == 1 &&
         | f((1, "b")) == 2 &&
         | f((1, 7)) == 2   &&
         | f((1, "c")) == 3
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("multi-sized tuples") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case "a"             => 1
         |     case ("a", "b")      => 2
         |     case ("a", "b", "c") => 3
         |     case _               => throw("unexpected")
         |   }
         |
         | f(("a")) == 1           &&
         | f(("a", "b")) == 2      &&
         | f(("a", "b", "c")) == 3
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("typed tuples with case types") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case (_: Lease, Lease(recipient = Address(base58'aaa'), amount = 1, nonce = 1)) => 1
         |     case (_: Lease, Lease(recipient = Address(base58'bbb')))                        => 2
         |     case (_: Lease, Lease(recipient = r: Alias, amount = 1, nonce = 1))             => 3
         |     case (_: Lease, Lease(recipient = r: Alias, amount = 1, nonce = 1), _: Burn)    => 4
         |     case (_: Lease | Issue, Lease(recipient = r, amount = 1, nonce = 2))            => r
         |     case _                                                                          => throw("unexpected")
         |   }
         |
         | let l = Lease(Address(base58''), 1, 1)
         |
         | f((l, Lease(Address(base58'aaa'), 1, 1)))           == 1 &&
         | f((l, Lease(Address(base58'bbb'), 1, 1)))           == 2 &&
         | f((l, Lease(Alias("x"), 1, 1)))                     == 3 &&
         | f((l, Lease(Alias("x"), 1, 1), Burn(base58'', 1)))  == 4
         | # f((l, Lease(Alias("x"), 1, 2)))                     == Alias("x")
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("deep typed tuples") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case (_: Int, (x: Int, "a"))                       => x
         |     case ((x: Int, "b" | "c"), y: Int)                 => x + y
         |     case ((x: Int, "c" | 1), (y: Int, (7, z: String))) => x + y + z.size()
         |     case _                                             => throw("unexpected")
         |   }
         |
         | f((8, (1, "a")))               == 1 &&
         | f((8, (1, "a")))               == 1 &&
         | f(((1, "b"), 2))               == 3 &&
         | f(((1, "c"), (2, (7, "xxx")))) == 6
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  ignore("multi-sized tuples with untyped gaps") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case "a"         => 1
         |     case ("a", _)    => 2
         |     case ("a", _, _) => 3
         |     case _           => throw("unexpected")
         |   }
         |
         | f(("a")) == 1           &&
         | f(("a", "b")) == 2      &&
         | f(("a", "b", "c")) == 3
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }
}
