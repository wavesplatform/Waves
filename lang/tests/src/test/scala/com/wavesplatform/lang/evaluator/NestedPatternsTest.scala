package com.wavesplatform.lang.evaluator
import com.wavesplatform.lang.directives.values.{StdLibVersion, V6, V7, V8}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.test.produce

class NestedPatternsTest extends EvaluatorSpec {
  implicit val v: StdLibVersion = V6

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
         |     case (_: Lease, Lease(recipient = r: Alias, amount = 1, nonce = 1 | 2))         => 3
         |     case (_: Lease, Lease(recipient = r: Alias, amount = 1, nonce = 1), _: Burn)    => 4
         |     case (_: Lease | Burn, Lease(recipient = r, amount = a, nonce = n))             => if (n > a) then r else unit
         |     case _                                                                          => throw("unexpected")
         |   }
         |
         | let l = Lease(Address(base58''), 1, 1)
         | let b = Burn(base58'', 1)
         |
         | f((l, Lease(Address(base58'aaa'), 1, 1))) == 1          &&
         | f((l, Lease(Address(base58'bbb'), 1, 1))) == 2          &&
         | f((l, Lease(Alias("x"), 1, 1)))           == 3          &&
         | f((l, Lease(Alias("x"), 1, 2)))           == 3          &&
         | f((l, Lease(Alias("x"), 1, 1), b))        == 4          &&
         | f((b, Lease(Alias("x"), 1, 3)))           == Alias("x") &&
         | f((b, Lease(Alias("x"), 3, 1)))           == unit
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("deep typed tuples") {
    eval(
      s"""
         | func f(arg: Any) =
         |   match arg {
         |     case (_: Int, (x: Int, "a"))                            => x
         |     case ((x: Int, "b" | "c"), y: Int)                      => x + y
         |     case (((x: Int, "c" | 1), (y: Int, (7, z: String))), _) => x + y + z.size()
         |     case _                                                  => throw("unexpected")
         |   }
         |
         | f((8, (1, "a")))                       == 1 &&
         | f(((1, "b"), 2))                       == 3 &&
         | f((((1, "c"), (2, (7, "xxx"))), unit)) == 6
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("multi-sized tuples with untyped gaps") {
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

  property("restrictions from V8") {
    val script1 =
      s"""
         | match Lease(Address(base58''), 1, 1) {
         |   case Lease(nonce = f()) => true
         |   case _                  => false
         | }
       """.stripMargin
    eval(script1)(V7, checkNext = false) shouldBe Right(CONST_BOOLEAN(true))
    eval(script1)(V8) should produce("Only constant value could be matched with object field in 63-66")

    val script2 =
      s"""
         | func f() = 777
         | match Lease(Address(base58''), 1, 1) {
         |   case Lease(nonce = f()) => true
         |   case _                  => false
         | }
       """.stripMargin
    eval(script2)(V7, checkNext = false) shouldBe Right(CONST_BOOLEAN(true))
    eval(script2)(V8) should produce("Only constant value could be matched with object field in 79-82")

    val script3 =
      s"""
         | func f() = 777
         | match Lease(Address(base58''), 1, 1) {
         |   case Lease(nonce = {f()}) => true
         |   case _                    => false
         | }
       """.stripMargin
    eval(script3)(V7, checkNext = false) shouldBe Right(CONST_BOOLEAN(false))
    eval(script3)(V8) should produce("Only constant value could be matched with object field in 80-83")
  }
}
