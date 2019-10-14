package com.wavesplatform.lang.compiler.names
import cats.kernel.Monoid
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.compiler.compilerContext
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.Common
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}

class NameDuplicationTest extends FreeSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val ctx: CompilerContext =
    Monoid.combine(
      compilerContext,
      WavesContext
        .build(
          DirectiveSet(V3, Account, DAppType).explicitGet()
        )
        .compilerContext
    )

  "Contract compilation" - {

    "should succeed when" - {
      "these have the same name:" - {

        "constant and user function argument" in {
          compileOf("""
            |let x = 42
            |
            |func some(y: Boolean, x: Boolean) = !x
            |""") shouldBe 'right
        }

        "constant and callable function argument" in {
          compileOf("""
            |let x = 42
            |
            |@Callable(i)
            |func some(a: Int, x: String) = WriteSet([DataEntry("a", x)])
            |""") shouldBe 'right
        }

        "user function and its argument" in {
          compileOf("""
            |func sameName(sameName: Boolean) = !sameName
            |""") shouldBe 'right
        }

        "user function and argument; callable annotation bindings and arguments" in {
          compileOf("""
            |func i(i: Int) = i + 1
            |
            |@Callable(x)
            |func foo(i: Int) = WriteSet([DataEntry("a", i + 1)])
            |
            |@Callable(i)
            |func bar(x: Int) = WriteSet([DataEntry("a", this.bytes)])
            |""") shouldBe 'right
        }

      }
    }

    "should fail when" - {
      "these have the same name:" - {

        "two constants" in {
          compileOf("""
            |let x = 42
            |let x = true
            |""") should produce("already defined")
        }

        "constant and user function" in {
          compileOf("""
            |let x = 42
            |
            |func x() = true
            |""") should produce("already defined")
        }

        "constant and callable function" in {
          compileOf("""
            |let x = 42
            |
            |@Callable(i)
            |func x() = WriteSet([DataEntry("a", "a")])
            |""") should produce("already defined")
        }

        "constant and verifier function" in {
          compileOf("""
            |let x = 42
            |
            |@Verifier(i)
            |func x() = WriteSet([DataEntry("a", "a")])
            |""") should produce("already defined")
        }

        "constant and callable annotation binding" in {
          compileOf("""
            |let x = 42
            |
            |@Callable(x)
            |func some(i: Int) = WriteSet([DataEntry("a", "a")])
            |""") should produce("Annotation binding `x` overrides already defined var")
        }

        "constant and verifier annotation binding" in {
          compileOf("""
            |let x = 42
            |
            |@Verifier(x)
            |func some() = true
            |""") should produce("Annotation binding `x` overrides already defined var")
        }

        "two user functions" in {
          compileOf("""
            |func sameName() = true
            |
            |func sameName() = 1
            |""") should produce("already defined")
        }

        "two user function arguments" in {
          compileOf("""
            |func some(sameName: String, sameName: Int) = sameName
            |""") should produce("declared with duplicating argument names")
        }

        "user and callable functions" in {
          compileOf("""
            |func sameName() = true
            |
            |@Callable(i)
            |func sameName() = WriteSet([DataEntry("a", "a")])
            |""") should produce("already defined")
        }

        "user and verifier functions" in {
          compileOf("""
            |func sameName() = true
            |
            |@Verifier(i)
            |func sameName() = true
            |""") should produce("already defined")
        }

        "two callable functions" in {
          compileOf("""
            |@Callable(i)
            |func sameName() = WriteSet([DataEntry("a", "a")])
            |
            |@Callable(i)
            |func sameName() = WriteSet([DataEntry("b", "b")])
            |""") should produce("already defined")
        }

        "two callable function arguments" in {
          compileOf("""
            |@Callable(i)
            |func some(sameName: String, sameName: Int) = WriteSet([DataEntry("b", sameName)])
            |""") should produce("declared with duplicating argument names")
        }

        "callable and verifier functions" in {
          compileOf("""
            |@Callable(i)
            |func sameName() = WriteSet([DataEntry("a", "a")])
            |
            |@Verifier(i)
            |func sameName() = true
            |""") should produce("already defined")
        }

        "callable function and its callable annotation binding" in {
          compileOf("""
            |@Callable(sameName)
            |func sameName() = WriteSet([DataEntry("a", this.bytes)])
            |""") shouldBe 'right
        }

        "callable annotation binding and its function argument" in {
          compileOf("""
            |@Callable(i)
            |func some(s: String, i: Int) =
            |   if (this == "abc") then
            |      WriteSet([DataEntry("a", "a")])
            |   else
            |      WriteSet([DataEntry("a", "b")])
            |""") should produce("override annotation bindings")
        }

        "duplicating arguments with other arguments among them" in {
          val separatingArgsCount = Gen.choose(1, 20)
          forAll(separatingArgsCount) { c =>
            compileOf(
              s"""
                 | func f(
                 |      sameArg: Int,
                 |      ${1 to c map (i => s"x$i: Int") mkString("", ", ", ",")}
                 |      sameArg: Int
                 |    ) = true
             """
            ) should produce("duplicating argument")
          }
        }
      }
    }

  }

  def compileOf(script: String): Either[String, DApp] = {
    val expr = Parser.parseContract(script.stripMargin).get.value
    compiler.ContractCompiler(ctx, expr)
  }

}
