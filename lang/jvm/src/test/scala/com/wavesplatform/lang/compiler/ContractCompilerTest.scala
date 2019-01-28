package com.wavesplatform.lang.compiler
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, Version}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractCompilerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("contract compiles when uses annotation bindings and correct return type") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(Version.ContractV, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteStr) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet(List(DataEntry("a", a), DataEntry("sender", sender0)))
          | }
          |
          | @Verifier(t)
          | func verify() = {
          |   t.id == base58''
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val expectedResult = Right(
      Contract(
        List.empty,
        List(ContractFunction(
          CallableAnnotation("invocation"),
          Terms.FUNC(
            "foo",
            List("a"),
            BLOCK(
              LET("sender0", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
              FUNCTION_CALL(
                User(FieldNames.WriteSet),
                List(FUNCTION_CALL(
                  Native(1102),
                  List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                       FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("sender0"))))
                ))
              )
            )
          )
        )),
        Some(
          VerifierFunction(
            VerifierAnnotation("t"),
            FUNC("verify", List.empty, FUNCTION_CALL(Native(FunctionIds.EQ), List(GETTER(REF("t"), "id"), CONST_BYTESTR(ByteStr.empty))))
          ))
      ))
    compiler.ContractCompiler(ctx, expr) shouldBe expectedResult
  }

  property("contract compiles fails when incorrect return type") {
    val ctx = compilerContext
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteStr) = {
          |  a + invocation.caller.bytes
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("ContractFunction must return WriteSet/PaymentSet/ContractResult")
  }

  property("hodlContract") {
    val ctx = Monoid
      .combineAll(
        Seq(
          PureContext.build(Version.ContractV),
          CryptoContext.build(com.wavesplatform.lang.Global),
          WavesContext.build(Version.ContractV, Common.emptyBlockchainEnvironment(), false)
        ))
      .compilerContext
    val expr = {
      val script =
        """
          |
          |	@Callable(i)
          |	func deposit() = {
          |   let pmt = extract(i.payment)
          |   if (isDefined(pmt.asset)) then throw("can hodl waves only at the moment")
          |   else {
          |	  	let currentKey = toBase58String(i.caller.bytes)
          |	  	let currentAmount = match getInteger(i.contractAddress, currentKey) {
          |	  		case a:Int => a
          |	  		case _ => 0
          |	  	}
          |	  	let newAmount = currentAmount + pmt.amount
          |	  	WriteSet(List(DataEntry(currentKey, newAmount)))
          |
          |   }
          |	}
          |
          | @Callable(i)
          | func withdraw(amount: Int) = {
          |	  	let currentKey = toBase58String(i.caller.bytes)
          |	  	let currentAmount = match getInteger(i.contractAddress, currentKey) {
          |	  		case a:Int => a
          |	  		case _ => 0
          |	  	}
          |		let newAmount = currentAmount - amount
          |	 if (amount < 0)
          |			then throw("Can't withdraw negative amount")
          |  else if (newAmount < 0)
          |			then throw("Not enough balance")
          |			else ContractResult(
          |					WriteSet(List(DataEntry(currentKey, newAmount))),
          |					TransferSet(List(ContractTransfer(i.caller, amount, unit)))
          |				)
          |	}
          |
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }
}
