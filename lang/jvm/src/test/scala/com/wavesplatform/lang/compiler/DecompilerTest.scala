package com.wavesplatform.lang.compiler

import java.util.concurrent.Callable

import cats.instances.unit
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.{Decompiler, Terms}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ContractResult
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation.{LT_OP, SUB_OP, SUM_OP}
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions.{ANNOTATEDFUNC, ANNOTATION, BINARY_OP, CONTRACT, MATCH, MATCH_CASE}
import com.wavesplatform.lang.v1.parser.Expressions.Pos.{AnyPos, RealPos}
import fastparse.parsers.Combinators.Not
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable.ArrayBuffer

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  val opcodes = scala.collection.immutable.Map[Short, String](1.toShort -> "+", 100.toShort -> "sigVerify")

  property("Invoke contract") {
    val scriptText =
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
    val compledContract = com.wavesplatform.lang.v1.parser.Parser.parseContract(scriptText).get.value.toString
    compledContract shouldBe "CONTRACT(AnyPos,List(),List(ANNOTATEDFUNC(AnyPos,ArrayBuffer(ANNOTATION(AnyPos,VALID(RealPos(4,12),Callable),ArrayBuffer(VALID(RealPos(13,14),i)))),FUNC(AnyPos,VALID(RealPos(22,29),deposit),ArrayBuffer(),BLOCK(RealPos(39,430),LET(RealPos(39,67),VALID(RealPos(43,46),pmt),FUNCTION_CALL(RealPos(49,67),VALID(RealPos(49,56),extract),List(GETTER(RealPos(57,66),REF(RealPos(57,58),VALID(RealPos(57,58),i)),VALID(RealPos(59,66),payment)))),List(),false),IF(RealPos(71,430),FUNCTION_CALL(RealPos(75,95),VALID(RealPos(75,84),isDefined),List(GETTER(RealPos(85,94),REF(RealPos(85,88),VALID(RealPos(85,88),pmt)),VALID(RealPos(89,94),asset)))),FUNCTION_CALL(RealPos(102,144),VALID(RealPos(102,107),throw),List(CONST_STRING(RealPos(108,143),VALID(RealPos(109,142),can hodl waves only at the moment)))),BLOCK(RealPos(159,424),LET(RealPos(159,206),VALID(RealPos(163,173),currentKey),FUNCTION_CALL(RealPos(176,206),VALID(RealPos(176,190),toBase58String),List(GETTER(RealPos(191,205),GETTER(RealPos(191,199),REF(RealPos(191,192),VALID(RealPos(191,192),i)),VALID(RealPos(193,199),caller)),VALID(RealPos(200,205),bytes)))),List(),false),BLOCK(RealPos(159,424),LET(RealPos(211,324),VALID(RealPos(215,228),currentAmount),MATCH(RealPos(231,324),FUNCTION_CALL(RealPos(237,278),VALID(RealPos(237,247),getInteger),List(GETTER(RealPos(248,265),REF(RealPos(248,249),VALID(RealPos(248,249),i)),VALID(RealPos(250,265),contractAddress)), REF(RealPos(267,277),VALID(RealPos(267,277),currentKey)))),List(MATCH_CASE(RealPos(286,301),Some(VALID(RealPos(291,292),a)),ArrayBuffer(VALID(RealPos(293,296),Int)),REF(RealPos(300,301),VALID(RealPos(300,301),a))), MATCH_CASE(RealPos(307,318),None,List(),CONST_LONG(RealPos(317,318),0)))),List(),false),BLOCK(RealPos(159,424),LET(RealPos(329,371),VALID(RealPos(333,342),newAmount),BINARY_OP(RealPos(345,371),REF(RealPos(345,358),VALID(RealPos(345,358),currentAmount)),SUM_OP,GETTER(RealPos(361,371),REF(RealPos(361,364),VALID(RealPos(361,364),pmt)),VALID(RealPos(365,371),amount))),List(),false),FUNCTION_CALL(RealPos(376,424),VALID(RealPos(376,384),WriteSet),List(FUNCTION_CALL(RealPos(385,423),VALID(RealPos(385,389),List),List(FUNCTION_CALL(RealPos(390,422),VALID(RealPos(390,399),DataEntry),List(REF(RealPos(400,410),VALID(RealPos(400,410),currentKey)), REF(RealPos(412,421),VALID(RealPos(412,421),newAmount))))))))))))))), ANNOTATEDFUNC(AnyPos,ArrayBuffer(ANNOTATION(AnyPos,VALID(RealPos(437,445),Callable),ArrayBuffer(VALID(RealPos(446,447),i)))),FUNC(AnyPos,VALID(RealPos(455,463),withdraw),ArrayBuffer((VALID(RealPos(464,470),amount),ArrayBuffer(VALID(RealPos(472,475),Int)))),BLOCK(RealPos(485,969),LET(RealPos(485,532),VALID(RealPos(489,499),currentKey),FUNCTION_CALL(RealPos(502,532),VALID(RealPos(502,516),toBase58String),List(GETTER(RealPos(517,531),GETTER(RealPos(517,525),REF(RealPos(517,518),VALID(RealPos(517,518),i)),VALID(RealPos(519,525),caller)),VALID(RealPos(526,531),bytes)))),List(),false),BLOCK(RealPos(485,969),LET(RealPos(537,650),VALID(RealPos(541,554),currentAmount),MATCH(RealPos(557,650),FUNCTION_CALL(RealPos(563,604),VALID(RealPos(563,573),getInteger),List(GETTER(RealPos(574,591),REF(RealPos(574,575),VALID(RealPos(574,575),i)),VALID(RealPos(576,591),contractAddress)), REF(RealPos(593,603),VALID(RealPos(593,603),currentKey)))),List(MATCH_CASE(RealPos(612,627),Some(VALID(RealPos(617,618),a)),ArrayBuffer(VALID(RealPos(619,622),Int)),REF(RealPos(626,627),VALID(RealPos(626,627),a))), MATCH_CASE(RealPos(633,644),None,List(),CONST_LONG(RealPos(643,644),0)))),List(),false),BLOCK(RealPos(485,969),LET(RealPos(653,691),VALID(RealPos(657,666),newAmount),BINARY_OP(RealPos(669,691),REF(RealPos(669,682),VALID(RealPos(669,682),currentAmount)),SUB_OP,REF(RealPos(685,691),VALID(RealPos(685,691),amount))),List(),false),IF(RealPos(694,969),BINARY_OP(RealPos(698,708),CONST_LONG(RealPos(707,708),0),LT_OP,REF(RealPos(698,704),VALID(RealPos(698,704),amount))),FUNCTION_CALL(RealPos(718,757),VALID(RealPos(718,723),throw),List(CONST_STRING(RealPos(724,756),VALID(RealPos(725,755),Can't withdraw negative amount)))),IF(RealPos(765,969),BINARY_OP(RealPos(769,782),CONST_LONG(RealPos(781,782),0),LT_OP,REF(RealPos(769,778),VALID(RealPos(769,778),newAmount))),FUNCTION_CALL(RealPos(792,819),VALID(RealPos(792,797),throw),List(CONST_STRING(RealPos(798,818),VALID(RealPos(799,817),Not enough balance)))),FUNCTION_CALL(RealPos(828,969),VALID(RealPos(828,842),ContractResult),List(FUNCTION_CALL(RealPos(849,897),VALID(RealPos(849,857),WriteSet),List(FUNCTION_CALL(RealPos(858,896),VALID(RealPos(858,862),List),List(FUNCTION_CALL(RealPos(863,895),VALID(RealPos(863,872),DataEntry),List(REF(RealPos(873,883),VALID(RealPos(873,883),currentKey)), REF(RealPos(885,894),VALID(RealPos(885,894),newAmount)))))))), FUNCTION_CALL(RealPos(904,963),VALID(RealPos(904,915),TransferSet),List(FUNCTION_CALL(RealPos(916,962),VALID(RealPos(916,920),List),List(FUNCTION_CALL(RealPos(921,961),VALID(RealPos(921,937),ContractTransfer),List(GETTER(RealPos(938,946),REF(RealPos(938,939),VALID(RealPos(938,939),i)),VALID(RealPos(940,946),caller)), REF(RealPos(948,954),VALID(RealPos(948,954),amount)), REF(RealPos(956,960),VALID(RealPos(956,960),unit)))))))))))))))))))"
    CONTRACT(AnyPos,List(),List(ANNOTATEDFUNC(AnyPos,ArrayBuffer(ANNOTATION(AnyPos,VALID(RealPos(4,12),Callable),ArrayBuffer(VALID(RealPos(13,14),i)))),FUNC(AnyPos,VALID(RealPos(22,29),deposit),ArrayBuffer(),BLOCK(RealPos(39,430),LET(RealPos(39,67),VALID(RealPos(43,46),pmt),FUNCTION_CALL(RealPos(49,67),VALID(RealPos(49,56),extract),List(GETTER(RealPos(57,66),REF(RealPos(57,58),VALID(RealPos(57,58),i)),VALID(RealPos(59,66),payment)))),List(),false),IF(RealPos(71,430),FUNCTION_CALL(RealPos(75,95),VALID(RealPos(75,84),isDefined),List(GETTER(RealPos(85,94),REF(RealPos(85,88),VALID(RealPos(85,88),pmt)),VALID(RealPos(89,94),asset)))),FUNCTION_CALL(RealPos(102,144),VALID(RealPos(102,107),throw),List(CONST_STRING(RealPos(108,143),VALID(RealPos(109,142),can hodl waves only at the moment)))),BLOCK(RealPos(159,424),LET(RealPos(159,206),VALID(RealPos(163,173),currentKey),FUNCTION_CALL(RealPos(176,206),VALID(RealPos(176,190),toBase58String),List(GETTER(RealPos(191,205),GETTER(RealPos(191,199),REF(RealPos(191,192),VALID(RealPos(191,192),i)),VALID(RealPos(193,199),caller)),VALID(RealPos(200,205),bytes)))),List(),false),BLOCK(RealPos(159,424),LET(RealPos(211,324),VALID(RealPos(215,228),currentAmount),MATCH(RealPos(231,324),FUNCTION_CALL(RealPos(237,278),VALID(RealPos(237,247),getInteger),List(GETTER(RealPos(248,265),REF(RealPos(248,249),VALID(RealPos(248,249),i)),VALID(RealPos(250,265),contractAddress)), REF(RealPos(267,277),VALID(RealPos(267,277),currentKey)))),List(MATCH_CASE(RealPos(286,301),Some(VALID(RealPos(291,292),a)),ArrayBuffer(VALID(RealPos(293,296),Int)),REF(RealPos(300,301),VALID(RealPos(300,301),a))), MATCH_CASE(RealPos(307,318),None,List(),CONST_LONG(RealPos(317,318),0)))),List(),false),BLOCK(RealPos(159,424),LET(RealPos(329,371),VALID(RealPos(333,342),newAmount),BINARY_OP(RealPos(345,371),REF(RealPos(345,358),VALID(RealPos(345,358),currentAmount)),SUM_OP,GETTER(RealPos(361,371),REF(RealPos(361,364),VALID(RealPos(361,364),pmt)),VALID(RealPos(365,371),amount))),List(),false),FUNCTION_CALL(RealPos(376,424),VALID(RealPos(376,384),WriteSet),List(FUNCTION_CALL(RealPos(385,423),VALID(RealPos(385,389),List),List(FUNCTION_CALL(RealPos(390,422),VALID(RealPos(390,399),DataEntry),List(REF(RealPos(400,410),VALID(RealPos(400,410),currentKey)), REF(RealPos(412,421),VALID(RealPos(412,421),newAmount))))))))))))))), ANNOTATEDFUNC(AnyPos,ArrayBuffer(ANNOTATION(AnyPos,VALID(RealPos(437,445),Callable),ArrayBuffer(VALID(RealPos(446,447),i)))),FUNC(AnyPos,VALID(RealPos(455,463),withdraw),ArrayBuffer((VALID(RealPos(464,470),amount),ArrayBuffer(VALID(RealPos(472,475),Int)))),BLOCK(RealPos(485,969),LET(RealPos(485,532),VALID(RealPos(489,499),currentKey),FUNCTION_CALL(RealPos(502,532),VALID(RealPos(502,516),toBase58String),List(GETTER(RealPos(517,531),GETTER(RealPos(517,525),REF(RealPos(517,518),VALID(RealPos(517,518),i)),VALID(RealPos(519,525),caller)),VALID(RealPos(526,531),bytes)))),List(),false),BLOCK(RealPos(485,969),LET(RealPos(537,650),VALID(RealPos(541,554),currentAmount),MATCH(RealPos(557,650),FUNCTION_CALL(RealPos(563,604),VALID(RealPos(563,573),getInteger),List(GETTER(RealPos(574,591),REF(RealPos(574,575),VALID(RealPos(574,575),i)),VALID(RealPos(576,591),contractAddress)), REF(RealPos(593,603),VALID(RealPos(593,603),currentKey)))),List(MATCH_CASE(RealPos(612,627),Some(VALID(RealPos(617,618),a)),ArrayBuffer(VALID(RealPos(619,622),Int)),REF(RealPos(626,627),VALID(RealPos(626,627),a))), MATCH_CASE(RealPos(633,644),None,List(),CONST_LONG(RealPos(643,644),0)))),List(),false),BLOCK(RealPos(485,969),LET(RealPos(653,691),VALID(RealPos(657,666),newAmount),BINARY_OP(RealPos(669,691),REF(RealPos(669,682),VALID(RealPos(669,682),currentAmount)),SUB_OP,REF(RealPos(685,691),VALID(RealPos(685,691),amount))),List(),false),IF(RealPos(694,969),BINARY_OP(RealPos(698,708),CONST_LONG(RealPos(707,708),0),LT_OP,REF(RealPos(698,704),VALID(RealPos(698,704),amount))),FUNCTION_CALL(RealPos(718,757),VALID(RealPos(718,723),throw),List(CONST_STRING(RealPos(724,756),VALID(RealPos(725,755),Can't withdraw negative amount)))),IF(RealPos(765,969),BINARY_OP(RealPos(769,782),CONST_LONG(RealPos(781,782),0),LT_OP,REF(RealPos(769,778),VALID(RealPos(769,778),newAmount))),FUNCTION_CALL(RealPos(792,819),VALID(RealPos(792,797),throw),List(CONST_STRING(RealPos(798,818),VALID(RealPos(799,817),Not enough balance)))),FUNCTION_CALL(RealPos(828,969),VALID(RealPos(828,842),ContractResult),List(FUNCTION_CALL(RealPos(849,897),VALID(RealPos(849,857),WriteSet),List(FUNCTION_CALL(RealPos(858,896),VALID(RealPos(858,862),List),List(FUNCTION_CALL(RealPos(863,895),VALID(RealPos(863,872),DataEntry),List(REF(RealPos(873,883),VALID(RealPos(873,883),currentKey)), REF(RealPos(885,894),VALID(RealPos(885,894),newAmount)))))))), FUNCTION_CALL(RealPos(904,963),VALID(RealPos(904,915),TransferSet),List(FUNCTION_CALL(RealPos(916,962),VALID(RealPos(916,920),List),List(FUNCTION_CALL(RealPos(921,961),VALID(RealPos(921,937),ContractTransfer),List(GETTER(RealPos(938,946),REF(RealPos(938,939),VALID(RealPos(938,939),i)),VALID(RealPos(940,946),caller)), REF(RealPos(948,954),VALID(RealPos(948,954),amount)), REF(RealPos(956,960),VALID(RealPos(956,960),unit)))))))))))))))))))"
  }

  property("getter") {
    val expr = Terms.GETTER(Terms.FUNCTION_CALL(
                              function = FunctionHeader.User("testfunc"),
                              args = List(TRUE)
                            ),
                            "testfield")
    Decompiler(expr, opcodes) shouldBe
      """testfunc(true).testfield""".stripMargin
  }

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX"))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    1
        |else
        |    "XXX"
        |}""".stripMargin
  }

  property("if with complicated else branch") {
    val expr = IF(TRUE, CONST_LONG(1), IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    1
        |else
        |    { if (
        |        true
        |        )
        |    then
        |        1
        |    else
        |        "XXX"
        |    }
        |}""".stripMargin
  }

  property("if with complicated then branch") {
    val expr = IF(TRUE, IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")), CONST_LONG(1))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    { if (
        |        true
        |        )
        |    then
        |        1
        |    else
        |        "XXX"
        |    }
        |else
        |    1
        |}""".stripMargin
  }

  property("simple let") {
    val expr = Terms.LET_BLOCK(LET("a", CONST_LONG(1)), TRUE)
    Decompiler(expr, opcodes) shouldBe "{ let a = 1; true }"
  }

  property("native function call with one arg") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(100),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "sigVerify(true)"
  }

  property("undefined native function call  one arg") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(101),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "<Native_101>(true)"
  }

  property("user function call with one args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr, opcodes) shouldBe "foo()"
  }

  property("definition of user function") {
    val expr = Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))
    Decompiler(expr, opcodes) shouldBe
      """func foo (bar,buz) = {
         |    TRUE
         |}""".stripMargin
  }

  property("v2 with LET in BLOCK") {
    val expr = Terms.BLOCK(
      LET("vari", REF("p")),
      TRUE
    )
    Decompiler(expr, opcodes) shouldBe
      """{
      |    let vari =
      |        p;
      |    true
      |}""".stripMargin
  }

  property("let and function call in block") {
    val expr = Terms.BLOCK(Terms.LET("v", REF("p")),
                           Terms.FUNCTION_CALL(
                             PureContext._isInstanceOf.header,
                             List(REF("v"), Terms.CONST_STRING("a"))
                           ))
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        p;
        |    +(v,"a")
        |}""".stripMargin
  }

  property("complicated let in let and function call in block") {
    val expr = Terms.BLOCK(
      Terms.LET("v",
                Terms.BLOCK(Terms.LET("v", REF("p")),
                            Terms.FUNCTION_CALL(
                              PureContext._isInstanceOf.header,
                              List(REF("v"), Terms.CONST_STRING("a"))
                            ))),
      Terms.FUNCTION_CALL(
        PureContext._isInstanceOf.header,
        List(REF("v"), Terms.CONST_STRING("a"))
      )
    )
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        {
        |            let v =
        |                p;
        |            +(v,"a")
        |        };
        |    +(v,"a")
        |}""".stripMargin
  }

  property("old match") {
    val expr = Terms.BLOCK(
      LET("v", REF("p")),
      IF(
        IF(
          FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), CONST_STRING("a"))
          ),
          TRUE,
          FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), CONST_STRING("b"))
          )
        ),
        BLOCK(LET("p", REF("v")), TRUE),
        FALSE
      )
    )
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        p;
        |    { if (
        |        { if (
        |            +(v,"a")
        |            )
        |        then
        |            true
        |        else
        |            +(v,"b")
        |        }
        |        )
        |    then
        |        {
        |            let p =
        |                v;
        |            true
        |        }
        |    else
        |        false
        |    }
        |}""".stripMargin
  }

  property("new match") {
    val expr = Terms.BLOCK(
      Terms.LET("v", REF("p")),
      Terms.IF(
        Terms.IF(
          Terms.FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), Terms.CONST_STRING("a"))
          ),
          TRUE,
          Terms.FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), Terms.CONST_STRING("b"))
          )
        ),
        Terms.BLOCK(Terms.LET("z", Terms.REF("x")), TRUE),
        FALSE
      )
    )
    Decompiler(expr, opcodes) shouldBe
      """{
      |    let v =
      |        p;
      |    { if (
      |        { if (
      |            +(v,"a")
      |            )
      |        then
      |            true
      |        else
      |            +(v,"b")
      |        }
      |        )
      |    then
      |        {
      |            let z =
      |                x;
      |            true
      |        }
      |    else
      |        false
      |    }
      |}""".stripMargin
  }

}
