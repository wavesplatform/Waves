package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class ParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  def parse(x: String): Expr = Parser(x).get.value

  property("simple expressions") {
    parse("10") shouldBe CONST_INT(10)
    parse("10+11") shouldBe SUM(CONST_INT(10), CONST_INT(11))
    parse("(10+11)") shouldBe SUM(CONST_INT(10), CONST_INT(11))
    parse("(10+11) + 12") shouldBe SUM(SUM(CONST_INT(10), CONST_INT(11)), CONST_INT(12))
    parse("10   + 11 + 12") shouldBe SUM(CONST_INT(10), SUM(CONST_INT(11), CONST_INT(12)))
    parse("1+2+3+4+5") shouldBe SUM(CONST_INT(1), SUM(CONST_INT(2), SUM(CONST_INT(3), SUM(CONST_INT(4), CONST_INT(5)))))

    parse("1==1") shouldBe EQ_INT(CONST_INT(1), CONST_INT(1))

    parse("true && true") shouldBe AND(TRUE, TRUE)
    parse("true || false") shouldBe OR(TRUE, FALSE)
    parse("true || (true && false)") shouldBe OR(TRUE, AND(TRUE, FALSE))
    parse("false || false || false") shouldBe OR(FALSE, OR(FALSE, FALSE))

    parse("(1>= 0)||(3 >2)") shouldBe OR(GE(CONST_INT(1), CONST_INT(0)), GT(CONST_INT(3), CONST_INT(2)))
  }

  property("priority in expressions") {
    parse("3 + 2 > 2 + 1") shouldBe GT(SUM(CONST_INT(3), CONST_INT(2)), SUM(CONST_INT(2), CONST_INT(1)))
    parse("1 >= 0 || 3 > 2") shouldBe OR(GE(CONST_INT(1), CONST_INT(0)), GT(CONST_INT(3), CONST_INT(2)))

  }
}
