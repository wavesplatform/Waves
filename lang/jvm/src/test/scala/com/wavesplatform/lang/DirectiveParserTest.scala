package com.wavesplatform.lang

import com.wavesplatform.lang.directives.DirectiveKey._
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DirectiveParserTest extends PropSpec with PropertyChecks with Matchers {

  def parse(s: String): List[Directive] = DirectiveParser(s)

  property("parse directives") {
    parse("{-# STDLIB_VERSION 10 #-}") shouldBe List(Directive(STDLIB_VERSION, "10"))
    parse("""
        |
        |{-# STDLIB_VERSION 10 #-}
        |
      """.stripMargin) shouldBe List(Directive(STDLIB_VERSION, "10"))
    parse("""
            |
            |{-# CONTENT_TYPE FOO #-}
            |
      """.stripMargin) shouldBe List(Directive(CONTENT_TYPE, "FOO"))
    parse("""
            |
            |{-# SCRIPT_TYPE BAR #-}
            |
      """.stripMargin) shouldBe List(Directive(SCRIPT_TYPE, "BAR"))
  }
}
