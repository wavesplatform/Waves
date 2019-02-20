package com.wavesplatform.lang

import com.wavesplatform.lang.directives.DirectiveKey._
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DirectiveParserTest extends PropSpec with PropertyChecks with Matchers {

  def parse(s: String): List[Directive] = DirectiveParser(s)

  property("parse STDLIB_VERSION directive") {
    parse("{-# STDLIB_VERSION 10 #-}") shouldBe List(Directive(STDLIB_VERSION, "10"))
    parse("""
        |
        |{-# STDLIB_VERSION 10 #-}
        |
      """.stripMargin) shouldBe List(Directive(STDLIB_VERSION, "10"))
    parse("""
            |
            |{-# SCRIPT_TYPE FOO #-}
            |
      """.stripMargin) shouldBe List(Directive(CONTENT_TYPE, "FOO"))
  }
}
