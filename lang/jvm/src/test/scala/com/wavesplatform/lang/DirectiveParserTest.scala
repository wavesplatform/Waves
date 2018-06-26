package com.wavesplatform.lang

import com.wavesplatform.lang.directives.DirectiveKey.LANGUAGE_VERSION
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DirectiveParserTest extends PropSpec with PropertyChecks with Matchers {

  def parse(s: String): List[Directive] = DirectiveParser(s)

  property("parse LANGUAGE_VERSION directive") {
    parse("{-# LANGUAGE_VERSION 10 #-}") shouldBe List(Directive(LANGUAGE_VERSION, "10"))
    parse("""
        |
        |{-# LANGUAGE_VERSION 10 #-}
        |
      """.stripMargin) shouldBe List(Directive(LANGUAGE_VERSION, "10"))
  }
}
