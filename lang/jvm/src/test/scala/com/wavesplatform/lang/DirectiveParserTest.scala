package com.wavesplatform.lang

import com.wavesplatform.lang.directives.DirectiveKey._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DirectiveParserTest extends PropSpec with PropertyChecks with Matchers {

  def parse(s: String): Either[ExecutionError, List[Directive]] = DirectiveParser(s)

  property("parse directives") {
    parse("{-# STDLIB_VERSION 1 #-}") shouldBe Right(List(Directive(STDLIB_VERSION, V1)))

    parse("""
        |
        |{-# STDLIB_VERSION 1 #-}
        |
      """.stripMargin) shouldBe Right(List(Directive(STDLIB_VERSION, V1)))

    parse("""
            |
            |{-# CONTENT_TYPE EXPRESSION #-}
            |
      """.stripMargin) shouldBe Right(List(Directive(CONTENT_TYPE, Expression)))

    parse("""
            |
            |{-# SCRIPT_TYPE ASSET #-}
            |
      """.stripMargin) shouldBe Right(List(Directive(SCRIPT_TYPE, Asset)))
  }
}
