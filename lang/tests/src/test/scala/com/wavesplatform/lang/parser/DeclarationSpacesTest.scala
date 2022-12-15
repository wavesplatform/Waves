package com.wavesplatform.lang.parser
import com.wavesplatform.lang.directives.values.{Expression, V6}
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.{Decompiler, TestCompiler}
import com.wavesplatform.test.PropSpec

class DeclarationSpacesTest extends PropSpec {
  property("absence of spaces between declaration syntax symbols should not be significant") {
    val script =
      """
        | func f(a:(Int,Int,Int),b:Int,c:Int)=[1,2,3]
        | let (a,b,c)=(1,2,3)
        | true
      """.stripMargin
    val compiled = TestCompiler(V6).compileExpression(script).expr
    Decompiler(compiled, getDecompilerContext(V6, Expression)) shouldBe
      """
        |func f (a,b,c) = [1, 2, 3]
        |
        |let $t04766 = $Tuple3(1, 2, 3)
        |let a = $t04766._1
        |let b = $t04766._2
        |let c = $t04766._3
        |true
      """.stripMargin.trim
  }
}
