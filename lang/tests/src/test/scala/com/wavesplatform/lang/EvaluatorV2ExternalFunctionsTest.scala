package com.wavesplatform.lang

class EvaluatorV2ExternalFunctionsTest extends EvaluatorV2TestBase {

  property("simple let") {
    val script =
      """
        | let a = getInteger(Address(base58''), "x"+"y").value()
        | let b = 1000 + a + 10000
        | let c = a + b + 100000
        | c + a
      """.stripMargin

/*    inside(eval(script, limit = 0, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = value(getInteger(Address(base58''), ("x" + "y")))
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 1, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = {
            |    let @a = getInteger(Address(
            |	bytes = base58''
            |), ("x" + "y"))
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 2, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = {
            |    let @a = getInteger(Address(
            |	bytes = base58''
            |), ("x" + "y"))
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 3, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = {
            |    let @a = getInteger(Address(
            |	bytes = base58''
            |), ("x" + "y"))
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 4, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = {
            |    let @a = getInteger(Address(
            |	bytes = base58''
            |), ("x" + "y"))
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 19, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = {
            |    let @a = getInteger(Address(
            |	bytes = base58''
            |), ("x" + "y"))
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 20, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 20
        decompiled shouldBe
          """
            |let a = {
            |    let @a = getInteger(Address(
            |	bytes = base58''
            |), "xy")
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }*/

/*    inside(eval(script, limit = 29, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 20
        decompiled shouldBe
          """
            |let a = {
            |    let @a = getInteger(Address(
            |	bytes = base58''
            |), "xy")
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }*/

    inside(eval(script, limit = 30, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 30
        decompiled shouldBe
          """
            |let a = value(42)
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }
/*
    inside(eval(script, limit = 31, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 30
        decompiled shouldBe
          """
            |let a = {
            |    let @a = 42
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }

    inside(eval(script, limit = 100, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        cost shouldBe 30
        decompiled shouldBe
          """
            |let a = {
            |    let @a = 42
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    }
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }*/
  }

  property("let with args") {
    val script =
      """
        | let a = 1 + 10 + getInteger(Address(base58''), "x"+"y").value()
        | let b = 1000 + a + 10000
        | let c = a + b + 100000
        | c + a
      """.stripMargin

    inside(eval(script, limit = 100000, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
//        cost shouldBe 0
        decompiled shouldBe
          """
            |let a = ((1 + 10) + {
            |    let @a = 42
            |    if ((@a == unit))
            |        then throw("value() called on unit value")
            |        else @a
            |    })
            |let b = ((1000 + a) + 10000)
            |let c = ((a + b) + 100000)
            |(c + a)
          """.stripMargin.trim
    }
  }

  property("if with branches") {
    val script =
      """
        | if(2+2 == 4)
        | then 3 + getInteger(Address(base58''), "x"+"y").value()
        | else 43
      """.stripMargin

    inside(eval(script, limit = 100000, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
//        cost shouldBe 0
        decompiled shouldBe
          """
            |if (((2 + 2) == 4))
            |    then (3 + {
            |        let @a = 42
            |        if ((@a == unit))
            |            then throw("value() called on unit value")
            |            else @a
            |        })
            |    else 43
          """.stripMargin.trim
    }
  }

  property("if with branches 2") {
    val script =
      """
        | if(2+2 == 4)
        | then 43
        | else getInteger(Address(base58''), "x"+"y")
      """.stripMargin

    inside(eval(script, limit = 100000, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
        //        cost shouldBe 0
        decompiled shouldBe
          """
            |if (((2 + 2) == 4))
            |    then 43
            |    else 42
          """.stripMargin.trim
    }
  }

  property("pure if inside getInteger") {
    val script =
      """
        | getInteger(Address(base58''), if(2+2 == 4) then "x" + "y" else "x" + "z")
      """.stripMargin

    inside(eval(script, limit = 100000, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
        //        cost shouldBe 0
        decompiled shouldBe "42"
    }
  }

  property("expand user function") {
    val script =
      """
        | func user(s : String) = {
        |    3 + getInteger(Address(base58''), if(2+2 == 4) then s + "y" else s + "z").value()
        | }
        | 2 + user("x")
      """.stripMargin

    inside(eval(script, limit = 999, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
        //        cost shouldBe 0
        decompiled shouldBe
          """
            |func user (s) = (3 + value(getInteger(Address(base58''), if (((2 + 2) == 4))
            |    then (s + "y")
            |    else (s + "z"))))
            |
            |(2 + {
            |    let s = "x"
            |    (3 + {
            |        let @a = 42
            |        if ((@a == unit))
            |            then throw("value() called on unit value")
            |            else @a
            |        })
            |    })
            |""".stripMargin.trim
    }
  }


  property("expand predefined user function") {
    val script =
      """
        | func getIntegerValue2(addr: Address, a:String) = getInteger(addr,a).value()
        | func user(s : String) = {
        |    3 + getIntegerValue2(Address(base58''), if(2+2 == 4) then s + "y" else s + "z")
        | }
        | 2 + user("x")
      """.stripMargin

    inside(eval(script, limit = 999, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
        //        cost shouldBe 0
        decompiled shouldBe
          """
            |func getIntegerValue2 (addr,a) = value(getInteger(addr, a))
            |
            |func user (s) = (3 + getIntegerValue2(Address(base58''), if (((2 + 2) == 4))
            |    then (s + "y")
            |    else (s + "z")))
            |
            |(2 + {
            |    let s = "x"
            |    (3 + {
            |        let addr = Address(
            |	bytes = base58''
            |)
            |        let a = "xy"
            |        let @a = 42
            |        if ((@a == unit))
            |            then throw("value() called on unit value")
            |            else @a
            |        })
            |    })
            """.stripMargin.trim
    }
  }

  property("expand user calls user") {
    val script =
      """
        | func u1(a: Int) = a + 1
        | func u2(b:Int) = u1(b) + 2
        | func u3(c:Int) = u2(c) + 3
        | func u4(d:Int) = u3(d) + 4
        | func u5(e:Int) = u4(e) + 5
        | u5(88+99) + 7
      """.stripMargin

    inside(eval(script, limit = 999, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
        //        cost shouldBe 0
        decompiled shouldBe
          """func u1 (a) = (a + 1)
            |
            |func u2 (b) = (u1(b) + 2)
            |
            |func u3 (c) = (u2(c) + 3)
            |
            |func u4 (d) = (u3(d) + 4)
            |
            |func u5 (e) = (u4(e) + 5)
            |
            |({
            |    let e = (88 + 99)
            |    ({
            |        let d = e
            |        ({
            |            let c = d
            |            ({
            |                let b = c
            |                ({
            |                    let a = b
            |                    (a + 1)
            |                    } + 2)
            |                } + 3)
            |            } + 4)
            |        } + 5)
            |    } + 7)
            |""".stripMargin.trim
    }
  }

  property("expand user calls constructor w/ impure") {
    val script =
      """
        | func u1(a: Int) = Address(getBinary(Address(base58''), "x" + "y").value()).bytes.size() + 1
        | u1(6) + 7
      """.stripMargin

    inside(eval(script, limit = 999, evaluateAll = false)) {
      case (_, decompiled, cost) =>
        println(decompiled)
        //        cost shouldBe 0
        decompiled shouldBe
          """func u1 (a) = (size(Address(value(getBinary(Address(base58''), ("x" + "y")))).bytes) + 1)
            |
            |({
            |    let a = 6
            |    (size(Address({
            |        let @a = base58'Ajszg3RAw2'
            |        if ((@a == unit))
            |            then throw("value() called on unit value")
            |            else @a
            |        }).bytes) + 1)
            |    } + 7)
            |""".stripMargin.trim
    }
  }
}
