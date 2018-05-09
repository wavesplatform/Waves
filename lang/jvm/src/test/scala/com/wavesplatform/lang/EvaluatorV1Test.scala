package com.wavesplatform.lang
import cats.data.EitherT
import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.Terms.Typed._
import com.wavesplatform.lang.v1.Terms._
import com.wavesplatform.lang.v1.ctx.Context._
import com.wavesplatform.lang.v1.ctx._
import com.wavesplatform.lang.v1.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluation.EvaluatorV1_1
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.{EvaluatorV1, FunctionHeader}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}

class EvaluatorV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev[T: TypeInfo](context: Context = PureContext.instance, expr: EXPR): Either[(Context, ExecutionLog, ExecutionError), T] =
    EvaluatorV1[T](context, expr)
  private def simpleDeclarationAndUsage(i: Int) = BLOCK(LET("x", CONST_LONG(i)), REF("x", LONG), LONG)

  private def logTime[A](in: => A): A = {
    val start = System.currentTimeMillis()
    in
    println(System.currentTimeMillis() - start)
    in
  }

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_LONG(0))((acc, _) => FUNCTION_CALL(sumLong.header, List(acc, CONST_LONG(1)), LONG))

    val r1 = logTime(ev[Long](expr = term))
    val r2 = logTime(EvaluatorV1_1[Long](PureContext.instance, term))

    ev[Long](expr = term) shouldBe Right(100000)
  }

  property("return log and context of failed evaluation") {
    val Left((ctx, log, err)) = ev[Long](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(
          LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)), LONG)),
          FUNCTION_CALL(eqLong.header, List(REF("x", LONG), CONST_LONG(1)), LONG),
          LONG
        ),
        LONG
      )
    )

    val expectedLog =
      "Evaluating BLOCK\n" ++
        "LET: LET(x,CONST_LONG(3)); TYPE: LONG\n" ++
        "Evaluating BLOCK\n" ++
        "LET: LET(x,FUNCTION_CALL(FunctionHeader(+,List(LONG, LONG)),List(CONST_LONG(3), CONST_LONG(0)),LONG)); TYPE: LONG"

    val expectedError =
      "Value 'x' already defined in the scope"

    log shouldBe expectedLog
    err shouldBe expectedError
    ctx.letDefs.contains("x") shouldBe true
  }

  property("successful on unused let") {
    ev[Long](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        CONST_LONG(3),
        LONG
      )) shouldBe Right(3)
  }

  property("successful on x = y") {
    ev[Long](
      expr = BLOCK(LET("x", CONST_LONG(3)),
                   BLOCK(
                     LET("y", REF("x", LONG)),
                     FUNCTION_CALL(sumLong.header, List(REF("x", LONG), REF("y", LONG)), LONG),
                     LONG
                   ),
                   LONG)) shouldBe Right(6)
  }

  property("successful on simple get") {
    ev[Long](expr = simpleDeclarationAndUsage(3)) shouldBe Right(3)
  }

  property("successful on get used further in expr") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        FUNCTION_CALL(eqLong.header, List(REF("x", LONG), CONST_LONG(2)), BOOLEAN),
        BOOLEAN
      )) shouldBe Right(false)
  }

  property("successful on multiple lets") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(LET("y", CONST_LONG(3)), FUNCTION_CALL(eqLong.header, List(REF("x", LONG), REF("y", LONG)), BOOLEAN), BOOLEAN),
        BOOLEAN
      )) shouldBe Right(true)
  }

  property("successful on multiple lets with expression") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(
          LET("y", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)), LONG)),
          FUNCTION_CALL(eqLong.header, List(REF("x", LONG), REF("y", LONG)), BOOLEAN),
          BOOLEAN
        ),
        BOOLEAN
      )) shouldBe Right(true)
  }

  property("successful on deep type resolution") {
    ev[Long](expr = IF(FUNCTION_CALL(eqLong.header, List(CONST_LONG(1), CONST_LONG(2)), BOOLEAN), simpleDeclarationAndUsage(3), CONST_LONG(4), LONG)) shouldBe Right(
      4)
  }

  property("successful on same value names in different branches") {
    val expr =
      IF(FUNCTION_CALL(eqLong.header, List(CONST_LONG(1), CONST_LONG(2)), BOOLEAN), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4), LONG)
    ev[Long](expr = expr) shouldBe Right(4)
  }

  property("fails if override") {
    ev[Long](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(
          LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)), LONG)),
          FUNCTION_CALL(eqLong.header, List(REF("x", LONG), CONST_LONG(1)), LONG),
          LONG
        ),
        LONG
      )) should produce("already defined")
  }

  property("fails if definition not found") {
    ev[Long](expr = FUNCTION_CALL(sumLong.header, List(REF("x", LONG), CONST_LONG(2)), LONG)) should produce("A definition of 'x' is not found")
  }

  property("custom type field access") {
    val pointType     = PredefType("Point", List("X" -> LONG, "Y"                           -> LONG))
    val pointInstance = Obj(Map("X"                  -> LazyVal(LONG)(EitherT.pure(3)), "Y" -> LazyVal(LONG)(EitherT.pure(4))))
    ev[Long](
      context = PureContext.instance |+| Context(
        typeDefs = Map(pointType.name -> pointType),
        letDefs = Map(("p", LazyVal(TYPEREF(pointType.name))(EitherT.pure(pointInstance)))),
        functions = Map.empty
      ),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p", TYPEREF("Point")), "X", LONG), CONST_LONG(2)), LONG)
    ) shouldBe Right(5)
  }

  property("lazy let evaluation doesn't throw if not used") {
    val pointType     = PredefType("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = Obj(Map(("X", LazyVal(LONG)(EitherT.pure(3))), ("Y", LazyVal(LONG)(EitherT.pure(4)))))
    val context = PureContext.instance |+| Context(
      typeDefs = Map((pointType.name, pointType)),
      letDefs = Map(("p", LazyVal(TYPEREF(pointType.name))(EitherT.pure(pointInstance))), ("badVal", LazyVal(LONG)(EitherT.leftT("Error")))),
      functions = Map.empty
    )
    ev[Long](
      context = context,
      expr = BLOCK(LET("Z", REF("badVal", LONG)),
                   FUNCTION_CALL(sumLong.header, List(GETTER(REF("p", TYPEREF("Point")), "X", LONG), CONST_LONG(2)), LONG),
                   LONG)
    ) shouldBe Right(5)
  }

  property("field and value are evaluated maximum once") {
    var fieldCalculated = 0
    var valueCalculated = 0

    val pointType = PredefType("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = Obj(Map(("X", LazyVal(LONG)(EitherT.pure {
      fieldCalculated = fieldCalculated + 1
      3
    })), ("Y", LazyVal(LONG)(EitherT.pure(4)))))
    val context = PureContext.instance |+| Context(
      typeDefs = Map((pointType.name, pointType)),
      letDefs = Map(("p", LazyVal(TYPEREF(pointType.name))(EitherT.pure(pointInstance))), ("h", LazyVal(LONG)(EitherT.pure {
        valueCalculated = valueCalculated + 1
        4
      }))),
      functions = Map.empty
    )
    ev[Long](
      context = context,
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p", TYPEREF("Point")), "X", LONG), GETTER(REF("p", TYPEREF("Point")), "X", LONG)), LONG)
    ) shouldBe Right(6)

    ev[Long](
      context = context,
      expr = FUNCTION_CALL(sumLong.header, List(REF("h", LONG), REF("h", LONG)), LONG)
    ) shouldBe Right(8)

    fieldCalculated shouldBe 1
    valueCalculated shouldBe 1
  }

  property("let is evaluated maximum once") {
    var functionEvaluated = 0

    val f = PredefFunction("F", 1, LONG, List(("_", LONG))) { _ =>
      functionEvaluated = functionEvaluated + 1
      Right(1L)
    }

    val context = PureContext.instance |+| Context(
      typeDefs = Map.empty,
      letDefs = Map.empty,
      functions = Map(f.header -> f)
    )
    ev[Long](
      context = context,
      expr = BLOCK(LET("X", FUNCTION_CALL(f.header, List(CONST_LONG(1000)), LONG)),
                   FUNCTION_CALL(sumLong.header, List(REF("X", LONG), REF("X", LONG)), LONG),
                   LONG)
    ) shouldBe Right(2L)

    functionEvaluated shouldBe 1

  }

  property("successful on ref getter evaluation") {
    val fooType = PredefType("Foo", List(("bar", STRING), ("buz", LONG)))

    val fooInstance =
      Obj(Map("bar" -> LazyVal(STRING)(EitherT.pure("bAr")), "buz" -> LazyVal(LONG)(EitherT.pure(1L))))

    val context = Context(
      typeDefs = Map(fooType.name -> fooType),
      letDefs = Map("fooInstance" -> LazyVal(fooType.typeRef)(EitherT.pure(fooInstance))),
      functions = Map.empty
    )

    val expr = GETTER(REF("fooInstance", fooType.typeRef), "bar", STRING)

    ev[String](context, expr) shouldBe Right("bAr")
  }

  property("successful on function call getter evaluation") {
    val fooType = PredefType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = PredefFunction("createFoo", 1, fooType.typeRef, List.empty) { _ =>
      Right(
        Obj(
          Map(
            "bar" -> LazyVal(STRING)(EitherT.pure("bAr")),
            "buz" -> LazyVal(LONG)(EitherT.pure(1L))
          )
        )
      )
    }

    val context = Context(
      typeDefs = Map(fooType.name -> fooType),
      letDefs = Map.empty,
      functions = Map(fooCtor.header -> fooCtor)
    )

    val expr = GETTER(FUNCTION_CALL(fooCtor.header, List.empty, fooType.typeRef), "bar", STRING)

    ev[String](context, expr) shouldBe Right("bAr")
  }

  property("successful on block getter evaluation") {
    val fooType = PredefType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = PredefFunction("createFoo", 1, fooType.typeRef, List.empty) { _ =>
      Right(
        Obj(
          Map(
            "bar" -> LazyVal(STRING)(EitherT.pure("bAr")),
            "buz" -> LazyVal(LONG)(EitherT.pure(1L))
          )
        )
      )
    }
    val fooTransform = PredefFunction("transformFoo", 1, fooType.typeRef, List(("foo", fooType.typeRef))) {
      case (fooObj: Obj) :: Nil => Right(fooObj.copy(fooObj.fields.updated("bar", LazyVal(STRING)(EitherT.pure("TRANSFORMED_BAR")))))
      case _                    => ???
    }

    val context = Context(
      typeDefs = Map(fooType.name -> fooType),
      letDefs = Map.empty,
      functions = Map(
        fooCtor.header      -> fooCtor,
        fooTransform.header -> fooTransform
      )
    )

    val expr = GETTER(
      BLOCK(
        LET("fooInstance", FUNCTION_CALL(fooCtor.header, List.empty, fooType.typeRef)),
        FUNCTION_CALL(fooTransform.header, List(REF("fooInstance", fooType.typeRef)), fooType.typeRef),
        fooType.typeRef
      ),
      "bar",
      STRING
    )

    ev[String](context, expr) shouldBe Right("TRANSFORMED_BAR")
  }

  property("successful on simple function evaluation") {
    ev[Long](
      context = Context(
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Map(multiplierFunction.header -> multiplierFunction)
      ),
      expr = FUNCTION_CALL(multiplierFunction.header, List(Typed.CONST_LONG(3), Typed.CONST_LONG(4)), LONG)
    ) shouldBe Right(12)
  }

  property("returns an success if sigVerify return a success") {
    val seed                    = "seed".getBytes()
    val (privateKey, publicKey) = Curve25519.createKeyPair(seed)

    val bodyBytes = "message".getBytes()
    val signature = Curve25519.sign(privateKey, bodyBytes)

    val r = sigVerifyTest(bodyBytes, publicKey, signature)
    r.isRight shouldBe true
  }

  property("returns an error if sigVerify return an error") {
    val seed           = "seed".getBytes()
    val (_, publicKey) = Curve25519.createKeyPair(seed)
    val bodyBytes      = "message".getBytes()

    val r = sigVerifyTest(bodyBytes, publicKey, Signature("signature".getBytes()))
    r.isLeft shouldBe false
  }

  private def sigVerifyTest(bodyBytes: Array[Byte],
                            publicKey: PublicKey,
                            signature: Signature): Either[(Context, ExecutionLog, ExecutionError), Boolean] = {
    val txType = PredefType(
      "Transaction",
      List(
        "bodyBytes" -> BYTEVECTOR,
        "senderPk"  -> BYTEVECTOR,
        "proof0"    -> BYTEVECTOR
      )
    )

    val txObj = Obj(
      Map(
        "bodyBytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(bodyBytes))),
        "senderPk"  -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(publicKey))),
        "proof0"    -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(signature)))
      ))

    val context = Monoid.combineAll(
      Seq(
        PureContext.instance,
        CryptoContext.build(Global),
        Context.build(
          types = Seq(txType),
          letDefs = Map("tx" -> LazyVal(TYPEREF(txType.name))(EitherT.pure(txObj))),
          functions = Seq.empty
        )
      ))

    ev[Boolean](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader("sigVerify", List(FunctionHeaderType.BYTEVECTOR, FunctionHeaderType.BYTEVECTOR, FunctionHeaderType.BYTEVECTOR)),
        args = List(
          GETTER(REF("tx", TYPEREF(txType.name)), "bodyBytes", BYTEVECTOR),
          GETTER(REF("tx", TYPEREF(txType.name)), "proof0", BYTEVECTOR),
          GETTER(REF("tx", TYPEREF(txType.name)), "senderPk", BYTEVECTOR)
        ),
        BOOLEAN
      )
    )
  }

  property("checking a hash of some message by crypto function invoking") {
    val bodyText      = "some text for test"
    val bodyBytes     = bodyText.getBytes()
    val hashFunctions = Map("sha256" -> Sha256, "blake2b256" -> Blake2b256, "keccak256" -> Keccak256)

    for ((funcName, funcClass) <- hashFunctions) hashFuncTest(bodyBytes, funcName) shouldBe Right(ByteVector(funcClass.hash(bodyText)))
  }

  private def hashFuncTest(bodyBytes: Array[Byte], funcName: String): Either[(Context, ExecutionLog, ExecutionError), ByteVector] = {
    val context = Monoid.combineAll(
      Seq(
        PureContext.instance,
        CryptoContext.build(Global)
      )
    )

    ev[ByteVector](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader(funcName, List(FunctionHeaderType.BYTEVECTOR)),
        args = List(Typed.CONST_BYTEVECTOR(ByteVector(bodyBytes))),
        BYTEVECTOR
      )
    )
  }
}
