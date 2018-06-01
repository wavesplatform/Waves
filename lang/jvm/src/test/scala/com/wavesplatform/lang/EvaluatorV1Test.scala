package com.wavesplatform.lang
import cats.data.EitherT
import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

class EvaluatorV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev[T: TypeInfo](context: EvaluationContext = PureContext.instance, expr: EXPR): (EvaluationContext, Either[ExecutionError, T]) =
    EvaluatorV1[T](context, expr)
  private def simpleDeclarationAndUsage(i: Int) = BLOCK(LET("x", CONST_LONG(i)), REF("x", LONG), LONG)

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_LONG(0))((acc, _) => FUNCTION_CALL(sumLong.header, List(acc, CONST_LONG(1)), LONG))

    ev[Long](expr = term)._2 shouldBe Right(100000)
  }

  property("return error and context of failed evaluation") {
    val (ctx, Left(err)) = ev[Long](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(
          LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)), LONG)),
          FUNCTION_CALL(eqLong.header, List(REF("z", LONG), CONST_LONG(1)), LONG),
          LONG
        ),
        LONG
      )
    )

    val expectedError = "A definition of 'z' not found"

    err shouldBe expectedError
    ctx.letDefs.contains("x") shouldBe true
  }

  property("successful on unused let") {
    ev[Long](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        CONST_LONG(3),
        LONG
      ))._2 shouldBe Right(3)
  }

  property("successful on x = y") {
    ev[Long](
      expr = BLOCK(LET("x", CONST_LONG(3)),
                   BLOCK(
                     LET("y", REF("x", LONG)),
                     FUNCTION_CALL(sumLong.header, List(REF("x", LONG), REF("y", LONG)), LONG),
                     LONG
                   ),
                   LONG))._2 shouldBe Right(6)
  }

  property("successful on simple get") {
    ev[Long](expr = simpleDeclarationAndUsage(3))._2 shouldBe Right(3)
  }

  property("successful on get used further in expr") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        FUNCTION_CALL(eqLong.header, List(REF("x", LONG), CONST_LONG(2)), BOOLEAN),
        BOOLEAN
      ))._2 shouldBe Right(false)
  }

  property("successful on multiple lets") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(LET("y", CONST_LONG(3)), FUNCTION_CALL(eqLong.header, List(REF("x", LONG), REF("y", LONG)), BOOLEAN), BOOLEAN),
        BOOLEAN
      ))._2 shouldBe Right(true)
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
      ))._2 shouldBe Right(true)
  }

  property("successful on deep type resolution") {
    ev[Long](expr = IF(FUNCTION_CALL(eqLong.header, List(CONST_LONG(1), CONST_LONG(2)), BOOLEAN), simpleDeclarationAndUsage(3), CONST_LONG(4), LONG))._2 shouldBe Right(
      4)
  }

  property("successful on same value names in different branches") {
    val expr =
      IF(FUNCTION_CALL(eqLong.header, List(CONST_LONG(1), CONST_LONG(2)), BOOLEAN), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4), LONG)
    ev[Long](expr = expr)._2 shouldBe Right(4)
  }

  property("fails if definition not found") {
    ev[Long](expr = FUNCTION_CALL(sumLong.header, List(REF("x", LONG), CONST_LONG(2)), LONG))._2 should produce("A definition of 'x' not found")
  }

  property("custom type field access") {
    val pointType     = PredefCaseType("Point", List("X"   -> LONG, "Y"         -> LONG))
    val pointInstance = CaseObj(pointType.typeRef, Map("X" -> Val(LONG)(3), "Y" -> Val(LONG)(4)))
    ev[Long](
      context = PureContext.instance |+| EvaluationContext(
        letDefs = Map(("p", LazyVal(EitherT.pure(pointInstance)))),
        functions = Map.empty
      ),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p", pointType.typeRef), "X", LONG), CONST_LONG(2)), LONG)
    )._2 shouldBe Right(5)
  }

  property("lazy let evaluation doesn't throw if not used") {
    val pointType     = PredefCaseType("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = CaseObj(pointType.typeRef, Map("X" -> Val(LONG)(3), "Y" -> Val(LONG)(4)))
    val context = PureContext.instance |+| EvaluationContext(
      letDefs = Map(("p", LazyVal(EitherT.pure(pointInstance))), ("badVal", LazyVal(EitherT.leftT("Error")))),
      functions = Map.empty
    )
    ev[Long](
      context = context,
      expr = BLOCK(LET("Z", REF("badVal", LONG)),
                   FUNCTION_CALL(sumLong.header, List(GETTER(REF("p", pointType.typeRef), "X", LONG), CONST_LONG(2)), LONG),
                   LONG)
    )._2 shouldBe Right(5)
  }

  property("let is evaluated maximum once") {
    var functionEvaluated = 0

    val f = PredefFunction("F", 1, LONG, List(("_", LONG))) { _ =>
      functionEvaluated = functionEvaluated + 1
      Right(1L)
    }

    val context = PureContext.instance |+| EvaluationContext(
      letDefs = Map.empty,
      functions = Map(f.header -> f)
    )
    ev[Long](
      context = context,
      expr = BLOCK(LET("X", FUNCTION_CALL(f.header, List(CONST_LONG(1000)), LONG)),
                   FUNCTION_CALL(sumLong.header, List(REF("X", LONG), REF("X", LONG)), LONG),
                   LONG)
    )._2 shouldBe Right(2L)

    functionEvaluated shouldBe 1

  }

  property("successful on ref getter evaluation") {
    val fooType = PredefCaseType("Foo", List(("bar", STRING), ("buz", LONG)))

    val fooInstance = CaseObj(fooType.typeRef, Map("bar" -> Val(STRING)("bAr"), "buz" -> Val(LONG)(1L)))

    val context = EvaluationContext(
      letDefs = Map("fooInstance" -> LazyVal(EitherT.pure(fooInstance))),
      functions = Map.empty
    )

    val expr = GETTER(REF("fooInstance", fooType.typeRef), "bar", STRING)

    ev[String](context, expr)._2 shouldBe Right("bAr")
  }

  property("successful on function call getter evaluation") {
    val fooType = PredefCaseType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = PredefFunction("createFoo", 1, fooType.typeRef, List.empty) { _ =>
      Right(
        CaseObj(fooType.typeRef, Map("bar" -> Val(STRING)("bAr"), "buz" -> Val(LONG)(1L)))
      )

    }

    val context = EvaluationContext(
      letDefs = Map.empty,
      functions = Map(fooCtor.header -> fooCtor)
    )

    val expr = GETTER(FUNCTION_CALL(fooCtor.header, List.empty, fooType.typeRef), "bar", STRING)

    ev[String](context, expr)._2 shouldBe Right("bAr")
  }

  property("successful on block getter evaluation") {
    val fooType = PredefCaseType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = PredefFunction("createFoo", 1, fooType.typeRef, List.empty) { _ =>
      Right(
        CaseObj(fooType.typeRef,
                Map(
                  "bar" -> Val(STRING)("bAr"),
                  "buz" -> Val(LONG)(1L)
                )))
    }
    val fooTransform = PredefFunction("transformFoo", 1, fooType.typeRef, List(("foo", fooType.typeRef))) {
      case (fooObj: CaseObj) :: Nil => Right(fooObj.copy(fields = fooObj.fields.updated("bar", Val(STRING)("TRANSFORMED_BAR"))))
      case _                        => ???
    }

    val context = EvaluationContext(
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

    ev[String](context, expr)._2 shouldBe Right("TRANSFORMED_BAR")
  }

  property("successful on simple function evaluation") {
    ev[Long](
      context = EvaluationContext(
        letDefs = Map.empty,
        functions = Map(multiplierFunction.header -> multiplierFunction)
      ),
      expr = FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(3), CONST_LONG(4)), LONG)
    )._2 shouldBe Right(12)
  }

  property("returns an success if sigVerify return a success") {
    val seed                    = "seed".getBytes()
    val (privateKey, publicKey) = Curve25519.createKeyPair(seed)

    val bodyBytes = "message".getBytes()
    val signature = Curve25519.sign(privateKey, bodyBytes)

    val r = sigVerifyTest(bodyBytes, publicKey, signature)._2
    r.isRight shouldBe true
  }

  property("returns correct context") {
    val (alicePrivateKey, _)          = Curve25519.createKeyPair("seed0".getBytes())
    val (bobPrivateKey, bobPublicKey) = Curve25519.createKeyPair("seed1".getBytes())
    val (_, senderPublicKey)          = Curve25519.createKeyPair("seed2".getBytes())

    val bodyBytes = "message".getBytes()

    val (ctx, result) = multiSig(
      bodyBytes,
      senderPublicKey,
      bobPublicKey,
      bobPublicKey,
      Curve25519.sign(alicePrivateKey, bodyBytes),
      Curve25519.sign(bobPrivateKey, bodyBytes)
    )

    result shouldBe Right(false)

    //it false, because script fails on Alice's signature check, and bobSigned is not evaluated
    ctx.letDefs("bobSigned").evaluated.read() shouldBe false

    ctx.letDefs("aliceSigned").evaluated.read() shouldBe true
    ctx.letDefs("aliceSigned").value.value() shouldBe Right(false)
  }

  property("returns an error if sigVerify return an error") {
    val seed           = "seed".getBytes()
    val (_, publicKey) = Curve25519.createKeyPair(seed)
    val bodyBytes      = "message".getBytes()

    val r = sigVerifyTest(bodyBytes, publicKey, Signature("signature".getBytes()))._2
    r.isLeft shouldBe false
  }

  private def sigVerifyTest(bodyBytes: Array[Byte],
                            publicKey: PublicKey,
                            signature: Signature): (EvaluationContext, Either[ExecutionError, Boolean]) = {
    val txType = PredefCaseType(
      "Transaction",
      List(
        "bodyBytes" -> BYTEVECTOR,
        "senderPk"  -> BYTEVECTOR,
        "proof0"    -> BYTEVECTOR
      )
    )

    val txObj = CaseObj(
      txType.typeRef,
      Map(
        "bodyBytes" -> Val(BYTEVECTOR)(ByteVector(bodyBytes)),
        "senderPk"  -> Val(BYTEVECTOR)(ByteVector(publicKey)),
        "proof0"    -> Val(BYTEVECTOR)(ByteVector(signature))
      )
    )

    val context = Monoid.combineAll(
      Seq(
        PureContext.instance,
        CryptoContext.build(Global),
        EvaluationContext.build(
          letDefs = Map("tx" -> LazyVal(EitherT.pure(txObj))),
          functions = Seq.empty
        )
      ))

    ev[Boolean](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader("sigVerify", List(FunctionHeaderType.BYTEVECTOR, FunctionHeaderType.BYTEVECTOR, FunctionHeaderType.BYTEVECTOR)),
        args = List(
          GETTER(REF("tx", txType.typeRef), "bodyBytes", BYTEVECTOR),
          GETTER(REF("tx", txType.typeRef), "proof0", BYTEVECTOR),
          GETTER(REF("tx", txType.typeRef), "senderPk", BYTEVECTOR)
        ),
        BOOLEAN
      )
    )
  }

  private def multiSig(bodyBytes: Array[Byte],
                       senderPK: PublicKey,
                       alicePK: PublicKey,
                       bobPK: PublicKey,
                       aliceProof: Signature,
                       bobProof: Signature): (EvaluationContext, Either[ExecutionError, Boolean]) = {
    val txType = PredefCaseType(
      "Transaction",
      List(
        "bodyBytes" -> BYTEVECTOR,
        "senderPk"  -> BYTEVECTOR,
        "proof0"    -> BYTEVECTOR,
        "proof1"    -> BYTEVECTOR
      )
    )

    val txObj = CaseObj(
      txType.typeRef,
      Map(
        "bodyBytes" -> Val(BYTEVECTOR)(ByteVector(bodyBytes)),
        "senderPk"  -> Val(BYTEVECTOR)(ByteVector(senderPK)),
        "proof0"    -> Val(BYTEVECTOR)(ByteVector(aliceProof)),
        "proof1"    -> Val(BYTEVECTOR)(ByteVector(bobProof))
      )
    )

    val context = Monoid.combineAll(
      Seq(
        PureContext.instance,
        CryptoContext.build(Global),
        EvaluationContext.build(
          letDefs = Map(
            "tx"          -> LazyVal(EitherT.pure(txObj)),
            "alicePubKey" -> LazyVal(EitherT.pure(ByteVector(alicePK))),
            "bobPubKey"   -> LazyVal(EitherT.pure(ByteVector(bobPK)))
          ),
          functions = Seq.empty
        )
      ))

    val compilerContext =
      CompilerContext.fromEvaluationContext(context,
                                            Map(txType.name -> txType),
                                            Map("tx"        -> txType.typeRef, "alicePubKey" -> BYTEVECTOR, "bobPubKey" -> BYTEVECTOR))

    val script =
      s"""
         |let aliceSigned  = sigVerify(tx.bodyBytes, tx.proof0, alicePubKey)
         |let bobSigned    = sigVerify(tx.bodyBytes, tx.proof1, bobPubKey  )
         |
         |aliceSigned && bobSigned
   """.stripMargin

    ev[Boolean](
      context = context,
      expr = new CompilerV1(compilerContext).compile(script, List.empty).explicitGet()
    )
  }

  property("checking a hash of some message by crypto function invoking") {
    val bodyText      = "some text for test"
    val bodyBytes     = bodyText.getBytes()
    val hashFunctions = Map("sha256" -> Sha256, "blake2b256" -> Blake2b256, "keccak256" -> Keccak256)

    for ((funcName, funcClass) <- hashFunctions) hashFuncTest(bodyBytes, funcName)._2 shouldBe Right(ByteVector(funcClass.hash(bodyText)))
  }

  private def hashFuncTest(bodyBytes: Array[Byte], funcName: String): (EvaluationContext, Either[ExecutionError, ByteVector]) = {
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
        args = List(CONST_BYTEVECTOR(ByteVector(bodyBytes))),
        BYTEVECTOR
      )
    )
  }
}
