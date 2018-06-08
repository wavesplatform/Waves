package com.wavesplatform.lang

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

class EvaluatorV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev[T](context: EvaluationContext = PureContext.evalContext, expr: EXPR): (EvaluationContext, Either[ExecutionError, T]) =
    EvaluatorV1[T](context, expr)

  private def simpleDeclarationAndUsage(i: Int) = BLOCK(LET("x", CONST_LONG(i)), REF("x"))

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_LONG(0))((acc, _) => FUNCTION_CALL(sumLong.header, List(acc, CONST_LONG(1))))

    ev[Long](expr = term)._2 shouldBe Right(100000)
  }

  property("return error and context of failed evaluation") {
    val (ctx, Left(err)) = ev[Long](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(
          LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
          FUNCTION_CALL(PureContext.eq.header, List(REF("z"), CONST_LONG(1)))
        )
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
        CONST_LONG(3)
      ))._2 shouldBe Right(3)
  }

  property("successful on x = y") {
    ev[Long](
      expr = BLOCK(LET("x", CONST_LONG(3)),
                   BLOCK(
                     LET("y", REF("x")),
                     FUNCTION_CALL(sumLong.header, List(REF("x"), REF("y")))
                   )))._2 shouldBe Right(6)
  }

  property("successful on simple get") {
    ev[Long](expr = simpleDeclarationAndUsage(3))._2 shouldBe Right(3)
  }

  property("successful on get used further in expr") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        FUNCTION_CALL(PureContext.eq.header, List(REF("x"), CONST_LONG(2)))
      ))._2 shouldBe Right(false)
  }

  property("successful on multiple lets") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(LET("y", CONST_LONG(3)), FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y"))))
      ))._2 shouldBe Right(true)
  }

  property("successful on multiple lets with expression") {
    ev[Boolean](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(
          LET("y", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
          FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y")))
        )
      ))._2 shouldBe Right(true)
  }

  property("successful on deep type resolution") {
    ev[Long](expr = IF(FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))), simpleDeclarationAndUsage(3), CONST_LONG(4)))._2 shouldBe Right(
      4)
  }

  property("successful on same value names in different branches") {
    val expr =
      IF(FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4))
    ev[Long](expr = expr)._2 shouldBe Right(4)
  }

  property("fails if definition not found") {
    ev[Long](expr = FUNCTION_CALL(sumLong.header, List(REF("x"), CONST_LONG(2))))._2 should produce("A definition of 'x' not found")
  }

  property("custom type field access") {
    val pointType     = CaseType("Point", List("X"         -> LONG, "Y" -> LONG))
    val pointInstance = CaseObj(pointType.typeRef, Map("X" -> 3L, "Y"   -> 4L))
    ev[Long](
      context = Monoid.combine(PureContext.evalContext,
                               EvaluationContext(
                                 letDefs = Map(("p", LazyVal(EitherT.pure(pointInstance)))),
                                 functions = Map.empty
                               )),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2)))
    )._2 shouldBe Right(5)
  }

  property("lazy let evaluation doesn't throw if not used") {
    val pointType     = CaseType("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = CaseObj(pointType.typeRef, Map("X" -> 3L, "Y" -> 4L))
    val context = Monoid.combine(
      PureContext.evalContext,
      EvaluationContext(
        letDefs = Map(("p", LazyVal(EitherT.pure(pointInstance))), ("badVal", LazyVal(EitherT.leftT("Error")))),
        functions = Map.empty
      )
    )
    ev[Long](
      context = context,
      expr = BLOCK(LET("Z", REF("badVal")), FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2))))
    )._2 shouldBe Right(5)
  }

  property("let is evaluated maximum once") {
    var functionEvaluated = 0

    val f = PredefFunction("F", 1, 258, List("_" -> LONG), LONG) { _ =>
      functionEvaluated = functionEvaluated + 1
      Right(1L)
    }

    val context = Monoid.combine(PureContext.evalContext,
                                 EvaluationContext(
                                   letDefs = Map.empty,
                                   functions = Map(f.header -> f)
                                 ))
    ev[Long](
      context = context,
      expr = BLOCK(LET("X", FUNCTION_CALL(f.header, List(CONST_LONG(1000)))), FUNCTION_CALL(sumLong.header, List(REF("X"), REF("X"))))
    )._2 shouldBe Right(2L)

    functionEvaluated shouldBe 1

  }

  property("successful on ref getter evaluation") {
    val fooType = CaseType("Foo", List(("bar", STRING), ("buz", LONG)))

    val fooInstance = CaseObj(fooType.typeRef, Map("bar" -> "bAr", "buz" -> 1L))

    val context = EvaluationContext(
      letDefs = Map("fooInstance" -> LazyVal(EitherT.pure(fooInstance))),
      functions = Map.empty
    )

    val expr = GETTER(REF("fooInstance"), "bar")

    ev[String](context, expr)._2 shouldBe Right("bAr")
  }

  property("successful on function call getter evaluation") {
    val fooType = CaseType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = PredefFunction("createFoo", 1, 259, List.empty, fooType.typeRef) { _ =>
      Right(
        CaseObj(fooType.typeRef, Map("bar" -> "bAr", "buz" -> 1L))
      )
    }

    val context = EvaluationContext(
      letDefs = Map.empty,
      functions = Map(fooCtor.header -> fooCtor)
    )

    val expr = GETTER(FUNCTION_CALL(fooCtor.header, List.empty), "bar")

    ev[String](context, expr)._2 shouldBe Right("bAr")
  }

  property("successful on block getter evaluation") {
    val fooType = CaseType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = PredefFunction("createFoo", 1, 259, List.empty, fooType.typeRef) { _ =>
      Right(
        CaseObj(
          fooType.typeRef,
          Map(
            "bar" -> "bAr",
            "buz" -> 1L
          )
        ))
    }
    val fooTransform = PredefFunction("transformFoo", 1, 260, List("foo" -> fooType.typeRef), fooType.typeRef) {
      case (fooObj: CaseObj) :: Nil => Right(fooObj.copy(fields = fooObj.fields.updated("bar", "TRANSFORMED_BAR")))
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
        LET("fooInstance", FUNCTION_CALL(fooCtor.header, List.empty)),
        FUNCTION_CALL(fooTransform.header, List(REF("fooInstance")))
      ),
      "bar"
    )

    ev[String](context, expr)._2 shouldBe Right("TRANSFORMED_BAR")
  }

  property("successful on simple function evaluation") {
    ev[Long](
      context = EvaluationContext(
        letDefs = Map.empty,
        functions = Map(multiplierFunction.header -> multiplierFunction)
      ),
      expr = FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(3), CONST_LONG(4)))
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
    val txType = CaseType(
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
        "bodyBytes" -> ByteVector(bodyBytes),
        "senderPk"  -> ByteVector(publicKey),
        "proof0"    -> ByteVector(signature)
      )
    )

    val context = Monoid.combineAll(
      Seq(
        PureContext.evalContext,
        CryptoContext.evalContext(Global),
        EvaluationContext.build(
          letDefs = Map("tx" -> LazyVal(EitherT.pure(txObj))),
          functions = Seq.empty
        )
      ))

    ev[Boolean](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader.Predef(SIGVERIFY),
        args = List(
          GETTER(REF("tx"), "bodyBytes"),
          GETTER(REF("tx"), "proof0"),
          GETTER(REF("tx"), "senderPk")
        )
      )
    )
  }

  private def multiSig(bodyBytes: Array[Byte],
                       senderPK: PublicKey,
                       alicePK: PublicKey,
                       bobPK: PublicKey,
                       aliceProof: Signature,
                       bobProof: Signature): (EvaluationContext, Either[ExecutionError, Boolean]) = {
    val txType = CaseType(
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
        "bodyBytes" -> ByteVector(bodyBytes),
        "senderPk"  -> ByteVector(senderPK),
        "proof0"    -> ByteVector(aliceProof),
        "proof1"    -> ByteVector(bobProof)
      )
    )

    val vars: Map[String, (TYPE, LazyVal)] = Map(
      ("tx", (txType.typeRef, LazyVal(EitherT.pure(txObj)))),
      ("alicePubKey", (BYTEVECTOR, LazyVal(EitherT.pure(ByteVector(alicePK))))),
      ("bobPubKey", (BYTEVECTOR, LazyVal(EitherT.pure(ByteVector(bobPK)))))
    )

    val context = Monoid.combineAll(
      Seq(
        PureContext.ctx,
        CryptoContext.build(Global),
        CTX(Seq(txType), vars, Seq.empty)
      ))

    val script =
      s"""
         |let aliceSigned  = sigVerify(tx.bodyBytes, tx.proof0, alicePubKey)
         |let bobSigned    = sigVerify(tx.bodyBytes, tx.proof1, bobPubKey  )
         |
         |aliceSigned && bobSigned
   """.stripMargin

    ev[Boolean](context.evaluationContext, new CompilerV1(context.compilerContext).compile(script, List.empty).explicitGet())
  }

  property("checking a hash of some message by crypto function invoking") {
    val bodyText      = "some text for test"
    val bodyBytes     = bodyText.getBytes()
    val hashFunctions = Map(SHA256 -> Sha256, BLAKE256 -> Blake2b256, KECCAK256 -> Keccak256)

    for ((funcName, funcClass) <- hashFunctions) hashFuncTest(bodyBytes, funcName)._2 shouldBe Right(ByteVector(funcClass.hash(bodyText)))
  }

  private def hashFuncTest(bodyBytes: Array[Byte], funcName: Short): (EvaluationContext, Either[ExecutionError, ByteVector]) = {
    val context = Monoid.combineAll(
      Seq(
        PureContext.evalContext,
        CryptoContext.evalContext(Global)
      )
    )

    ev[ByteVector](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader.Predef(funcName),
        args = List(CONST_BYTEVECTOR(ByteVector(bodyBytes)))
      )
    )
  }

  property("math functions") {
    val sum   = FUNCTION_CALL(sumLong.header, List(CONST_LONG(5), CONST_LONG(5)))
    val mul   = FUNCTION_CALL(mulLong.header, List(CONST_LONG(5), CONST_LONG(5)))
    val div   = FUNCTION_CALL(divLong.header, List(CONST_LONG(10), CONST_LONG(3)))
    val mod   = FUNCTION_CALL(modLong.header, List(CONST_LONG(10), CONST_LONG(3)))
    val frac  = FUNCTION_CALL(fraction.header, List(CONST_LONG(Long.MaxValue), CONST_LONG(2), CONST_LONG(4)))
    val frac2 = FUNCTION_CALL(fraction.header, List(CONST_LONG(Long.MaxValue), CONST_LONG(3), CONST_LONG(2)))
    val frac3 = FUNCTION_CALL(fraction.header, List(CONST_LONG(-Long.MaxValue), CONST_LONG(3), CONST_LONG(2)))

    ev[Long](expr = sum)._2 shouldBe Right(10)
    ev[Long](expr = mul)._2 shouldBe Right(25)
    ev[Long](expr = div)._2 shouldBe Right(3)
    ev[Long](expr = mod)._2 shouldBe Right(1)
    ev[Long](expr = frac)._2 shouldBe Right(Long.MaxValue / 2)
    ev[Long](expr = frac2)._2 shouldBe Left(s"Long overflow: value `${BigInt(Long.MaxValue) * 3 / 2}` greater than 2^63-1")
    ev[Long](expr = frac3)._2 shouldBe Left(s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1")
  }
}
