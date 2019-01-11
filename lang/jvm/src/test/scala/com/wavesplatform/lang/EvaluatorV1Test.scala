package com.wavesplatform.lang

import java.nio.ByteBuffer

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.ExprEvaluator.Log
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.Testing._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, EnvironmentFunctions, PureContext, _}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.utils.{Base58, Base64}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.util.Try

class EvaluatorV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private val pureContext = PureContext.build(V1)

  private val defaultCryptoContext = CryptoContext.build(Global)

  private def defaultFullContext(environment: Environment): CTX = Monoid.combineAll(
    Seq(
      defaultCryptoContext,
      pureContext,
      WavesContext.build(V1, environment, isTokenContext = false)
    )
  )

  private val pureEvalContext: EvaluationContext = PureContext.build(V1).evaluationContext

  private def ev[T <: EVALUATED](context: EvaluationContext = pureEvalContext, expr: EXPR): Either[ExecutionError, T] =
    EvaluatorV1[T](context, expr)

  private def simpleDeclarationAndUsage(i: Int) = BLOCK(LET("x", CONST_LONG(i)), REF("x"))

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_LONG(0))((acc, _) => FUNCTION_CALL(sumLong.header, List(acc, CONST_LONG(1))))

    ev(expr = term) shouldBe evaluated(100000)
  }

  property("return error and log of failed evaluation") {
    val (log, Left(err)) = EvaluatorV1.applywithLogging[EVALUATED](
      pureEvalContext,
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
    log.isEmpty shouldBe true
  }

  property("successful on unused let") {
    ev[EVALUATED](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        CONST_LONG(3)
      )) shouldBe evaluated(3)
  }

  property("successful on x = y") {
    ev[EVALUATED](
      expr = BLOCK(LET("x", CONST_LONG(3)),
                   BLOCK(
                     LET("y", REF("x")),
                     FUNCTION_CALL(sumLong.header, List(REF("x"), REF("y")))
                   ))) shouldBe evaluated(6)
  }

  property("successful on simple get") {
    ev[EVALUATED](expr = simpleDeclarationAndUsage(3)) shouldBe evaluated(3)
  }

  property("successful on get used further in expr") {
    ev[EVALUATED](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        FUNCTION_CALL(PureContext.eq.header, List(REF("x"), CONST_LONG(2)))
      )) shouldBe evaluated(false)
  }

  property("successful on multiple lets") {
    ev[EVALUATED](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(LET("y", CONST_LONG(3)), FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y"))))
      )) shouldBe evaluated(true)
  }

  property("successful on multiple lets with expression") {
    ev[EVALUATED](
      expr = BLOCK(
        LET("x", CONST_LONG(3)),
        BLOCK(
          LET("y", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
          FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y")))
        )
      )) shouldBe evaluated(true)
  }

  property("successful on deep type resolution") {
    ev[EVALUATED](expr = IF(FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))), simpleDeclarationAndUsage(3), CONST_LONG(4))) shouldBe evaluated(
      4)
  }

  property("successful on same value names in different branches") {
    val expr =
      IF(FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4))
    ev[EVALUATED](expr = expr) shouldBe evaluated(4)
  }

  property("fails if definition not found") {
    ev[EVALUATED](expr = FUNCTION_CALL(sumLong.header, List(REF("x"), CONST_LONG(2)))) should produce("A definition of 'x' not found")
  }

  property("custom type field access") {
    val pointType     = CaseType("Point", List("X"         -> LONG, "Y" -> LONG))
    val pointInstance = CaseObj(pointType.typeRef, Map("X" -> 3L, "Y"   -> 4L))
    ev[EVALUATED](
      context = Monoid.combine(pureEvalContext,
                               EvaluationContext(
                                 typeDefs = Map.empty,
                                 letDefs = Map(("p", LazyVal(EitherT.pure(pointInstance)))),
                                 functions = Map.empty
                               )),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2)))
    ) shouldBe evaluated(5)
  }

  property("ne works") {
    ev[EVALUATED](
      expr = FUNCTION_CALL(FunctionHeader.User(PureContext.ne.name), List(CONST_LONG(1), CONST_LONG(2)))
    ) shouldBe evaluated(true)

    ev[EVALUATED](
      expr = FUNCTION_CALL(FunctionHeader.User(PureContext.ne.name), List(CONST_LONG(1), CONST_LONG(1)))
    ) shouldBe evaluated(false)
  }

  property("lazy let evaluation doesn't throw if not used") {
    val pointType     = CaseType("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = CaseObj(pointType.typeRef, Map("X" -> 3L, "Y" -> 4L))
    val context = Monoid.combine(
      pureEvalContext,
      EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map(("p", LazyVal(EitherT.pure(pointInstance))), ("badVal", LazyVal(EitherT.leftT("Error")))),
        functions = Map.empty
      )
    )
    ev[EVALUATED](
      context = context,
      expr = BLOCK(LET("Z", REF("badVal")), FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2))))
    ) shouldBe evaluated(5)
  }

  property("let is evaluated maximum once") {
    var functionEvaluated = 0

    val f = NativeFunction("F", 1: Long, 258: Short, LONG: TYPE, "test function", Seq(("_", LONG, "")): _*) { _ =>
      functionEvaluated = functionEvaluated + 1
      evaluated(1L)
    }

    val context = Monoid.combine(pureEvalContext,
                                 EvaluationContext(
                                   typeDefs = Map.empty,
                                   letDefs = Map.empty,
                                   functions = Map(f.header -> f)
                                 ))
    ev[EVALUATED](
      context = context,
      expr = BLOCK(LET("X", FUNCTION_CALL(f.header, List(CONST_LONG(1000)))), FUNCTION_CALL(sumLong.header, List(REF("X"), REF("X"))))
    ) shouldBe evaluated(2L)

    functionEvaluated shouldBe 1
  }

  property("successful on ref getter evaluation") {
    val fooType = CaseType("Foo", List(("bar", STRING), ("buz", LONG)))

    val fooInstance = CaseObj(fooType.typeRef, Map("bar" -> "bAr", "buz" -> 1L))

    val context = EvaluationContext(
      typeDefs = Map.empty,
      letDefs = Map("fooInstance" -> LazyVal(EitherT.pure(fooInstance))),
      functions = Map.empty
    )

    val expr = GETTER(REF("fooInstance"), "bar")

    ev[EVALUATED](context, expr) shouldBe evaluated("bAr")
  }

  property("successful on function call getter evaluation") {
    val fooType = CaseType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = NativeFunction("createFoo", 1: Long, 259: Short, fooType.typeRef, "test function", List.empty: _*) { _ =>
      evaluated(CaseObj(fooType.typeRef, Map("bar" -> "bAr", "buz" -> 1L)))
    }

    val context = EvaluationContext(
      typeDefs = Map.empty,
      letDefs = Map.empty,
      functions = Map(fooCtor.header -> fooCtor)
    )

    val expr = GETTER(FUNCTION_CALL(fooCtor.header, List.empty), "bar")

    ev[EVALUATED](context, expr) shouldBe evaluated("bAr")
  }

  property("successful on block getter evaluation") {
    val fooType = CaseType("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = NativeFunction("createFoo", 1: Long, 259: Short, fooType.typeRef, "test function", List.empty: _*) { _ =>
      evaluated(
        CaseObj(
          fooType.typeRef,
          Map(
            "bar" -> "bAr",
            "buz" -> 1L
          )
        ))
    }
    val fooTransform =
      NativeFunction("transformFoo", 1: Long, 260: Short, fooType.typeRef, "test function", ("foo", fooType.typeRef, "foo")) {
        case (fooObj: CaseObj) :: Nil => evaluated(fooObj.copy(fields = fooObj.fields.updated("bar", "TRANSFORMED_BAR")))
        case _                        => ???
      }

    val context = EvaluationContext(
      typeDefs = Map.empty,
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

    ev[EVALUATED](context, expr) shouldBe evaluated("TRANSFORMED_BAR")
  }

  property("successful on simple function evaluation") {
    ev[EVALUATED](
      context = EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Map(multiplierFunction.header -> multiplierFunction)
      ),
      expr = FUNCTION_CALL(multiplierFunction.header, List(CONST_LONG(3), CONST_LONG(4)))
    ) shouldBe evaluated(12)
  }

  property("returns an success if sigVerify return a success") {
    val seed                    = "seed".getBytes()
    val (privateKey, publicKey) = Curve25519.createKeyPair(seed)

    val bodyBytes = "message".getBytes()
    val signature = Curve25519.sign(privateKey, bodyBytes)

    val r = sigVerifyTest(bodyBytes, publicKey, signature)
    r.isRight shouldBe true
  }

  property("returns correct context") {
    val (alicePrivateKey, _)          = Curve25519.createKeyPair("seed0".getBytes())
    val (bobPrivateKey, bobPublicKey) = Curve25519.createKeyPair("seed1".getBytes())
    val (_, senderPublicKey)          = Curve25519.createKeyPair("seed2".getBytes())

    val bodyBytes = "message".getBytes()

    val (log, result) = multiSig(
      bodyBytes,
      senderPublicKey,
      bobPublicKey,
      bobPublicKey,
      Curve25519.sign(alicePrivateKey, bodyBytes),
      Curve25519.sign(bobPrivateKey, bodyBytes)
    )

    result shouldBe Right(false)

    //it false, because script fails on Alice's signature check, and bobSigned is not evaluated
    log.find(_._1 == "bobSigned") shouldBe None
    log.find(_._1 == "aliceSigned") shouldBe Some(("aliceSigned", evaluated(false)))
  }

  property("returns an error if sigVerify return an error") {
    val seed           = "seed".getBytes()
    val (_, publicKey) = Curve25519.createKeyPair(seed)
    val bodyBytes      = "message".getBytes()

    val r = sigVerifyTest(bodyBytes, publicKey, Signature("signature".getBytes()))
    r.isLeft shouldBe false
  }

  private val genBytesAndNumber = for {
    xs     <- Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
    number <- Arbitrary.arbInt.arbitrary
  } yield (ByteVector(xs), number)

  property("drop(ByteVector, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.dropBytes.header, List(CONST_BYTEVECTOR(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.drop(number))
    }
  }

  property("take(ByteVector, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(FunctionHeader.Native(TAKE_BYTES), List(CONST_BYTEVECTOR(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.take(number))
    }
  }

  property("dropRightBytes(ByteVector, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.dropRightBytes.header, List(CONST_BYTEVECTOR(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.dropRight(number))
    }
  }

  property("takeRightBytes(ByteVector, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.takeRightBytes.header, List(CONST_BYTEVECTOR(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.takeRight(number))
    }
  }

  private val genStringAndNumber = for {
    xs     <- Arbitrary.arbString.arbitrary
    number <- Arbitrary.arbInt.arbitrary
  } yield (xs, number)

  property("drop(String, Long) works as the native one") {
    forAll(genStringAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(FunctionHeader.Native(DROP_STRING), List(CONST_STRING(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.drop(number))
    }
  }

  property("take(String, Long) works as the native one") {
    forAll(genStringAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(FunctionHeader.Native(TAKE_STRING), List(CONST_STRING(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.take(number))
    }
  }

  property("dropRight(String, Long) works as the native one") {
    forAll(genStringAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.dropRightString.header, List(CONST_STRING(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.dropRight(number))
    }
  }

  property("takeRight(String, Long) works as the native one") {
    forAll(genStringAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.takeRightString.header, List(CONST_STRING(xs), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.takeRight(number))
    }
  }

  property("size(String) works as the native one") {
    forAll(Arbitrary.arbString.arbitrary) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(SIZE_STRING), List(CONST_STRING(xs)))
      val actual = ev[EVALUATED](pureEvalContext, expr)
      actual shouldBe evaluated(xs.length)
    }
  }

  property("fromBase58String(String) works as the native one") {
    val gen = for {
      len <- Gen.choose(0, Global.MaxBase58Bytes)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield Base58.encode(xs)

    forAll(gen) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(CONST_STRING(xs)))
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteVector(Base58.decode(xs).get))
    }
  }

  property("fromBase58String(String) input is 100 chars max") {
    import Global.{MaxBase58String => Max}
    val gen = for {
      len <- Gen.choose(Max + 1, Max * 2)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield Base58.encode(xs)

    forAll(gen) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(CONST_STRING(xs)))
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe Left("base58Decode input exceeds 100")
    }
  }

  property("fromBase64String(String) works as the native one: without prefix") {
    val gen = for {
      len <- Gen.choose(0, 512)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield Base64.encode(xs)

    forAll(gen) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE64), List(CONST_STRING(xs)))
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteVector(Base64.decode(xs).get))
    }
  }

  property("fromBase64String(String) works as the native one: with prefix") {
    val gen = for {
      len <- Gen.choose(0, 512)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield s"base64:${Base64.encode(xs)}"

    forAll(gen) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE64), List(CONST_STRING(xs)))
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteVector(Base64.decode(xs).get))
    }
  }

  property("addressFromPublicKey works as the native one") {
    val environment = emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      pk
    }

    forAll(gen) { pkBytes =>
      val expr = FUNCTION_CALL(
        FunctionHeader.User("addressFromPublicKey"),
        List(CONST_BYTEVECTOR(ByteVector(pkBytes)))
      )

      val actual = ev[CaseObj](ctx.evaluationContext, expr).map(_.fields("bytes"))
      actual shouldBe evaluated(ByteVector(addressFromPublicKey(environment.networkByte, pkBytes)))
    }
  }

  property("addressFromString works as the native one: sunny without prefix") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      Base58.encode(addressFromPublicKey(environment.networkByte, pk))
    }

    forAll(gen) { addrStr =>
      val expr                                   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr)))
      val actual                                 = ev[CaseObj](ctx.evaluationContext, expr)
      val a: Either[ExecutionError, EVALUATED]   = actual.map(_.fields("bytes"))
      val e: Either[String, Option[Array[Byte]]] = addressFromString(environment.networkByte, addrStr)
      a.explicitGet() shouldBe CONST_BYTEVECTOR(ByteVector(e.explicitGet().get))
    }
  }

  property("addressFromString works as the native one: sunny with prefix") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.networkByte, pk))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr)))
      val actual = ev[CaseObj](ctx.evaluationContext, expr)
      val e      = addressFromString(environment.networkByte, addrStr).explicitGet().get
      actual.map(_.fields("bytes")).explicitGet() shouldBe CONST_BYTEVECTOR(ByteVector(e))
    }
  }

  property("addressFromString works as the native one: wrong length") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.networkByte, pk) :+ (1: Byte))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr)))
      val actual = ev[EVALUATED](ctx.evaluationContext, expr)
      actual shouldBe evaluated(unit)
    }
  }

  property("addressFromString works as the native one: wrong address version") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = for {
      seed           <- Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
      addressVersion <- Gen.choose[Byte](0, 100)
      if addressVersion != EnvironmentFunctions.AddressVersion
    } yield {
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.networkByte, pk, addressVersion))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr)))
      val actual = ev[EVALUATED](ctx.evaluationContext, expr)
      actual shouldBe evaluated(unit)
    }
  }

  property("addressFromString works as the native one: from other network") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = for {
      seed        <- Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
      networkByte <- Gen.choose[Byte](0, 100)
      if networkByte != environment.networkByte
    } yield {
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(networkByte, pk))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr)))
      val actual = ev[EVALUATED](ctx.evaluationContext, expr)
      actual shouldBe evaluated(unit)
    }
  }

  property("addressFromString works as the native one: wrong checksum") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = for {
      seed <- Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
      bytes = {
        val (_, pk) = Curve25519.createKeyPair(seed)
        addressFromPublicKey(environment.networkByte, pk)
      }
      checkSum = bytes.takeRight(EnvironmentFunctions.ChecksumLength)
      wrongCheckSum <- Gen.containerOfN[Array, Byte](EnvironmentFunctions.ChecksumLength, Arbitrary.arbByte.arbitrary)
      if !checkSum.sameElements(wrongCheckSum)
    } yield EnvironmentFunctions.AddressPrefix + Base58.encode(bytes.dropRight(EnvironmentFunctions.ChecksumLength) ++ wrongCheckSum)

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr)))
      val actual = ev[EVALUATED](ctx.evaluationContext, expr)
      actual shouldBe evaluated(unit)
    }
  }

  private def sigVerifyTest(bodyBytes: Array[Byte], publicKey: PublicKey, signature: Signature): Either[ExecutionError, Boolean] = {
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
        pureEvalContext,
        defaultCryptoContext.evaluationContext,
        EvaluationContext.build(
          typeDefs = Map.empty,
          letDefs = Map("tx" -> LazyVal(EitherT.pure(txObj))),
          functions = Seq.empty
        )
      ))

    ev[EVALUATED](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader.Native(SIGVERIFY),
        args = List(
          GETTER(REF("tx"), "bodyBytes"),
          GETTER(REF("tx"), "proof0"),
          GETTER(REF("tx"), "senderPk")
        )
      )
    ).map {
      case CONST_BOOLEAN(b) => b
      case _                => ???
    }
  }

  private def multiSig(bodyBytes: Array[Byte],
                       senderPK: PublicKey,
                       alicePK: PublicKey,
                       bobPK: PublicKey,
                       aliceProof: Signature,
                       bobProof: Signature): (Log, Either[ExecutionError, Boolean]) = {
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

    val vars: Map[String, ((FINAL, String), LazyVal)] = Map(
      ("tx", ((txType.typeRef, "Test transaction"), LazyVal(EitherT.pure(txObj)))),
      ("alicePubKey", ((BYTEVECTOR, "Alices test publik key"), LazyVal(EitherT.pure(ByteVector(alicePK))))),
      ("bobPubKey", ((BYTEVECTOR, "Bob test public key"), LazyVal(EitherT.pure(ByteVector(bobPK)))))
    )

    val context = Monoid.combineAll(
      Seq(
        pureContext,
        defaultCryptoContext,
        CTX(Seq(txType), vars, Array.empty)
      ))

    val script =
      s"""
         |let aliceSigned  = sigVerify(tx.bodyBytes, tx.proof0, alicePubKey)
         |let bobSigned    = sigVerify(tx.bodyBytes, tx.proof1, bobPubKey  )
         |
         |aliceSigned && bobSigned
   """.stripMargin

    val r = EvaluatorV1
      .applywithLogging[EVALUATED](context.evaluationContext,
                                   new CompilerV1(context.compilerContext)
                                     .compile(script, List.empty)
                                     .explicitGet())
    (r._1, r._2.map {
      case CONST_BOOLEAN(b) => b
      case _                => ???
    })
  }

  property("checking a hash of some message by crypto function invoking") {
    val bodyText      = "some text for test"
    val bodyBytes     = bodyText.getBytes()
    val hashFunctions = Map(SHA256 -> Sha256, BLAKE256 -> Blake2b256, KECCAK256 -> Keccak256)

    for ((funcName, funcClass) <- hashFunctions) hashFuncTest(bodyBytes, funcName) shouldBe Right(ByteVector(funcClass.hash(bodyText)))
  }

  private def hashFuncTest(bodyBytes: Array[Byte], funcName: Short): Either[ExecutionError, ByteVector] = {
    val context = Monoid.combineAll(Seq(pureEvalContext, defaultCryptoContext.evaluationContext))

    ev[CONST_BYTEVECTOR](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader.Native(funcName),
        args = List(CONST_BYTEVECTOR(ByteVector(bodyBytes)))
      )
    ).map(_.bs)
  }

  property("math functions") {
    val sum   = FUNCTION_CALL(sumLong.header, List(CONST_LONG(5), CONST_LONG(5)))
    val mul   = FUNCTION_CALL(mulLong.header, List(CONST_LONG(5), CONST_LONG(5)))
    val div   = FUNCTION_CALL(divLong.header, List(CONST_LONG(10), CONST_LONG(3)))
    val mod   = FUNCTION_CALL(modLong.header, List(CONST_LONG(10), CONST_LONG(3)))
    val frac  = FUNCTION_CALL(fraction.header, List(CONST_LONG(Long.MaxValue), CONST_LONG(2), CONST_LONG(4)))
    val frac2 = FUNCTION_CALL(fraction.header, List(CONST_LONG(Long.MaxValue), CONST_LONG(3), CONST_LONG(2)))
    val frac3 = FUNCTION_CALL(fraction.header, List(CONST_LONG(-Long.MaxValue), CONST_LONG(3), CONST_LONG(2)))

    ev[EVALUATED](expr = sum) shouldBe evaluated(10)
    ev[EVALUATED](expr = mul) shouldBe evaluated(25)
    ev[EVALUATED](expr = div) shouldBe evaluated(3)
    ev[EVALUATED](expr = mod) shouldBe evaluated(1)
    ev[EVALUATED](expr = frac) shouldBe evaluated(Long.MaxValue / 2)
    ev[EVALUATED](expr = frac2) shouldBe Left(s"Long overflow: value `${BigInt(Long.MaxValue) * 3 / 2}` greater than 2^63-1")
    ev[EVALUATED](expr = frac3) shouldBe Left(s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1")
  }

  property("data constructors") {
    val point     = "Point"
    val pointType = CaseType(point, List("X" -> LONG, "Y" -> LONG))
    val pointCtor = FunctionHeader.User(point)

    ev[EVALUATED](
      context = EvaluationContext(typeDefs = Map(point -> pointType), letDefs = Map.empty, functions = Map.empty),
      FUNCTION_CALL(pointCtor, List(CONST_LONG(1), CONST_LONG(2)))
    ) shouldBe evaluated(CaseObj(pointType.typeRef, Map("X" -> CONST_LONG(1), "Y" -> CONST_LONG(2))))
  }

  property("toString") {
    import PureContext.{toStringBoolean, toStringLong}
    def evalToString(f: FunctionHeader, arg: EXPR) = ev[EVALUATED](expr = FUNCTION_CALL(f, List(arg)))

    evalToString(toStringBoolean, TRUE) shouldBe evaluated("true")
    evalToString(toStringBoolean, FALSE) shouldBe evaluated("false")

    forAll(Gen.choose(Long.MinValue, Long.MaxValue), Gen.alphaNumStr) { (n, s) =>
      evalToString(toStringLong, CONST_LONG(n)) shouldBe evaluated(n.toString)
      Try(evalToString(toStringLong, CONST_STRING(""))).isFailure shouldBe true
      Try(evalToString(toStringBoolean, CONST_STRING(""))).isFailure shouldBe true
    }
  }

  property("toBytes") {
    import PureContext.{toBytesBoolean, toBytesLong, toBytesString}
    def evalToBytes(f: FunctionHeader, arg: EXPR) = ev[EVALUATED](expr = FUNCTION_CALL(f, List(arg)))

    evalToBytes(toBytesBoolean, TRUE) shouldBe evaluated(ByteVector(1))
    evalToBytes(toBytesBoolean, FALSE) shouldBe evaluated(ByteVector(0))
    Try(evalToBytes(toStringBoolean, REF("unit"))).isFailure shouldBe true

    forAll(Gen.choose(Long.MinValue, Long.MaxValue), Gen.alphaNumStr) { (n, s) =>
      evalToBytes(toBytesLong, CONST_LONG(n)) shouldBe evaluated(ByteVector(ByteBuffer.allocate(8).putLong(n).array))
      evalToBytes(toBytesString, CONST_STRING(s)) shouldBe evaluated(ByteVector(s.getBytes("UTF-8")))
    }
  }

  property("each argument is evaluated maximum once for user function") {
    var functionEvaluated = 0

    val f = NativeFunction("F", 1, 258: Short, LONG, "", ("_", LONG, "")) { _ =>
      functionEvaluated = functionEvaluated + 1
      evaluated(1L)
    }

    val doubleFst = UserFunction("ID", LONG, "", ("x", LONG, "")) {
      FUNCTION_CALL(sumLong.header, List(REF("x"), REF("x")))
    }

    val context = Monoid.combine(pureEvalContext,
                                 EvaluationContext(
                                   typeDefs = Map.empty,
                                   letDefs = Map.empty,
                                   functions = Map(f.header -> f, doubleFst.header -> doubleFst)
                                 ))

    // g(...(g(f(1000)))))
    val expr = (1 to 6).foldLeft(FUNCTION_CALL(f.header, List(CONST_LONG(1000)))) {
      case (r, _) => FUNCTION_CALL(doubleFst.header, List(r))
    }

    ev[EVALUATED](context, expr) shouldBe evaluated(64L)

    functionEvaluated shouldBe 1
  }

  property("function parameters (REF) in body should be taken from the arguments, not from the outer context") {
    val doubleFn = UserFunction("doubleFn", LONG, "", ("x", LONG, "")) {
      FUNCTION_CALL(sumLong.header, List(REF("x"), REF("x")))
    }

    val subFn = UserFunction("mulFn", LONG, "", ("y", LONG, ""), ("x", LONG, "")) {
      FUNCTION_CALL(subLong.header, List(REF("y"), REF("x")))
    }

    // let x = 3
    // let y = 100
    val context = Monoid.combine(
      pureEvalContext,
      EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map(
          "x" -> LazyVal(EitherT.pure(3L)),
          "y" -> LazyVal(EitherT.pure(100L))
        ),
        functions = Map(
          doubleFn.header -> doubleFn,
          subFn.header    -> subFn
        )
      )
    )

    // sub(dub(x), 7)
    val expr1 = FUNCTION_CALL(subFn.header, List(FUNCTION_CALL(doubleFn.header, List(REF("x"))), CONST_LONG(7)))
    ev[EVALUATED](context, expr1) shouldBe evaluated(-1)

    // sub(7, dub(x))
    val expr2 = FUNCTION_CALL(subFn.header, List(CONST_LONG(7), FUNCTION_CALL(doubleFn.header, List(REF("x")))))
    ev[EVALUATED](context, expr2) shouldBe evaluated(1)
  }
}
