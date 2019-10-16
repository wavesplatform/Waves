package com.wavesplatform.lang

import java.nio.ByteBuffer

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Testing._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, EnvironmentFunctions, PureContext, _}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, Log}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

class EvaluatorV1Test extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val version = V3

  private val pureContext = PureContext.build(Global, version)

  private val defaultCryptoContext = CryptoContext.build(Global, version)

  val blockBuilder: Gen[(LET, EXPR) => EXPR] = Gen.oneOf(true, false).map(if (_) BLOCK.apply else LET_BLOCK.apply)

  private def defaultFullContext(environment: Environment): CTX = Monoid.combineAll(
    Seq(
      defaultCryptoContext,
      pureContext,
      WavesContext.build(
        DirectiveSet(version, Account, Expression).explicitGet(),
        environment
      )
    )
  )

  private val pureEvalContext: EvaluationContext = PureContext.build(Global, version).evaluationContext

  private def ev[T <: EVALUATED](context: EvaluationContext = pureEvalContext, expr: EXPR): Either[ExecutionError, T] =
    EvaluatorV1[T](context, expr)

  private def simpleDeclarationAndUsage(i: Int, blockBuilder: (LET, EXPR) => EXPR) = blockBuilder(LET("x", CONST_LONG(i)), REF("x"))

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_LONG(0))((acc, _) => FUNCTION_CALL(sumLong.header, List(acc, CONST_LONG(1))))

    ev(expr = term) shouldBe evaluated(100000)
  }

  property("return error and log of failed evaluation") {
    forAll(blockBuilder) { block =>
      val (log, Left(err)) = EvaluatorV1.applyWithLogging[EVALUATED](
        pureEvalContext,
        expr = block(
          LET("x", CONST_LONG(3)),
          block(
            LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
            FUNCTION_CALL(PureContext.eq.header, List(REF("z"), CONST_LONG(1)))
          )
        )
      )

      val expectedError = "A definition of 'z' not found"
      err shouldBe expectedError
      log.isEmpty shouldBe true
    }

  }

  property("successful on unused let") {
    forAll(blockBuilder) { block =>
      ev[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          CONST_LONG(3)
        )) shouldBe evaluated(3)
    }
  }

  property("successful on x = y") {
    forAll(blockBuilder) { block =>
      ev[EVALUATED](
        expr = block(LET("x", CONST_LONG(3)),
                     block(
                       LET("y", REF("x")),
                       FUNCTION_CALL(sumLong.header, List(REF("x"), REF("y")))
                     ))) shouldBe evaluated(6)
    }
  }

  property("successful on simple get") {
    forAll(blockBuilder) { block =>
      ev[EVALUATED](expr = simpleDeclarationAndUsage(3, block)) shouldBe evaluated(3)
    }
  }

  property("successful on get used further in expr") {
    forAll(blockBuilder) { block =>
      ev[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          FUNCTION_CALL(PureContext.eq.header, List(REF("x"), CONST_LONG(2)))
        )) shouldBe evaluated(false)
    }
  }

  property("successful on multiple lets") {
    forAll(blockBuilder) { block =>
      ev[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          block(LET("y", CONST_LONG(3)), FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y"))))
        )) shouldBe evaluated(true)
    }
  }

  property("successful on multiple lets with expression") {
    forAll(blockBuilder) { block =>
      ev[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          block(
            LET("y", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
            FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y")))
          )
        )) shouldBe evaluated(true)
    }
  }

  property("successful on deep type resolution") {
    forAll(blockBuilder) { block =>
      ev[EVALUATED](expr = IF(FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))),
                              simpleDeclarationAndUsage(3, block),
                              CONST_LONG(4))) shouldBe evaluated(4)
    }
  }

  property("successful on same value names in different branches") {
    forAll(blockBuilder) { block =>
      val expr =
        IF(FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))),
           simpleDeclarationAndUsage(3, block),
           simpleDeclarationAndUsage(4, block))
      ev[EVALUATED](expr = expr) shouldBe evaluated(4)
    }
  }

  property("fails if definition not found") {
    ev[EVALUATED](expr = FUNCTION_CALL(sumLong.header, List(REF("x"), CONST_LONG(2)))) should produce("A definition of 'x' not found")
  }

  property("custom type field access") {
    val pointType     = CASETYPEREF("Point", List("X" -> LONG, "Y" -> LONG))
    val pointInstance = CaseObj(pointType, Map("X"    -> 3L, "Y"   -> 4L))
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
    val pointType     = CASETYPEREF("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = CaseObj(pointType, Map("X" -> 3L, "Y" -> 4L))
    val context = Monoid.combine(
      pureEvalContext,
      EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map(("p", LazyVal(EitherT.pure(pointInstance))), ("badVal", LazyVal(EitherT.leftT("Error")))),
        functions = Map.empty
      )
    )
    forAll(blockBuilder) { block =>
      ev[EVALUATED](
        context = context,
        expr = block(LET("Z", REF("badVal")), FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2))))
      ) shouldBe evaluated(5)
    }
  }

  property("let is evaluated maximum once") {
    forAll(blockBuilder) { block =>
      var functionEvaluated = 0

      val f = NativeFunction("F", 1: Long, 258: Short, LONG: TYPE, Seq(("_", LONG)): _*) {
        case _ =>
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
        expr = block(LET("X", FUNCTION_CALL(f.header, List(CONST_LONG(1000)))), FUNCTION_CALL(sumLong.header, List(REF("X"), REF("X"))))
      ) shouldBe evaluated(2L)

      functionEvaluated shouldBe 1
    }
  }

  property("successful on ref getter evaluation") {
    val fooType = CASETYPEREF("Foo", List(("bar", STRING), ("buz", LONG)))

    val fooInstance = CaseObj(fooType, Map("bar" -> "bAr", "buz" -> 1L))

    val context = EvaluationContext(
      typeDefs = Map.empty,
      letDefs = Map("fooInstance" -> LazyVal(EitherT.pure(fooInstance))),
      functions = Map.empty
    )

    val expr = GETTER(REF("fooInstance"), "bar")

    ev[EVALUATED](context, expr) shouldBe evaluated("bAr")
  }

  property("successful on function call getter evaluation") {
    val fooType = CASETYPEREF("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = NativeFunction("createFoo", 1: Long, 259: Short, fooType, List.empty: _*) {
      case _ =>
        evaluated(CaseObj(fooType, Map("bar" -> "bAr", "buz" -> 1L)))
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
    val fooType = CASETYPEREF("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = NativeFunction("createFoo", 1: Long, 259: Short, fooType, List.empty: _*) {
      case _ =>
        evaluated(
          CaseObj(
            fooType,
            Map(
              "bar" -> "bAr",
              "buz" -> 1L
            )
          ))
    }
    val fooTransform =
      NativeFunction("transformFoo", 1: Long, 260: Short, fooType, ("foo", fooType)) {
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

    forAll(blockBuilder) { block =>
      val expr = GETTER(
        block(
          LET("fooInstance", FUNCTION_CALL(fooCtor.header, List.empty)),
          FUNCTION_CALL(fooTransform.header, List(REF("fooInstance")))
        ),
        "bar"
      )
      ev[EVALUATED](context, expr) shouldBe evaluated("TRANSFORMED_BAR")
    }
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
    val seed                    = "seed".getBytes("UTF-8")
    val (privateKey, publicKey) = Curve25519.createKeyPair(seed)

    val bodyBytes = "message".getBytes("UTF-8")
    val signature = Curve25519.sign(privateKey, bodyBytes)

    val r = sigVerifyTest(bodyBytes, publicKey, signature)
    r.isRight shouldBe true
  }

  property("returns correct context") {
    val (alicePrivateKey, _)          = Curve25519.createKeyPair("seed0".getBytes("UTF-8"))
    val (bobPrivateKey, bobPublicKey) = Curve25519.createKeyPair("seed1".getBytes("UTF-8"))
    val (_, senderPublicKey)          = Curve25519.createKeyPair("seed2".getBytes("UTF-8"))

    val bodyBytes = "message".getBytes("UTF-8")

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
    val seed           = "seed".getBytes("UTF-8")
    val (_, publicKey) = Curve25519.createKeyPair(seed)
    val bodyBytes      = "message".getBytes("UTF-8")

    val r = sigVerifyTest(bodyBytes, publicKey, Signature("signature".getBytes("UTF-8")))
    r.isLeft shouldBe false
  }

  private val genBytesAndNumber = for {
    xs     <- Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
    number <- Arbitrary.arbInt.arbitrary
  } yield (ByteStr(xs), number)

  property("drop(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.dropBytes.header, List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.drop(number))
    }
  }

  property("take(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(FunctionHeader.Native(TAKE_BYTES), List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.take(number))
    }
  }

  property("dropRightBytes(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.dropRightBytes.header, List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.dropRight(number))
    }
  }

  property("takeRightBytes(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.takeRightBytes.header, List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
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
        val expr   = FUNCTION_CALL(FunctionHeader.Native(DROP_STRING), List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.drop(number))
    }
  }

  property("take(String, Long) works as the native one") {
    forAll(genStringAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(FunctionHeader.Native(TAKE_STRING), List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.take(number))
    }
  }

  property("dropRight(String, Long) works as the native one") {
    forAll(genStringAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.dropRightString.header, List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.dropRight(number))
    }
  }

  property("takeRight(String, Long) works as the native one") {
    forAll(genStringAndNumber) {
      case (xs, number) =>
        val expr   = FUNCTION_CALL(PureContext.takeRightString.header, List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
        val actual = ev[EVALUATED](pureEvalContext, expr)
        actual shouldBe evaluated(xs.takeRight(number))
    }
  }

  property("size(String) works as the native one") {
    forAll(Arbitrary.arbString.arbitrary) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(SIZE_STRING), List(CONST_STRING(xs).explicitGet()))
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
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(CONST_STRING(xs).explicitGet()))
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteStr(Base58.tryDecodeWithLimit(xs).get))
    }
  }

  property("fromBase58String(String) input is 100 chars max") {
    import Global.{MaxBase58String => Max}
    val gen = for {
      len <- Gen.choose(Max + 1, Max * 2)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield Base58.encode(xs)

    forAll(gen) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(CONST_STRING(xs).explicitGet()))
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
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE64), List(CONST_STRING(xs).explicitGet()))
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteStr(Base64.tryDecode(xs).get))
    }
  }

  property("fromBase64String(String) works as the native one: with prefix") {
    val gen = for {
      len <- Gen.choose(0, 512)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield s"base64:${Base64.encode(xs)}"

    forAll(gen) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE64), List(CONST_STRING(xs).explicitGet()))
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteStr(Base64.tryDecode(xs).get))
    }
  }

  property("from/to Base16(String)") {
    val gen = for {
      len <- Gen.choose(0, 512)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield xs

    forAll(gen) { xs =>
      val expr = FUNCTION_CALL(
        FunctionHeader.Native(FROMBASE16),
        List(
          FUNCTION_CALL(FunctionHeader.Native(TOBASE16), List(CONST_BYTESTR(xs).explicitGet()))
        )
      )
      val actual = ev[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteStr(xs))
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
        List(CONST_BYTESTR(ByteStr(pkBytes)).explicitGet())
      )

      val actual = ev[CaseObj](ctx.evaluationContext, expr).map(_.fields("bytes"))
      actual shouldBe evaluated(ByteStr(addressFromPublicKey(environment.chainId, pkBytes)))
    }
  }

  property("addressFromString works as the native one: sunny without prefix") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      Base58.encode(addressFromPublicKey(environment.chainId, pk))
    }

    forAll(gen) { addrStr =>
      val expr                                   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
      val actual                                 = ev[CaseObj](ctx.evaluationContext, expr)
      val a: Either[ExecutionError, EVALUATED]   = actual.map(_.fields("bytes"))
      val e: Either[String, Option[Array[Byte]]] = addressFromString(environment.chainId, addrStr)
      a shouldBe CONST_BYTESTR(ByteStr(e.explicitGet().get))
    }
  }

  property("addressFromString works as the native one: sunny with prefix") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.chainId, pk))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
      val actual = ev[CaseObj](ctx.evaluationContext, expr)
      val e      = addressFromString(environment.chainId, addrStr).explicitGet().get
      actual.map(_.fields("bytes")) shouldBe CONST_BYTESTR(ByteStr(e))
    }
  }

  property("addressFromString works as the native one: wrong length") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.chainId, pk) :+ (1: Byte))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
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
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.chainId, pk, addressVersion))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
      val actual = ev[EVALUATED](ctx.evaluationContext, expr)
      actual shouldBe evaluated(unit)
    }
  }

  property("addressFromString works as the native one: from other network") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(environment)

    val gen = for {
      seed    <- Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
      chainId <- Gen.choose[Byte](0, 100)
      if chainId != environment.chainId
    } yield {
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(chainId, pk))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
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
        addressFromPublicKey(environment.chainId, pk)
      }
      checkSum = bytes.takeRight(EnvironmentFunctions.ChecksumLength)
      wrongCheckSum <- Gen.containerOfN[Array, Byte](EnvironmentFunctions.ChecksumLength, Arbitrary.arbByte.arbitrary)
      if !checkSum.sameElements(wrongCheckSum)
    } yield EnvironmentFunctions.AddressPrefix + Base58.encode(bytes.dropRight(EnvironmentFunctions.ChecksumLength) ++ wrongCheckSum)

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
      val actual = ev[EVALUATED](ctx.evaluationContext, expr)
      actual shouldBe evaluated(unit)
    }
  }

  private def sigVerifyTest(bodyBytes: Array[Byte], publicKey: PublicKey, signature: Signature): Either[ExecutionError, Boolean] = {
    val txType = CASETYPEREF(
      "Transaction",
      List(
        "bodyBytes" -> BYTESTR,
        "senderPk"  -> BYTESTR,
        "proof0"    -> BYTESTR
      )
    )

    val txObj = CaseObj(
      txType,
      Map(
        "bodyBytes" -> ByteStr(bodyBytes),
        "senderPk"  -> ByteStr(publicKey),
        "proof0"    -> ByteStr(signature)
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
    val txType = CASETYPEREF(
      "Transaction",
      List(
        "bodyBytes" -> BYTESTR,
        "senderPk"  -> BYTESTR,
        "proof0"    -> BYTESTR,
        "proof1"    -> BYTESTR
      )
    )

    val txObj = CaseObj(
      txType,
      Map(
        "bodyBytes" -> ByteStr(bodyBytes),
        "senderPk"  -> ByteStr(senderPK),
        "proof0"    -> ByteStr(aliceProof),
        "proof1"    -> ByteStr(bobProof)
      )
    )

    val vars: Map[String, (FINAL, LazyVal)] = Map(
      ("tx", (txType, LazyVal(EitherT.pure(txObj)))),
      ("alicePubKey", (BYTESTR, LazyVal(EitherT.pure(ByteStr(alicePK))))),
      ("bobPubKey", (BYTESTR, LazyVal(EitherT.pure(ByteStr(bobPK)))))
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
      .applyWithLogging[EVALUATED](context.evaluationContext,
                                   ExpressionCompiler
                                     .compile(script, context.compilerContext)
                                     .explicitGet())
    (r._1, r._2.map {
      case CONST_BOOLEAN(b) => b
      case _                => ???
    })
  }

  property("checking a hash of some message by crypto function invoking") {
    val bodyText      = "some text for test"
    val bodyBytes     = bodyText.getBytes("UTF-8")
    val hashFunctions = Map(SHA256 -> Sha256, BLAKE256 -> Blake2b256, KECCAK256 -> Keccak256)

    for ((funcName, funcClass) <- hashFunctions) hashFuncTest(bodyBytes, funcName) shouldBe Right(ByteStr(funcClass.hash(bodyText)))
  }

  private def hashFuncTest(bodyBytes: Array[Byte], funcName: Short): Either[ExecutionError, ByteStr] = {
    val context = Monoid.combineAll(Seq(pureEvalContext, defaultCryptoContext.evaluationContext))

    ev[CONST_BYTESTR](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader.Native(funcName),
        args = List(CONST_BYTESTR(ByteStr(bodyBytes)).explicitGet())
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
    val pointType = CASETYPEREF(point, List("X" -> LONG, "Y" -> LONG))
    val pointCtor = FunctionHeader.User(point)

    ev[EVALUATED](
      context = EvaluationContext(typeDefs = Map(point -> pointType), letDefs = Map.empty, functions = Map.empty),
      FUNCTION_CALL(pointCtor, List(CONST_LONG(1), CONST_LONG(2)))
    ) shouldBe evaluated(CaseObj(pointType, Map("X" -> CONST_LONG(1), "Y" -> CONST_LONG(2))))
  }

  property("toString") {
    import PureContext.{toStringBoolean, toStringLong}
    def evalToString(f: FunctionHeader, arg: EXPR) = ev[EVALUATED](expr = FUNCTION_CALL(f, List(arg)))

    evalToString(toStringBoolean, TRUE) shouldBe evaluated("true")
    evalToString(toStringBoolean, FALSE) shouldBe evaluated("false")

    forAll(Gen.choose(Long.MinValue, Long.MaxValue), Gen.alphaNumStr) { (n, s) =>
      evalToString(toStringLong, CONST_LONG(n)) shouldBe evaluated(n.toString)
      evalToString(toStringLong, CONST_STRING("").explicitGet()) should produce("Can't apply (CONST_STRING) to 'toString(u: Int)'")
      evalToString(toStringBoolean, CONST_STRING("").explicitGet()) should produce("Can't apply (CONST_STRING) to 'toString(b: Boolean)'")
    }
  }

  property("toBytes") {
    import PureContext.{toBytesBoolean, toBytesLong, toBytesString}
    def evalToBytes(f: FunctionHeader, arg: EXPR) = ev[EVALUATED](expr = FUNCTION_CALL(f, List(arg)))

    evalToBytes(toBytesBoolean, TRUE) shouldBe evaluated(ByteStr.fromBytes(1))
    evalToBytes(toBytesBoolean, FALSE) shouldBe evaluated(ByteStr.fromBytes(0))
    evalToBytes(toStringBoolean, REF("unit")) should produce("Can't apply (CaseObj) to 'toString(b: Boolean)'")

    forAll(Gen.choose(Long.MinValue, Long.MaxValue), Gen.alphaNumStr) { (n, s) =>
      evalToBytes(toBytesLong, CONST_LONG(n)) shouldBe evaluated(ByteStr(ByteBuffer.allocate(8).putLong(n).array))
      evalToBytes(toBytesString, CONST_STRING(s).explicitGet()) shouldBe evaluated(ByteStr(s.getBytes("UTF-8")))
    }
  }

  property("each argument is evaluated maximum once for user function") {
    var functionEvaluated = 0

    val f = NativeFunction("F", 1, 258: Short, LONG, ("_", LONG)) {
      case _ =>
        functionEvaluated = functionEvaluated + 1
        evaluated(1L)
    }

    val doubleFst = UserFunction("ID", 0, LONG, ("x", LONG)) {
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
    val doubleFn = UserFunction("doubleFn", 0, LONG, ("x", LONG)) {
      FUNCTION_CALL(sumLong.header, List(REF("x"), REF("x")))
    }

    val subFn = UserFunction("mulFn", 0, LONG, ("y", LONG), ("x", LONG)) {
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
