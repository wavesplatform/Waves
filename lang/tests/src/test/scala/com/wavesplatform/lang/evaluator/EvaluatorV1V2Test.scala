package com.wavesplatform.lang.evaluator

import java.nio.ByteBuffer
import cats.Id
import cats.data.EitherT
import cats.kernel.Monoid
import cats.syntax.bifunctor.*
import com.google.common.io.BaseEncoding
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.crypto.*
import com.wavesplatform.lang.Common.*
import com.wavesplatform.lang.Testing.*
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1.*
import com.wavesplatform.lang.v1.evaluator.FunctionIds.*
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{Contextful, ContextfulVal, EvaluatorV1, EvaluatorV2, FunctionIds, Log}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, ContractLimits, FunctionHeader}
import com.wavesplatform.lang.{Common, EvalF, ExecutionError, Global}
import com.wavesplatform.test.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.EitherValues

class EvaluatorV1V2Test extends PropSpec with EitherValues {

  implicit val version: StdLibVersion = V4

  private def pureContext(implicit version: StdLibVersion) = PureContext.build(version, useNewPowPrecision = true)

  private def defaultCryptoContext(implicit version: StdLibVersion) = CryptoContext.build(Global, version)

  val blockBuilder: Gen[(LET, EXPR) => EXPR] = Gen.oneOf(true, false).map(if (_) BLOCK.apply else LET_BLOCK.apply)

  private def defaultFullContext(implicit version: StdLibVersion): CTX[Environment] =
    Monoid.combineAll(
      Seq(
        defaultCryptoContext(version).withEnvironment[Environment],
        pureContext(version).withEnvironment[Environment],
        WavesContext.build(
          Global,
          DirectiveSet(version, Account, Expression).explicitGet(),
          fixBigScriptField = true
        )
      )
    )

  private def pureEvalContext(implicit version: StdLibVersion): EvaluationContext[NoContext, Id] =
    PureContext.build(version, useNewPowPrecision = true).evaluationContext

  private val defaultEvaluator = new EvaluatorV1[Id, Environment]()

  private def evalV1[T <: EVALUATED](context: EvaluationContext[Environment, Id], expr: EXPR): Either[ExecutionError, T] =
    defaultEvaluator[T](context, expr)

  private def evalV2[T <: EVALUATED](context: EvaluationContext[Environment, Id], expr: EXPR): Either[ExecutionError, T] =
    EvaluatorV2
      .applyCompleted(context, expr, LogExtraInfo(), implicitly[StdLibVersion], correctFunctionCallScope = true, newMode = true)
      ._3
      .asInstanceOf[Either[ExecutionError, T]]

  private def eval[T <: EVALUATED](context: EvaluationContext[Environment, Id], expr: EXPR): Either[ExecutionError, T] = {
    val evaluatorV1Result = evalV1[T](context, expr)
    val evaluatorV2Result = evalV2[T](context, expr)

    evaluatorV1Result shouldBe evaluatorV2Result
    evaluatorV1Result
  }

  private def evalPure[T <: EVALUATED](context: EvaluationContext[NoContext, Id] = pureEvalContext, expr: EXPR): Either[ExecutionError, T] =
    eval[T](context.asInstanceOf[EvaluationContext[Environment, Id]], expr)

  private def evalWithLogging(context: EvaluationContext[Environment, Id], expr: EXPR): Either[(ExecutionError, Log[Id]), (EVALUATED, Log[Id])] = {
    val evaluatorV1Result = defaultEvaluator.applyWithLogging[EVALUATED](context, expr)
    val (evaluatorV2Log, _, evaluatorV2Result) =
      EvaluatorV2.applyCompleted(
        context,
        expr,
        LogExtraInfo(),
        implicitly[StdLibVersion],
        correctFunctionCallScope = true,
        newMode = true
      )

    evaluatorV2Result shouldBe evaluatorV1Result.bimap(_._1, _._1)
    evaluatorV2Log should contain allElementsOf evaluatorV1Result.fold(_._2, _._2)
    evaluatorV1Result
  }

  private def simpleDeclarationAndUsage(i: Int, blockBuilder: (LET, EXPR) => EXPR) = blockBuilder(LET("x", CONST_LONG(i)), REF("x"))

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_LONG(0))((acc, _) => FUNCTION_CALL(sumLong.header, List(acc, CONST_LONG(1))))

    evalPure(expr = term) shouldBe evaluated(100000)
  }

  property("return error and log of failed evaluation") {
    forAll(blockBuilder) { block =>
      val result = evalWithLogging(
        pureEvalContext.asInstanceOf[EvaluationContext[Environment, Id]],
        expr = block(
          LET("x", CONST_LONG(3)),
          block(
            LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
            FUNCTION_CALL(PureContext.eq.header, List(REF("z"), CONST_LONG(1)))
          )
        )
      )
      result should produce("A definition of 'z' not found")
      result.fold(_._2, _._2).isEmpty shouldBe true
    }

  }

  property("successful on unused let") {
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          CONST_LONG(3)
        )
      ) shouldBe evaluated(3)
    }
  }

  property("successful on x = y") {
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          block(
            LET("y", REF("x")),
            FUNCTION_CALL(sumLong.header, List(REF("x"), REF("y")))
          )
        )
      ) shouldBe evaluated(6)
    }
  }

  property("successful on simple get") {
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](expr = simpleDeclarationAndUsage(3, block)) shouldBe evaluated(3)
    }
  }

  property("successful on get used further in expr") {
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          FUNCTION_CALL(PureContext.eq.header, List(REF("x"), CONST_LONG(2)))
        )
      ) shouldBe evaluated(false)
    }
  }

  property("successful on multiple lets") {
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          block(LET("y", CONST_LONG(3)), FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y"))))
        )
      ) shouldBe evaluated(true)
    }
  }

  property("successful on multiple lets with expression") {
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](
        expr = block(
          LET("x", CONST_LONG(3)),
          block(
            LET("y", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
            FUNCTION_CALL(PureContext.eq.header, List(REF("x"), REF("y")))
          )
        )
      ) shouldBe evaluated(true)
    }
  }

  property("successful on deep type resolution") {
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](
        expr = IF(FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))), simpleDeclarationAndUsage(3, block), CONST_LONG(4))
      ) shouldBe evaluated(4)
    }
  }

  property("successful on same value names in different branches") {
    forAll(blockBuilder) { block =>
      val expr =
        IF(
          FUNCTION_CALL(PureContext.eq.header, List(CONST_LONG(1), CONST_LONG(2))),
          simpleDeclarationAndUsage(3, block),
          simpleDeclarationAndUsage(4, block)
        )
      evalPure[EVALUATED](expr = expr) shouldBe evaluated(4)
    }
  }

  property("fails if definition not found") {
    evalPure[EVALUATED](expr = FUNCTION_CALL(sumLong.header, List(REF("x"), CONST_LONG(2)))) should produce("A definition of 'x' not found")
  }

  property("custom type field access") {
    val pointType     = CASETYPEREF("Point", List("X" -> LONG, "Y" -> LONG))
    val pointInstance = CaseObj(pointType, Map("X" -> 3L, "Y" -> 4L))
    evalPure[EVALUATED](
      context = Monoid.combine(
        pureEvalContext,
        EvaluationContext[NoContext, Id](
          Contextful.empty[Id],
          typeDefs = Map.empty,
          letDefs = Map(("p", LazyVal.fromEvaluated[Id](pointInstance))),
          functions = Map.empty
        )
      ),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2)))
    ) shouldBe evaluated(5)
  }

  property("ne works") {
    evalPure[EVALUATED](
      expr = FUNCTION_CALL(FunctionHeader.User(PureContext.ne.name), List(CONST_LONG(1), CONST_LONG(2)))
    ) shouldBe evaluated(true)

    evalPure[EVALUATED](
      expr = FUNCTION_CALL(FunctionHeader.User(PureContext.ne.name), List(CONST_LONG(1), CONST_LONG(1)))
    ) shouldBe evaluated(false)
  }

  property("lazy let evaluation doesn't throw if not used") {
    val pointType     = CASETYPEREF("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = CaseObj(pointType, Map("X" -> 3L, "Y" -> 4L))
    val context = Monoid.combine(
      pureEvalContext,
      EvaluationContext[NoContext, Id](
        Contextful.empty[Id],
        typeDefs = Map.empty,
        letDefs = Map(
          ("p", LazyVal.fromEvaluated[Id](pointInstance)),
          ("badVal", LazyVal.apply[Id](EitherT.leftT[({ type L[A] = EvalF[Id, A] })#L, EVALUATED]("Error")))
        ),
        functions = Map.empty
      )
    )
    forAll(blockBuilder) { block =>
      evalPure[EVALUATED](
        context = context,
        expr = block(LET("Z", REF("badVal")), FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2))))
      ) shouldBe evaluated(5)
    }
  }

  property("let is evaluated maximum once") {
    forAll(blockBuilder) { block =>
      var functionEvaluated = 0

      val f = NativeFunction[NoContext]("F", 1: Long, 258: Short, LONG: TYPE, Seq(("_", LONG))*) { _ =>
        functionEvaluated = functionEvaluated + 1
        evaluated(1L)
      }

      val context = Monoid
        .combine(
          pureEvalContext,
          EvaluationContext[NoContext, Id](
            Contextful.empty[Id],
            typeDefs = Map.empty,
            letDefs = Map.empty,
            functions = Map(f.header -> f)
          )
        )
        .asInstanceOf[EvaluationContext[Environment, Id]]

      val expr = block(LET("X", FUNCTION_CALL(f.header, List(CONST_LONG(1000)))), FUNCTION_CALL(sumLong.header, List(REF("X"), REF("X"))))

      evalV1[EVALUATED](context, expr) shouldBe evaluated(2L)
      functionEvaluated shouldBe 1

      evalV2[EVALUATED](context, expr) shouldBe evaluated(2L)
      functionEvaluated shouldBe 2
    }
  }

  property("successful on ref getter evaluation") {
    val fooType = CASETYPEREF("Foo", List(("bar", STRING), ("buz", LONG)))

    val fooInstance = CaseObj(fooType, Map("bar" -> "bAr", "buz" -> 1L))

    val context = EvaluationContext.build(
      typeDefs = Map.empty,
      letDefs = Map("fooInstance" -> LazyVal.fromEvaluated[Id](fooInstance)),
      functions = Seq()
    )

    val expr = GETTER(REF("fooInstance"), "bar")

    evalPure[EVALUATED](context, expr) shouldBe evaluated("bAr")
  }

  property("successful on function call getter evaluation") {
    val fooType = CASETYPEREF("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = NativeFunction[NoContext]("createFoo", 1: Long, 259: Short, fooType, List.empty*)(_ =>
      evaluated(CaseObj(fooType, Map("bar" -> "bAr", "buz" -> 1L)))
    )

    val context = EvaluationContext.build(
      typeDefs = Map.empty,
      letDefs = Map.empty,
      functions = Seq(fooCtor)
    )

    val expr = GETTER(FUNCTION_CALL(fooCtor.header, List.empty), "bar")

    evalPure[EVALUATED](context, expr) shouldBe evaluated("bAr")
  }

  property("successful on block getter evaluation") {
    val fooType = CASETYPEREF("Foo", List(("bar", STRING), ("buz", LONG)))
    val fooCtor = NativeFunction[NoContext]("createFoo", 1: Long, 259: Short, fooType, List.empty*) { _ =>
      evaluated(
        CaseObj(
          fooType,
          Map(
            "bar" -> "bAr",
            "buz" -> 1L
          )
        )
      )
    }
    val fooTransform =
      NativeFunction[NoContext]("transformFoo", 1: Long, 260: Short, fooType, ("foo", fooType)) {
        case (fooObj: CaseObj) :: Nil => evaluated(CaseObj(fooObj.caseType, fooObj.fields.updated("bar", "TRANSFORMED_BAR")))
        case _                        => ???
      }

    val context = EvaluationContext.build(
      typeDefs = Map.empty,
      letDefs = Map.empty,
      functions = Seq(fooCtor, fooTransform)
    )

    forAll(blockBuilder) { block =>
      val expr = GETTER(
        block(
          LET("fooInstance", FUNCTION_CALL(fooCtor.header, List.empty)),
          FUNCTION_CALL(fooTransform.header, List(REF("fooInstance")))
        ),
        "bar"
      )
      evalPure[EVALUATED](context, expr) shouldBe evaluated("TRANSFORMED_BAR")
    }
  }

  property("successful on simple function evaluation") {
    evalPure[EVALUATED](
      context = EvaluationContext.build(
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Seq(multiplierFunction)
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

  property("returns an success if sigVerify_NKb return a success") {
    implicit val version        = V4
    val seed                    = "seed".getBytes("UTF-8")
    val (privateKey, publicKey) = Curve25519.createKeyPair(seed)

    for (i <- 0 to 4) {
      val bodyBytes = ("m" * ((8 << i) * 1024)).getBytes("UTF-8")
      val signature = Curve25519.sign(privateKey, bodyBytes)

      val r = sigVerifyTest(bodyBytes, publicKey, signature, Some(i.toShort))
      r.isRight shouldBe true
    }
  }

  property("fail if sigVerify_NKb limits exhausted") {
    implicit val version        = V4
    val seed                    = "seed".getBytes("UTF-8")
    val (privateKey, publicKey) = Curve25519.createKeyPair(seed)

    for (i <- 0 to 4) {
      val bodyBytes = ("m" * ((8 << i) * 1024 + 1)).getBytes("UTF-8")
      val signature = Curve25519.sign(privateKey, bodyBytes)

      val r = sigVerifyTest(bodyBytes, publicKey, signature, Some(i.toShort))
      r.isLeft shouldBe true
    }
  }

  property("returns correct context") {
    val (alicePrivateKey, _)          = Curve25519.createKeyPair("seed0".getBytes("UTF-8"))
    val (bobPrivateKey, bobPublicKey) = Curve25519.createKeyPair("seed1".getBytes("UTF-8"))
    val (_, senderPublicKey)          = Curve25519.createKeyPair("seed2".getBytes("UTF-8"))

    val bodyBytes = "message".getBytes("UTF-8")

    val (result, log) = multiSig(
      bodyBytes,
      senderPublicKey,
      bobPublicKey,
      bobPublicKey,
      Curve25519.sign(alicePrivateKey, bodyBytes),
      Curve25519.sign(bobPrivateKey, bodyBytes)
    ).explicitGet()

    result shouldBe false

    // it false, because script fails on Alice's signature check, and bobSigned is not evaluated
    log.find(_._1 == "bobSigned") shouldBe None
    log.find(_._1 == "aliceSigned") shouldBe Some(("aliceSigned", evaluated(false)))
  }

  property("returns an error if sigVerify return an error") {
    val seed           = "seed".getBytes("UTF-8")
    val (_, publicKey) = Curve25519.createKeyPair(seed)
    val bodyBytes      = "message".getBytes("UTF-8")

    val r = sigVerifyTest(bodyBytes, publicKey, "signature".getBytes("UTF-8"))
    r.isLeft shouldBe false
  }

  private val genBytesAndNumber = for {
    xs     <- Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
    number <- Arbitrary.arbInt.arbitrary
  } yield (ByteStr(xs), number)

  property("drop(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(Native(FunctionIds.DROP_BYTES), List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureEvalContext, expr)
      actual shouldBe evaluated(xs.drop(number))
    }
  }

  property("take(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(TAKE_BYTES), List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureEvalContext, expr)
      actual shouldBe evaluated(xs.take(number))
    }
  }

  property("dropRightBytes(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(Native(FunctionIds.DROP_RIGHT_BYTES), List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureContext(V6).evaluationContext, expr).leftMap(_.message)
      val limit  = 165947
      actual shouldBe (
        if (number < 0)
          Left(s"Unexpected negative number = $number passed to dropRight()")
        else if (number > limit)
          Left(s"Number = $number passed to dropRight() exceeds ByteVector limit = $limit")
        else
          evaluated(xs.dropRight(number))
      )
    }
  }

  property("takeRightBytes(ByteStr, Long) works as the native one") {
    forAll(genBytesAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(Native(FunctionIds.TAKE_RIGHT_BYTES), List(CONST_BYTESTR(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureContext(V6).evaluationContext, expr).leftMap(_.message)
      val limit  = 165947
      actual shouldBe (
        if (number < 0)
          Left(s"Unexpected negative number = $number passed to takeRight()")
        else if (number > limit)
          Left(s"Number = $number passed to takeRight() exceeds ByteVector limit = $limit")
        else
          evaluated(xs.takeRight(number))
      )
    }
  }

  private val genStringAndNumber = for {
    xs     <- Arbitrary.arbString.arbitrary
    number <- Arbitrary.arbInt.arbitrary
  } yield (xs, number)

  property("drop(String, Long) works as the native one") {
    forAll(genStringAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(DROP_STRING), List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureEvalContext, expr)
      actual shouldBe evaluated(xs.drop(number))
    }
  }

  property("take(String, Long) works as the native one") {
    forAll(genStringAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(TAKE_STRING), List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureEvalContext, expr)
      actual shouldBe evaluated(xs.take(number))
    }
  }

  property("dropRight(String, Long) works as the native one") {
    forAll(genStringAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(User("dropRight"), List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureEvalContext, expr)
      actual shouldBe evaluated(xs.dropRight(number))
    }
  }

  property("takeRight(String, Long) works as the native one") {
    forAll(genStringAndNumber) { case (xs, number) =>
      val expr   = FUNCTION_CALL(User("takeRight"), List(CONST_STRING(xs).explicitGet(), CONST_LONG(number)))
      val actual = evalPure[EVALUATED](pureEvalContext, expr)
      actual shouldBe evaluated(xs.takeRight(number))
    }
  }

  property("size(String) works as the native one") {
    forAll(Arbitrary.arbString.arbitrary) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(SIZE_STRING), List(CONST_STRING(xs).explicitGet()))
      val actual = evalPure[EVALUATED](pureEvalContext, expr)
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
      val actual = evalPure[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteStr(Base58.tryDecodeWithLimit(xs).get))
    }
  }

  property("fromBase58String(String) input is 100 chars max") {
    import Global.MaxBase58String as Max
    val gen = for {
      len <- Gen.choose(Max + 1, Max * 2)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield Base58.encode(xs)

    forAll(gen) { xs =>
      val expr = FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(CONST_STRING(xs).explicitGet()))
      evalPure(defaultCryptoContext.evaluationContext, expr) should produce("base58Decode input exceeds 100")
    }
  }

  property("fromBase64String(String) works as the native one: without prefix") {
    val gen = for {
      len <- Gen.choose(0, 512)
      xs  <- Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
    } yield Base64.encode(xs)

    forAll(gen) { xs =>
      val expr   = FUNCTION_CALL(FunctionHeader.Native(FROMBASE64), List(CONST_STRING(xs).explicitGet()))
      val actual = evalPure[EVALUATED](defaultCryptoContext.evaluationContext, expr)
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
      val actual = evalPure[EVALUATED](defaultCryptoContext.evaluationContext, expr)
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
          FUNCTION_CALL(FunctionHeader.Native(TOBASE16), List(CONST_BYTESTR(ByteStr(xs)).explicitGet()))
        )
      )
      val actual = evalPure[EVALUATED](defaultCryptoContext.evaluationContext, expr)
      actual shouldBe evaluated(ByteStr(xs))
    }
  }

  property("addressFromPublicKey works as the native one") {
    val environment = emptyBlockchainEnvironment()
    val ctx         = defaultFullContext

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      pk
    }

    forAll(gen) { pkBytes =>
      val expr = FUNCTION_CALL(
        FunctionHeader.User("addressFromPublicKey"),
        List(CONST_BYTESTR(ByteStr(pkBytes)).explicitGet())
      )

      val actual = eval[CaseObj](ctx.evaluationContext(environment), expr).map(_.fields("bytes"))
      actual shouldBe evaluated(ByteStr(addressFromPublicKey(environment.chainId, pkBytes)))
    }
  }

  property("addressFromString works as the native one: sunny without prefix") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(V3)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      Base58.encode(addressFromPublicKey(environment.chainId, pk))
    }

    forAll(gen) { addrStr =>
      val expr                                   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
      val actual                                 = eval[CaseObj](ctx.evaluationContext(environment), expr)
      val a: Either[ExecutionError, EVALUATED]   = actual.map(_.fields("bytes"))
      val e: Either[String, Option[Array[Byte]]] = addressFromString(environment.chainId, addrStr)
      a shouldBe CONST_BYTESTR(ByteStr(e.explicitGet().get))
    }
  }

  property("addressFromString works as the native one: sunny with prefix") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(V3)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.chainId, pk))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
      val actual = eval[CaseObj](ctx.evaluationContext(environment), expr)
      val e      = addressFromString(environment.chainId, addrStr).explicitGet().get
      actual.map(_.fields("bytes")) shouldBe CONST_BYTESTR(ByteStr(e))
    }
  }

  property("addressFromString works as the native one: wrong length") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(V3)

    val gen = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map { seed =>
      val (_, pk) = Curve25519.createKeyPair(seed)
      EnvironmentFunctions.AddressPrefix + Base58.encode(addressFromPublicKey(environment.chainId, pk) :+ (1: Byte))
    }

    forAll(gen) { addrStr =>
      val expr   = FUNCTION_CALL(FunctionHeader.User("addressFromString"), List(CONST_STRING(addrStr).explicitGet()))
      val actual = eval[EVALUATED](ctx.evaluationContext(environment), expr)
      actual shouldBe evaluated(unit)
    }
  }

  property("addressFromString works as the native one: wrong address version") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(V3)

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
      val actual = eval[EVALUATED](ctx.evaluationContext(environment), expr)
      actual shouldBe evaluated(unit)
    }
  }

  property("addressFromString works as the native one: from other network") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(V3)

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
      val actual = eval[EVALUATED](ctx.evaluationContext(environment), expr)
      actual shouldBe evaluated(unit)
    }
  }

  property("addressFromString works as the native one: wrong checksum") {
    val environment = Common.emptyBlockchainEnvironment()
    val ctx         = defaultFullContext(V3)

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
      val actual = eval[EVALUATED](ctx.evaluationContext(environment), expr)
      actual shouldBe evaluated(unit)
    }
  }

  private def hashTest(bodyBytes: Array[Byte], hash: String, lim: Int)(implicit version: StdLibVersion): Either[ExecutionError, ByteStr] = {
    val vars: Map[String, (FINAL, ContextfulVal[NoContext])] = Map(
      ("b", (BYTESTR, ContextfulVal.pure[NoContext](CONST_BYTESTR(ByteStr(bodyBytes), limit = CONST_BYTESTR.DataTxSize).explicitGet())))
    )

    val context: CTX[NoContext] = Monoid.combineAll(
      Seq(
        pureContext,
        defaultCryptoContext,
        CTX[NoContext](Seq(), vars, Array.empty[BaseFunction[NoContext]])
      )
    )

    val script = s"""{-# STDLIB_VERSION 4 #-} ${hash}_${16 << lim}Kb(b)"""

    val expr = ExpressionCompiler
      .compileUntyped(script, context.compilerContext)
      .explicitGet()

    evalPure[EVALUATED](
      context = context.evaluationContext[Id],
      expr = expr
    ).map {
      case CONST_BYTESTR(b) => b
      case _                => ???
    }
  }

  val hashes = Seq("keccak256", "blake2b256", "sha256")
  property("returns an success if hash functions (*_NKb) return a success") {
    implicit val version = V4

    for {
      h <- hashes
      i <- 0 to 3
    } {
      val bodyBytes = ("m" * ((16 << i) * 1024)).getBytes("UTF-8")

      val r = hashTest(bodyBytes, h, i.toShort)
      r shouldBe Symbol("Right")
    }
  }

  property("fail if hash functions (*_NKb) limits exhausted") {
    implicit val version = V4

    for {
      h <- hashes
      i <- 0 to 3
    } {
      val bodyBytes = ("m" * ((16 << i) * 1024 + 1)).getBytes("UTF-8")

      val r = hashTest(bodyBytes, h, i.toShort)
      r shouldBe Symbol("Left")
    }
  }

  private def sigVerifyTest(bodyBytes: Array[Byte], publicKey: Array[Byte], signature: Array[Byte], lim_n: Option[Short] = None)(implicit
      version: StdLibVersion
  ): Either[ExecutionError, Boolean] = {
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
        "bodyBytes" -> CONST_BYTESTR(ByteStr(bodyBytes), limit = CONST_BYTESTR.NoLimit).explicitGet(),
        "senderPk"  -> ByteStr(publicKey),
        "proof0"    -> ByteStr(signature)
      )
    )

    val context = Monoid.combineAll(
      Seq(
        pureEvalContext,
        defaultCryptoContext.evaluationContext[Id],
        EvaluationContext.build(
          typeDefs = Map.empty,
          letDefs = Map("tx" -> LazyVal.fromEvaluated[Id](txObj)),
          functions = Seq.empty
        )
      )
    )

    evalPure[EVALUATED](
      context = context,
      expr = FUNCTION_CALL(
        function = FunctionHeader.Native(lim_n.fold(SIGVERIFY)(n => (SIGVERIFY_LIM + n).toShort)),
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

  private def recArrWeight(script: String): Either[ExecutionError, EVALUATED] = {
    val context: CTX[NoContext] = Monoid.combineAll(
      Seq(
        pureContext,
        defaultCryptoContext,
        CTX[NoContext](Seq(), Map(), Array.empty[BaseFunction[NoContext]])
      )
    )

    com.wavesplatform.lang.v1.parser.Parser.parseExpr(script) match {
      case fastparse.Parsed.Success(xs, _) =>
        evalPure[EVALUATED](
          context.evaluationContext[Id],
          ExpressionCompiler
            .apply(context.compilerContext, xs)
            .explicitGet()
            ._1
        )
      case f: fastparse.Parsed.Failure => Left(s"Parse error at ${f.index}")
    }
  }

  private def recCmp(cnt: Int)(
      f: ((String => String) => String) = (gen => gen("x") ++ gen("y") ++ s"x${cnt + 1} == y${cnt + 1}")
  ): Either[(ExecutionError, Log[Id]), (Boolean, Log[Id])] = {
    val context = Monoid
      .combineAll(
        Seq(
          pureContext,
          defaultCryptoContext,
          CTX[NoContext](Seq(), Map(), Array.empty[BaseFunction[NoContext]])
        )
      )
      .withEnvironment[Environment]

    def gen(a: String) = (0 to cnt).foldLeft(s"""let ${a}0="qqqq";""") { (c, n) =>
      c ++ s"""let $a${n + 1}=[$a$n,$a$n,$a$n];"""
    }
    val script = f(gen)

    evalWithLogging(
      context.evaluationContext(Common.emptyBlockchainEnvironment()),
      ExpressionCompiler
        .compileBoolean(script, context.compilerContext)
        .explicitGet()
    ).map {
      case (CONST_BOOLEAN(b), log) => (b, log)
      case _                       => ???
    }
  }

  property("recCmp") {
    val (result, log) = recCmp(4)().explicitGet()

    result shouldBe true

    // it false, because script fails on Alice's signature check, and bobSigned is not evaluated
    log.find(_._1 == "bobSigned") shouldBe None
    log.find(_._1 == "x0") shouldBe Some(("x0", evaluated("qqqq")))
  }

  property("recCmp fail by cmp") {
    recCmp(5)() shouldBe Symbol("Left")
  }

  property("recData fail by ARR") {
    val cnt    = 8
    val result = recCmp(cnt)(gen => gen("x") ++ s"x${cnt + 1}.size() == 3")

    result shouldBe Symbol("Left")
  }

  property("recData use uncomparable data") {
    val cnt         = 7
    val (result, _) = recCmp(cnt)(gen => gen("x") ++ s"x${cnt + 1}[1].size() == 3").explicitGet()
    result shouldBe true
  }

  property("List weight correct") {
    val xs = recArrWeight("[[0] ++ [1], 0::1::nil]")
      .explicitGet()
      .asInstanceOf[ARR]
      .xs
    xs(0).weight shouldBe xs(1).weight
  }

  private def genRCO(cnt: Int) = {
    (0 to cnt).foldLeft[EXPR](CONST_STRING("qqqq").explicitGet()) { (acc, i) =>
      val n = s"x$i"
      val r = REF(n)
      LET_BLOCK(LET(n, acc), FUNCTION_CALL(FunctionHeader.User("ScriptTransfer"), List(r, r, r)))
    }
  }

  property("recursive caseobject") {
    val environment = emptyBlockchainEnvironment()
    val term        = genRCO(3)

    eval[CONST_BOOLEAN](
      defaultFullContext.evaluationContext(environment),
      FUNCTION_CALL(FunctionHeader.Native(EQ), List(term, term))
    ) shouldBe evaluated(true)
  }

  property("recursive caseobject fail by compare") {
    val environment = emptyBlockchainEnvironment()
    val term        = genRCO(4)

    eval[CONST_BOOLEAN](
      defaultFullContext.evaluationContext(environment),
      FUNCTION_CALL(FunctionHeader.Native(EQ), List(term, term))
    ) shouldBe Symbol("Left")
  }

  property("recursive caseobject compare with unit") {
    val environment = emptyBlockchainEnvironment()
    val term        = genRCO(4)

    eval[CONST_BOOLEAN](
      defaultFullContext.evaluationContext(environment),
      FUNCTION_CALL(FunctionHeader.Native(EQ), List(term, REF("unit")))
    ) shouldBe evaluated(false)
  }

  private def multiSig(
      bodyBytes: Array[Byte],
      senderPK: Array[Byte],
      alicePK: Array[Byte],
      bobPK: Array[Byte],
      aliceProof: Array[Byte],
      bobProof: Array[Byte]
  ): Either[(ExecutionError, Log[Id]), (Boolean, Log[Id])] = {
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

    val vars: Map[String, (FINAL, ContextfulVal[NoContext])] = Map(
      ("tx", (txType, ContextfulVal.pure[NoContext](txObj))),
      ("alicePubKey", (BYTESTR, ContextfulVal.pure[NoContext](ByteStr(alicePK)))),
      ("bobPubKey", (BYTESTR, ContextfulVal.pure[NoContext](ByteStr(bobPK))))
    )

    val context = Monoid
      .combineAll(
        Seq(
          pureContext,
          defaultCryptoContext,
          CTX[NoContext](Seq(txType), vars, Array.empty[BaseFunction[NoContext]])
        )
      )
      .withEnvironment[Environment]

    val script =
      s"""
         |let aliceSigned  = sigVerify(tx.bodyBytes, tx.proof0, alicePubKey)
         |let bobSigned    = sigVerify(tx.bodyBytes, tx.proof1, bobPubKey  )
         |
         |aliceSigned && bobSigned
   """.stripMargin

    evalWithLogging(
      context.evaluationContext[Id](Common.emptyBlockchainEnvironment()),
      ExpressionCompiler
        .compileBoolean(script, context.compilerContext)
        .explicitGet()
    ).map {
      case (CONST_BOOLEAN(b), log) => (b, log)
      case _                       => ???
    }
  }

  property("checking a hash of some message by crypto function invoking") {
    val bodyText      = "some text for test"
    val bodyBytes     = bodyText.getBytes("UTF-8")
    val hashFunctions = Map(SHA256 -> Sha256.hash _, BLAKE256 -> Blake2b256.hash _, KECCAK256 -> Keccak256.hash _)

    for ((funcName, funcClass) <- hashFunctions) hashFuncTest(bodyBytes, funcName) shouldBe Right(ByteStr(funcClass(bodyBytes)))
  }

  private def hashFuncTest(bodyBytes: Array[Byte], funcName: Short): Either[ExecutionError, ByteStr] = {
    val context = Monoid.combineAll(Seq(pureEvalContext, defaultCryptoContext.evaluationContext[Id]))

    evalPure[CONST_BYTESTR](
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
    val frac  = FUNCTION_CALL(fraction(fixLimitCheck = false).header, List(CONST_LONG(Long.MaxValue), CONST_LONG(2), CONST_LONG(4)))
    val frac2 = FUNCTION_CALL(fraction(fixLimitCheck = false).header, List(CONST_LONG(Long.MaxValue), CONST_LONG(3), CONST_LONG(2)))
    val frac3 = FUNCTION_CALL(fraction(fixLimitCheck = false).header, List(CONST_LONG(-Long.MaxValue), CONST_LONG(3), CONST_LONG(2)))

    evalPure[EVALUATED](expr = sum) shouldBe evaluated(10)
    evalPure[EVALUATED](expr = mul) shouldBe evaluated(25)
    evalPure[EVALUATED](expr = div) shouldBe evaluated(3)
    evalPure[EVALUATED](expr = mod) shouldBe evaluated(1)
    evalPure[EVALUATED](expr = frac) shouldBe evaluated(Long.MaxValue / 2)
    evalPure[EVALUATED](expr = frac2) should produce(s"Long overflow: value `${BigInt(Long.MaxValue) * 3 / 2}` greater than 2^63-1")
    evalPure[EVALUATED](expr = frac3) should produce(s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1")
  }

  property("data constructors") {
    val point     = "Point"
    val pointType = CASETYPEREF(point, List("X" -> LONG, "Y" -> LONG))
    val pointCtor = FunctionHeader.User(point)

    evalPure[EVALUATED](
      context = EvaluationContext.build(typeDefs = Map(point -> pointType), letDefs = Map.empty, functions = Seq()),
      FUNCTION_CALL(pointCtor, List(CONST_LONG(1), CONST_LONG(2)))
    ) shouldBe evaluated(CaseObj(pointType, Map("X" -> CONST_LONG(1), "Y" -> CONST_LONG(2))))
  }

  property("toString") {
    import PureContext.{toStringBoolean, toStringLong}
    def evalToString(f: FunctionHeader, arg: EXPR) = evalPure[EVALUATED](expr = FUNCTION_CALL(f, List(arg)))

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
    def evalToBytes(f: FunctionHeader, arg: EXPR) = evalPure[EVALUATED](expr = FUNCTION_CALL(f, List(arg)))

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

    val f = NativeFunction[NoContext]("F", 1, 258: Short, LONG, ("_", LONG)) { case _ =>
      functionEvaluated = functionEvaluated + 1
      evaluated(1L)
    }

    val doubleFst = UserFunction[NoContext]("ID", 0, LONG, ("x", LONG)) {
      FUNCTION_CALL(sumLong.header, List(REF("x"), REF("x")))
    }

    val context = Monoid
      .combine(
        pureEvalContext,
        EvaluationContext.build(
          typeDefs = Map.empty,
          letDefs = Map.empty,
          functions = Seq(f, doubleFst)
        )
      )
      .asInstanceOf[EvaluationContext[Environment, Id]]

    // g(...(g(f(1000)))))
    val expr = (1 to 6).foldLeft(FUNCTION_CALL(f.header, List(CONST_LONG(1000)))) { case (r, _) =>
      FUNCTION_CALL(doubleFst.header, List(r))
    }

    evalV1[EVALUATED](context, expr) shouldBe evaluated(64L)
    functionEvaluated shouldBe 1

    evalV2[EVALUATED](context, expr) shouldBe evaluated(64L)
    functionEvaluated shouldBe 2
  }

  property("function parameters (REF) in body should be taken from the arguments, not from the outer context") {
    // func doubleFn(x: Int) = x + x
    val doubleFn = UserFunction[NoContext]("doubleFn", 0, LONG, ("x", LONG)) {
      FUNCTION_CALL(sumLong.header, List(REF("x"), REF("x")))
    }

    // func mulFn(y: Int, x: Int) = y - x
    val subFn = UserFunction[NoContext]("mulFn", 0, LONG, ("y", LONG), ("x", LONG)) {
      FUNCTION_CALL(subLong.header, List(REF("y"), REF("x")))
    }

    // let x = 3
    // let y = 100
    val context = Monoid
      .combine(
        pureEvalContext,
        EvaluationContext.build(
          typeDefs = Map.empty,
          letDefs = Map(
            "x" -> LazyVal.fromEvaluated[Id](3L),
            "y" -> LazyVal.fromEvaluated[Id](100L)
          ),
          functions = Seq(doubleFn, subFn)
        )
      )
      .asInstanceOf[EvaluationContext[Environment, Id]]

    // mulFn(doubleFn(x), 7) = (x + x) - 7 = 6 - 7 = -1
    val expr1 = FUNCTION_CALL(subFn.header, List(FUNCTION_CALL(doubleFn.header, List(REF("x"))), CONST_LONG(7)))
    evalV2[EVALUATED](context, expr1) shouldBe evaluated(-1)

    // mulFn(7, doubleFn(x)) = 7 - (x + x) = 7 - 6 = 1
    val expr2 = FUNCTION_CALL(subFn.header, List(CONST_LONG(7), FUNCTION_CALL(doubleFn.header, List(REF("x")))))
    evalV2[EVALUATED](context, expr2) shouldBe evaluated(1)
  }

  property("fromBase16String limit 32768 digits from V4") {
    val string32Kb = ("fedcba9876543210" * (32 * 1024 / 16))
    def script(base16String: String) =
      FUNCTION_CALL(
        Native(FunctionIds.FROMBASE16),
        List(CONST_STRING(base16String, reduceLimit = false).explicitGet())
      )

    def bytes(base16String: String) =
      ByteStr(BaseEncoding.base16().decode(base16String.toUpperCase))

    val v3Ctx = defaultFullContext(V3).evaluationContext(emptyBlockchainEnvironment())
    val v4Ctx = defaultFullContext(V4).evaluationContext(emptyBlockchainEnvironment())

    eval[EVALUATED](v3Ctx, script(string32Kb)) shouldBe CONST_BYTESTR(bytes(string32Kb))
    eval[EVALUATED](v4Ctx, script(string32Kb)) shouldBe CONST_BYTESTR(bytes(string32Kb))

    eval[EVALUATED](v3Ctx, script(string32Kb + "aa")) shouldBe CONST_BYTESTR(bytes(string32Kb + "aa"))
    eval[EVALUATED](v4Ctx, script(string32Kb + "aa")) should produce("Base16 decode input length=32770 should not exceed 32768")
  }

  property("tuple size limit") {
    def createTuple(size: Int) =
      FUNCTION_CALL(Native((CREATE_TUPLE - 2 + size).toShort), List.fill(size)(CONST_LONG(1)))

    val v4Ctx = defaultFullContext(V4).evaluationContext(emptyBlockchainEnvironment())

    eval[CaseObj](v4Ctx, createTuple(ContractLimits.MaxTupleSize)).explicitGet().fields.size shouldBe ContractLimits.MaxTupleSize
    eval[CaseObj](v4Ctx, createTuple(ContractLimits.MaxTupleSize + 1)) should produce("not found")
  }

  property("illegal concatenation args") {
    evalPure(expr = FUNCTION_CALL(PureContext.sumString, List(CONST_LONG(1), CONST_LONG(2)))) should produce(
      "Unexpected args (1,2) for string concatenation operator"
    )
    evalPure(expr = FUNCTION_CALL(PureContext.sumByteStr, List(CONST_LONG(1), CONST_LONG(2)))) should produce(
      "Unexpected args (1,2) for bytes concatenation operator"
    )
  }

  property("Rounding modes DOWN, HALFUP, HALFEVEN, CEILING, FLOOR are available for all versions") {
    DirectiveDictionary[StdLibVersion].all
      .foreach(version =>
        Rounding.fromV5.foreach { rounding =>
          evalPure(pureContext(version).evaluationContext, REF(rounding.`type`.name.toUpperCase)) shouldBe Right(rounding.value)
        }
      )
  }

  property("Rounding modes UP, HALFDOWN are not available from V5") {
    DirectiveDictionary[StdLibVersion].all
      .foreach(version =>
        Rounding.all.filterNot(Rounding.fromV5.contains).foreach { rounding =>
          val ref = rounding.`type`.name.toUpperCase
          val r   = evalPure(pureContext(version).evaluationContext, REF(ref))
          if (version < V5)
            r shouldBe Right(rounding.value)
          else
            r should produce(s"A definition of '$ref' not found")
        }
      )
  }
}
