package com.wavesplatform.lang.v1

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, StdLibVersion}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_BIGINT, CONST_BYTESTR, CONST_LONG, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{
  BIGINT_TO_BYTES,
  BYTES_TO_BIGINT,
  BYTES_TO_BIGINT_LIM,
  DIV_BIGINT,
  FRACTION_BIGINT,
  FRACTION_BIGINT_ROUNDS,
  GE_BIGINT,
  GT_BIGINT,
  MAX_LIST_BIGINT,
  MEDIAN_LISTBIGINT,
  MIN_LIST_BIGINT,
  MOD_BIGINT,
  MUL_BIGINT,
  STRING_TO_BIGINT,
  STRING_TO_BIGINTOPT,
  SUB_BIGINT,
  SUM_BIGINT,
  UMINUS_BIGINT
}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, Rounding}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class BigIntOpsBenchmark {
  @Benchmark
  def sum1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.sumExpr1))

  @Benchmark
  def sum2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.sumExpr2))

  @Benchmark
  def sum3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.sumExpr3))

  @Benchmark
  def sum4(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.sumExpr4))

  @Benchmark
  def sub1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.subExpr1))

  @Benchmark
  def sub2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.subExpr2))

  @Benchmark
  def sub3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.subExpr3))

  @Benchmark
  def sub4(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.subExpr4))

  @Benchmark
  def mul1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.mulExpr1))

  @Benchmark
  def mul2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.mulExpr2))

  @Benchmark
  def mul3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.mulExpr3))

  @Benchmark
  def div1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.divExpr1))

  @Benchmark
  def div2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.divExpr2))

  @Benchmark
  def div3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.divExpr3))

  @Benchmark
  def mod1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.modExpr1))

  @Benchmark
  def mod2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.modExpr2))

  @Benchmark
  def mod3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.modExpr3))

  @Benchmark
  def mod4(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.modExpr4))

  @Benchmark
  def fraction1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.fractionExpr1))

  @Benchmark
  def fraction2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.fractionExpr2))

  @Benchmark
  def fraction3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.fractionExpr3))

  @Benchmark
  def fraction1Round(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.fractionRoundExpr1))

  @Benchmark
  def fraction2Round(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.fractionRoundExpr2))

  @Benchmark
  def fraction3Round(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.fractionRoundExpr3))

  @Benchmark
  def uminus1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.uminusExpr1))

  @Benchmark
  def uminus2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.uminusExpr2))

  @Benchmark
  def ge1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.geExpr1))

  @Benchmark
  def ge2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.geExpr2))

  @Benchmark
  def ge3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.geExpr3))

  @Benchmark
  def gt1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.gtExpr1))

  @Benchmark
  def gt2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.gtExpr2))

  @Benchmark
  def gt3(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.gtExpr3))

  @Benchmark
  def listMin1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.listMinExpr1))

  @Benchmark
  def listMin2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.listMinExpr2))

  @Benchmark
  def listMax1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.listMaxExpr1))

  @Benchmark
  def listMax2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.listMaxExpr2))

  @Benchmark
  def medianList(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.medianListExpr))

  @Benchmark
  def bigIntToBytes1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.bigIntToBytes1))

  @Benchmark
  def bigIntToBytes2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.bigIntToBytes2))

  @Benchmark
  def bytesToBigInt1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.bytesToBigIntExpr1))

  @Benchmark
  def bytesToBigInt2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.bytesToBigIntExpr2))

  @Benchmark
  def bytesToBigIntLim1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.bytesToBigIntLimExpr1))

  @Benchmark
  def bytesToBigIntLim2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.bytesToBigIntLimExpr2))

  @Benchmark
  def stringToBigInt1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.stringToBigIntExpr1))

  @Benchmark
  def stringToBigInt2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.stringToBigIntExpr2))

  @Benchmark
  def stringToBigIntOpt1(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.stringToBigIntOptExpr1))

  @Benchmark
  def stringToBigIntOpt2(bh: Blackhole, s: BigIntOpsSt): Unit = bh.consume(eval(s.ctx, s.stringToBigIntOptExpr2))
}

@State(Scope.Benchmark)
class BigIntOpsSt {
  val ds  = DirectiveSet(StdLibVersion.VersionDic.all.max, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max     = CONST_BIGINT(PureContext.BigIntMax)
  val prevMax = CONST_BIGINT(PureContext.BigIntMax - 1)
  val halfMax = CONST_BIGINT(PureContext.BigIntMax / 2)
  val halfMin = CONST_BIGINT(PureContext.BigIntMin / 2)
  val min     = CONST_BIGINT(PureContext.BigIntMin)
  val maxSqrt = CONST_BIGINT(BigInt("57896044618658097711785492504343953926634992332820282019728792003956564819968"))
  val three   = CONST_BIGINT(3)
  val two     = CONST_BIGINT(2)
  val list =
    ARR((PureContext.BigIntMin to PureContext.BigIntMin + PureContext.MaxListLengthV4 - 1).map(CONST_BIGINT.apply), limited = true).explicitGet()
  val listReversed = ARR(list.xs.reverse, limited = true).explicitGet()
  val maxBytes     = PureContext.BigIntMax.toByteArray

  val sumExpr1 = FUNCTION_CALL(
    Native(SUM_BIGINT),
    List(halfMax, halfMax)
  )

  val sumExpr2 = FUNCTION_CALL(
    Native(SUM_BIGINT),
    List(halfMin, halfMin)
  )

  val sumExpr3 = FUNCTION_CALL(
    Native(SUM_BIGINT),
    List(halfMin, halfMax)
  )

  val sumExpr4 = FUNCTION_CALL(
    Native(SUM_BIGINT),
    List(halfMax, three)
  )

  val subExpr1 = FUNCTION_CALL(
    Native(SUB_BIGINT),
    List(halfMax, halfMin)
  )

  val subExpr2 = FUNCTION_CALL(
    Native(SUB_BIGINT),
    List(halfMin, halfMin)
  )

  val subExpr3 = FUNCTION_CALL(
    Native(SUB_BIGINT),
    List(halfMax, halfMax)
  )

  val subExpr4 = FUNCTION_CALL(
    Native(SUB_BIGINT),
    List(halfMax, three)
  )

  val mulExpr1 = FUNCTION_CALL(
    Native(MUL_BIGINT),
    List(maxSqrt, three)
  )

  val mulExpr2 = FUNCTION_CALL(
    Native(MUL_BIGINT),
    List(maxSqrt, maxSqrt)
  )

  val mulExpr3 = FUNCTION_CALL(
    Native(MUL_BIGINT),
    List(halfMin, two)
  )

  val divExpr1 = FUNCTION_CALL(
    Native(DIV_BIGINT),
    List(max, three)
  )

  val divExpr2 = FUNCTION_CALL(
    Native(DIV_BIGINT),
    List(max, max)
  )

  val divExpr3 = FUNCTION_CALL(
    Native(DIV_BIGINT),
    List(min, three)
  )

  val modExpr1 = FUNCTION_CALL(
    Native(MOD_BIGINT),
    List(max, three)
  )

  val modExpr2 = FUNCTION_CALL(
    Native(MOD_BIGINT),
    List(max, max)
  )

  val modExpr3 = FUNCTION_CALL(
    Native(MOD_BIGINT),
    List(min, three)
  )

  val modExpr4 = FUNCTION_CALL(
    Native(MOD_BIGINT),
    List(prevMax, max)
  )

  val fractionExpr1 = FUNCTION_CALL(
    Native(FRACTION_BIGINT),
    List(halfMax, three, three)
  )

  val fractionExpr2 = FUNCTION_CALL(
    Native(FRACTION_BIGINT),
    List(max, min, min)
  )

  val fractionExpr3 = FUNCTION_CALL(
    Native(FRACTION_BIGINT),
    List(maxSqrt, maxSqrt, three)
  )

  val fractionRoundExpr1 = FUNCTION_CALL(
    Native(FRACTION_BIGINT_ROUNDS),
    List(halfMax, three, three, Rounding.HalfEven.value)
  )

  val fractionRoundExpr2 = FUNCTION_CALL(
    Native(FRACTION_BIGINT_ROUNDS),
    List(max, min, min, Rounding.HalfEven.value)
  )

  val fractionRoundExpr3 = FUNCTION_CALL(
    Native(FRACTION_BIGINT_ROUNDS),
    List(maxSqrt, maxSqrt, maxSqrt, Rounding.HalfEven.value)
  )

  val uminusExpr1 = FUNCTION_CALL(
    Native(UMINUS_BIGINT),
    List(halfMax)
  )

  val uminusExpr2 = FUNCTION_CALL(
    Native(UMINUS_BIGINT),
    List(halfMin)
  )

  val geExpr1 = FUNCTION_CALL(
    Native(GE_BIGINT),
    List(max, min)
  )

  val geExpr2 = FUNCTION_CALL(
    Native(GE_BIGINT),
    List(min, max)
  )

  val geExpr3 = FUNCTION_CALL(
    Native(GE_BIGINT),
    List(max, max)
  )

  val gtExpr1 = FUNCTION_CALL(
    Native(GT_BIGINT),
    List(max, min)
  )

  val gtExpr2 = FUNCTION_CALL(
    Native(GT_BIGINT),
    List(min, max)
  )

  val gtExpr3 = FUNCTION_CALL(
    Native(GT_BIGINT),
    List(max, max)
  )

  val listMinExpr1 = FUNCTION_CALL(
    Native(MIN_LIST_BIGINT),
    List(list)
  )

  val listMinExpr2 = FUNCTION_CALL(
    Native(MIN_LIST_BIGINT),
    List(listReversed)
  )

  val listMaxExpr1 = FUNCTION_CALL(
    Native(MAX_LIST_BIGINT),
    List(list)
  )

  val listMaxExpr2 = FUNCTION_CALL(
    Native(MAX_LIST_BIGINT),
    List(listReversed)
  )

  val medianListExpr = FUNCTION_CALL(
    Native(MEDIAN_LISTBIGINT),
    List(list)
  )

  val bigIntToBytes1 = FUNCTION_CALL(
    Native(BIGINT_TO_BYTES),
    List(max)
  )

  val bigIntToBytes2 = FUNCTION_CALL(
    Native(BIGINT_TO_BYTES),
    List(min)
  )

  val bytesToBigIntExpr1 = FUNCTION_CALL(
    Native(BYTES_TO_BIGINT),
    List(CONST_BYTESTR(ByteStr(PureContext.BigIntMax.toByteArray)).explicitGet())
  )

  val bytesToBigIntExpr2 = FUNCTION_CALL(
    Native(BYTES_TO_BIGINT),
    List(CONST_BYTESTR(ByteStr(PureContext.BigIntMin.toByteArray)).explicitGet())
  )

  val bytesToBigIntLimExpr1 = FUNCTION_CALL(
    Native(BYTES_TO_BIGINT_LIM),
    List(CONST_BYTESTR(ByteStr(maxBytes ++ Array.fill(100)(1.toByte))).explicitGet(), CONST_LONG(0), CONST_LONG(maxBytes.length))
  )

  val bytesToBigIntLimExpr2 = FUNCTION_CALL(
    Native(BYTES_TO_BIGINT_LIM),
    List(CONST_BYTESTR(ByteStr(Array.fill(100)(1.toByte) ++ maxBytes)).explicitGet(), CONST_LONG(100), CONST_LONG(maxBytes.length))
  )

  val stringToBigIntExpr1 = FUNCTION_CALL(
    Native(STRING_TO_BIGINT),
    List(CONST_STRING(PureContext.BigIntMax.toString()).explicitGet())
  )

  val stringToBigIntExpr2 = FUNCTION_CALL(
    Native(STRING_TO_BIGINT),
    List(CONST_STRING(PureContext.BigIntMin.toString()).explicitGet())
  )

  val stringToBigIntOptExpr1 = FUNCTION_CALL(
    Native(STRING_TO_BIGINTOPT),
    List(CONST_STRING(PureContext.BigIntMax.toString()).explicitGet())
  )

  val stringToBigIntOptExpr2 = FUNCTION_CALL(
    Native(STRING_TO_BIGINTOPT),
    List(CONST_STRING(PureContext.BigIntMin.toString()).explicitGet())
  )
}
