package com.wavesplatform

import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.ByteVector
import scorex.transaction.smart.lang.Terms
import scorex.transaction.smart.lang.Terms._

trait ScriptGen {

  def CONST_INTgen: Gen[CONST_INT] = Gen.choose(Int.MinValue, Int.MaxValue).map(CONST_INT)

  def HEIGHTgen: Gen[Terms.HEIGHT.type] = Gen.const(HEIGHT)

  def SUMgen(gas: Int): Gen[SUM] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield SUM(i1, i2)

  def INTGen(gas: Int): Gen[INT] = if (gas > 0) Gen.oneOf(HEIGHTgen, CONST_INTgen, SUMgen(gas - 1), IF_INTgen(gas - 1)) else Gen.oneOf(HEIGHTgen, CONST_INTgen)

  def GEgen(gas: Int): Gen[GE] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield GE(i1, i2)


  def GTgen(gas: Int): Gen[GT] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield GT(i1, i2)

  def EQ_INTgen(gas: Int): Gen[EQ_INT] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield EQ_INT(i1, i2)

  def ANDgen(gas: Int): Gen[AND] = for {
    i1 <- BOOLgen((gas - 2) / 2)
    i2 <- BOOLgen((gas - 2) / 2)
  } yield AND(i1, i2)


  def ORgen(gas: Int): Gen[OR] = for {
    i1 <- BOOLgen((gas - 2) / 2)
    i2 <- BOOLgen((gas - 2) / 2)
  } yield OR(i1, i2)

  def SIG_VERIFYgen: Gen[SIG_VERIFY] = for {
    x <- CONST_BYTEVECTORgen
    y <- CONST_BYTEVECTORgen
    z <- CONST_BYTEVECTORgen
  } yield SIG_VERIFY(x, y, z)

  def BOOLgen(gas: Int): Gen[BOOL] = if (gas > 0) Gen.oneOf(GEgen(gas - 1), GTgen(gas - 1), EQ_INTgen(gas - 1), ANDgen(gas - 1), ORgen(gas - 1), IF_BOOLgen(gas - 1), SIG_VERIFYgen) else SIG_VERIFYgen

  def CONST_BYTEVECTORgen: Gen[CONST_BYTEVECTOR] = Gen.choose(0, 100).flatMap(l => Gen.containerOfN[Array, Byte](l, Arbitrary.arbitrary[Byte])).map(bs => CONST_BYTEVECTOR(ByteVector(bs)))

  def IF_BOOLgen(gas: Int): Gen[BOOL] = for {
    cnd <- BOOLgen((gas - 3) / 3)
    t <- BOOLgen((gas - 3) / 3)
    f <- BOOLgen((gas - 3) / 3)
  } yield IF(cnd, t, f)

  def IF_INTgen(gas: Int): Gen[INT] = for {
    cnd <- BOOLgen((gas - 3) / 3)
    t <- INTGen((gas - 3) / 3)
    f <- INTGen((gas - 3) / 3)
  } yield IF(cnd, t, f)

}
