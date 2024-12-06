package com.wavesplatform.consensus

import scala.io.Source
import scala.util.Random

import cats.data.NonEmptyList
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.test.PropSpec

class FairPoSCalculatorTest extends PropSpec {
  import FairPoSCalculatorTest.*
  import PoSCalculator.*

  val pos: PoSCalculator = FairPoSCalculator.V1

  case class Block(height: Int, baseTarget: Long, miner: KeyPair, timestamp: Long, delay: Long)

  def genSig: Array[Byte] = {
    val arr = new Array[Byte](32)
    Random.nextBytes(arr)
    arr
  }

  val balance: Long     = 50000000L * 100000000L
  val blockDelaySeconds = 60
  val defaultBaseTarget = 100L

  property("Base target should not be 0") {
    val baseTarget = FairPoSCalculator(60, 0).calculateBaseTarget(60, 1, 1, 2, Some(1), 3)
    baseTarget shouldBe 1
  }

  property("Base target should not be overflowed") {
    val baseTarget = FairPoSCalculator(60, 0).calculateBaseTarget(60, 1, Long.MaxValue, 2, Some(1), 3e10.toLong)
    baseTarget shouldBe Long.MaxValue
  }

  property("Correct consensus parameters distribution of blocks generated with FairPoS") {

    val miners = mkMiners
    val first  = Block(0, defaultBaseTarget, KeyPair(ByteStr(genSig)), System.currentTimeMillis(), 0)

    val chain = (1 to 100000 foldLeft NonEmptyList.of(first))((acc, _) => {
      val gg     = acc.tail.lift(1)
      val blocks = miners.map(mineBlock(acc.head, gg, _))

      val next = blocks.minBy(_.delay)

      next :: acc
    }).reverse.tail

    val avgBT    = chain.map(_.baseTarget).sum / chain.length
    val avgDelay = chain.tail.map(_.delay).sum / (chain.length - 1)

    val minersPerformance = calcPerformance(chain, miners)

    assert(minersPerformance.forall(p => p._2 < 1.1 && p._2 > 0.9))
    assert(avgDelay < 80000 && avgDelay > 40000)
    assert(avgBT < 200 && avgBT > 20)
  }

  property("Correct consensus parameters accordingly sample data") {
    def getHit(account: (PrivateKey, PublicKey), prevHitSource: ByteStr): BigInt = {
      val (privateKey, publicKey) = account
      val vrfProof                = crypto.signVRF(privateKey, prevHitSource.arr)
      val vrf                     = crypto.verifyVRF(vrfProof, prevHitSource.arr, publicKey).map(_.arr).explicitGet()
      PoSCalculator.hit(vrf)
    }

    for (i <- 201 until inputs.size) {
      val prev100   = inputs(i - 101)
      val prev      = inputs(i - 1)
      val greatPrev = inputs(i - 2)
      val curr      = inputs(i)

      val hit        = getHit((curr.privateKey, curr.publicKey), prev100.vrf)
      val delay      = pos.calculateDelay(hit, prev.baseTarget, curr.balance)
      val baseTarget = pos.calculateBaseTarget(60, i - 1, prev.baseTarget, prev.time, Some(greatPrev.time), curr.time)

      delay shouldBe curr.delay
      baseTarget shouldBe curr.baseTarget
    }
  }

  def mineBlock(prev: Block, grand: Option[Block], minerWithBalance: (KeyPair, Long)): Block = {
    val (miner, balance) = minerWithBalance
    val gs               = generationSignature(ByteStr(genSig), miner.publicKey)
    val h                = hit(gs)
    val delay            = pos.calculateDelay(h, prev.baseTarget, balance)
    val bt = pos.calculateBaseTarget(
      blockDelaySeconds,
      prev.height + 1,
      prev.baseTarget,
      prev.timestamp,
      grand.map(_.timestamp),
      prev.timestamp + delay
    )

    Block(
      prev.height + 1,
      bt,
      miner,
      prev.timestamp + delay,
      delay
    )
  }

  def calcPerformance(chain: List[Block], miners: Map[KeyPair, Long]): Map[Long, Double] = {
    val balanceSum  = miners.values.sum
    val blocksCount = chain.length

    chain
      .groupBy(_.miner)
      .map(mbs => {
        val (miner, blocks) = mbs

        val minerBalance   = miners(miner)
        val expectedBlocks = ((minerBalance.toDouble / balanceSum) * blocksCount).toLong
        val performance    = blocks.length.toDouble / expectedBlocks

        minerBalance -> performance
      })
  }

  def mkMiners: Map[KeyPair, Long] =
    List(
      KeyPair(ByteStr(genSig)) -> 200000000000000L,
      KeyPair(ByteStr(genSig)) -> 500000000000000L,
      KeyPair(ByteStr(genSig)) -> 1000000000000000L,
      KeyPair(ByteStr(genSig)) -> 1500000000000000L,
      KeyPair(ByteStr(genSig)) -> 2000000000000000L,
      KeyPair(ByteStr(genSig)) -> 2500000000000000L
    ).toMap
}

object FairPoSCalculatorTest {
  import play.api.libs.functional.syntax.*
  import play.api.libs.json.*
  import play.api.libs.json.Reads.*

  case class Input(
      privateKey: PrivateKey,
      publicKey: PublicKey,
      balance: Long,
      baseTarget: Long,
      delay: Long,
      height: Int,
      time: Long,
      vrf: ByteStr,
      genSig: ByteStr
  )

  implicit val inputReads: Reads[Input] = (
    (JsPath \ "privateKey").read[String].map(value => PrivateKey(Base58.decode(value))) and
      (JsPath \ "publicKey").read[String].map(value => PublicKey(Base58.decode(value))) and
      (JsPath \ "balance").read[Long] and
      (JsPath \ "baseTarget").read[Long] and
      (JsPath \ "delay").read[Long] and
      (JsPath \ "height").read[Int] and
      (JsPath \ "time").read[Long] and
      (JsPath \ "vrf").read[String].map(value => ByteStr(Base58.decode(value))) and
      (JsPath \ "genSig").read[String].map(value => ByteStr(Base58.decode(value)))
  )(Input.apply _)

  implicit val privateKeyWrites: Writes[PrivateKey] = Writes(k => JsString(Base58.encode(k.arr)))
  implicit val publicKeyWrites: Writes[PublicKey]   = Writes(k => JsString(Base58.encode(k.arr)))
  implicit val byteStrWrites: Writes[ByteStr]       = Writes(bs => JsString(Base58.encode(bs.arr)))

  implicit val inputWrites: Writes[Input] = Json.writes[Input]

  val inputs: List[Input] = {
    val src    = Source.fromURL(getClass.getClassLoader.getResource("vrf-pos.json"))
    val inputs = Json.parse(src.mkString).as[List[Input]]
    src.close()
    inputs
  }
}
