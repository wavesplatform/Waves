package com.wavesplatform.curve25519.test

import java.io.{BufferedOutputStream, DataOutputStream, File, FileOutputStream}
import java.util
import java.util.concurrent.{ConcurrentHashMap, Executors, ThreadLocalRandom}

import com.google.common.io.BaseEncoding
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.typesafe.scalalogging.StrictLogging
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.{Ack, ExecutionModel, Scheduler}
import monix.reactive.observers.Subscriber
import monix.reactive.{Consumer, Observable}
import org.whispersystems.curve25519.{Curve25519Provider, JavaCurve25519Provider, NativeCurve25519Provider}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.reflect.ClassTag

class TestDataWriter(randomSeed: Long, outputFileName: String) extends Subscriber[Seq[(Int, Int, Array[Byte])]] {
  private val outputStream = new DataOutputStream(
    new BufferedOutputStream(
      new FileOutputStream(new File(outputFileName), false),
      1024 * 1024
    )
  )

  outputStream.writeLong(randomSeed)

  override implicit val scheduler = Scheduler(
    Executors.newFixedThreadPool(1, new ThreadFactoryBuilder().setDaemon(true).build()),
    ExecutionModel.AlwaysAsyncExecution
  )

  override def onNext(elem: Seq[(Int, Int, Array[Byte])]): Future[Ack] =
    Future {
      for ((seedNr, msgSize, signature) <- elem) {
        outputStream.writeInt(seedNr)
        outputStream.writeInt(msgSize)
        outputStream.write(signature)
      }
    }.flatMap(_ => Ack.Continue)

  private def stop(): Unit = {
    Await.result(Future {
      outputStream.flush()
      outputStream.close()
    }, Duration.Inf)
  }

  override def onError(ex: Throwable): Unit = stop()

  override def onComplete(): Unit = stop()
}

object App extends StrictLogging {
  def provider[A <: Curve25519Provider: ClassTag]: A = {
    val ctor = implicitly[ClassTag[A]].runtimeClass.getDeclaredConstructor()
    ctor.setAccessible(true)
    ctor.newInstance().asInstanceOf[A]
  }

  private val multiplier       = 0x5DEECE66DL
  private val addend           = 0xBL
  private val mask             = (1L << 48) - 1
  private val MaxMessageLength = 150 * 1024

  private val NativeSignerJavaVerifier               = 1 << 0
  private val JavaSignerNativeVerifier               = 1 << 1
  private val NativeSignerJavaVerifierAlteredMessage = 1 << 2
  private val JavaSignerNativeVerifierAlteredMessage = 1 << 3

  def mkAccountSeed(randomSeed: Long)(seqNr: Int): Array[Byte] = {
    val nv    = (seqNr * multiplier + addend) & mask
    val value = nv << 32 | nv
    Bytes.concat(
      Longs.toByteArray(value ^ (randomSeed & 0xF000F000F000F000L)),
      Longs.toByteArray(value ^ (randomSeed & 0x0F000F000F000F00L)),
      Longs.toByteArray(value ^ (randomSeed & 0x00F000F000F000F0L)),
      Longs.toByteArray(value ^ (randomSeed & 0x000F000F000F000FL))
    )
  }

  private val allSeeds = new ConcurrentHashMap[Int, Array[Byte]](1000, 0.9f, 8)

  def mkMessage(seedSeq: Seq[Byte])(seqNr: Int): Array[Byte] = {
    val length    = seqNr % MaxMessageLength + 1
    val seedBytes = Ints.toByteArray(seqNr / MaxMessageLength).reverseIterator

    (seedBytes ++ Iterator.continually(seedSeq).flatten.drop(4)).take(length).toArray
  }

  private val allMessages = new ConcurrentHashMap[Int, Array[Byte]](150000, 0.9f, 8)

  private def signAndCheck(randomSeed: Long, nativeProvider: Curve25519Provider, javaProvider: Curve25519Provider) = {
    val SeedSeq = Longs.toByteArray(randomSeed).toSeq
    (input: Input) =>
      Task {
        val seed    = allSeeds.computeIfAbsent(input.seedNr, mkAccountSeed(randomSeed)(_))
        val message = allMessages.computeIfAbsent(input.msgLength, mkMessage(SeedSeq)(_))

        val privateKey = nativeProvider.generatePrivateKey(seed)
        val publicKey  = javaProvider.generatePublicKey(privateKey)
        val random     = new Array[Byte](64)
        ThreadLocalRandom.current().nextBytes(random)

        val nativeSignature = nativeProvider.calculateSignature(random, privateKey, message)
        val javaSignature   = javaProvider.calculateSignature(random, privateKey, message)
        var result          = 0

        if (!util.Arrays.equals(nativeSignature, javaSignature)) {}

        if (!javaProvider.verifySignature(publicKey, message, nativeSignature)) {
          logger.error(s"NSJV: pk=${toHex(publicKey)},msg=${toHex(message)},ns=${toHex(nativeSignature)}")
          result |= NativeSignerJavaVerifier
        }

        if (!nativeProvider.verifySignature(publicKey, message, javaSignature)) {
          logger.error(s"JSNV: pk=${toHex(publicKey)},msg=${toHex(message)},js=${toHex(javaSignature)}")
          result |= JavaSignerNativeVerifier
        }

        val alteredMessage = util.Arrays.copyOf(message, message.length)

        if (alteredMessage(0) == 0) alteredMessage(0) = 1.toByte else alteredMessage(0) = 0

        if (nativeProvider.verifySignature(publicKey, alteredMessage, javaSignature)) {
          logger.error(s"MMNJ: pk=${toHex(publicKey)},msg=${toHex(alteredMessage)},js=${toHex(javaSignature)}")
          result |= JavaSignerNativeVerifierAlteredMessage
        }

        if (javaProvider.verifySignature(publicKey, alteredMessage, nativeSignature)) {
          logger.error(s"MMJN: pk=${toHex(publicKey)},msg=${toHex(alteredMessage)},ns=${toHex(nativeSignature)}")
          result |= NativeSignerJavaVerifierAlteredMessage
        }

        (input.maxSeedNr, CheckResult(input.seedNr, input.msgLength, result, nativeSignature, javaSignature))
      }
  }

  case class CheckResult(seedNr: Int, messageNr: Int, resultFlags: Int, nativeSignature: Array[Byte], javaSignature: Array[Byte])

  case class Input(maxSeedNr: Int, seedNr: Int, msgLength: Int)

  def iter(modulus: Int, startWith: Int = 0): Iterator[Input] =
    Iterator.from(startWith).flatMap { maxSeedNr =>
      (0 until modulus * (maxSeedNr + 1)).iterator.map { msgLength =>
        Input(maxSeedNr, maxSeedNr - msgLength / modulus, msgLength)
      }
    }

  private val codec = BaseEncoding.base16().lowerCase()

  private def toHex(bytes: Array[Byte]): String = codec.encode(bytes)

  private val HexPattern = "0x([0-9A-Fa-f]+)[Ll]*".r

  def check(
      randomSeed: Long,
      nativeProvider: Curve25519Provider,
      javaProvider: Curve25519Provider,
      iterator: () => Iterator[Input],
      outputFileName: Option[String] = None
  ): Unit = {
    val validation = Observable
      .fromIterator(Task(iterator()))
      .mapParallelUnordered(Runtime.getRuntime.availableProcessors())(signAndCheck(randomSeed, nativeProvider, javaProvider))
      .doOnNext {
        case (_, result) if result.resultFlags != 0 =>
          Task {
            val seedBytes  = allSeeds.get(result.seedNr)
            val privateKey = nativeProvider.generatePrivateKey(seedBytes)
            val publicKey  = nativeProvider.generatePublicKey(privateKey)
            logger.error(s"""MISMATCH (${result.resultFlags}):
                          |seed=${toHex(seedBytes)}, message=${toHex(allMessages.get(result.messageNr))}
                          |  sk=${toHex(privateKey)}
                          |  pk=${toHex(publicKey)}
                          |native_sig=${toHex(result.nativeSignature)}
                          |  java_sig=${toHex(result.javaSignature)}""".stripMargin)
          }
        case _ => Task.unit
      }

    val f = outputFileName.map { fn =>
      val writer = new TestDataWriter(randomSeed, fn)
      validation.share
        .map { case (_, result) => (result.seedNr, result.messageNr, result.nativeSignature) }
        .bufferTimedAndCounted(1.minute, 100000)
        .consumeWith(Consumer.fromObserver(_ => writer))
        .runToFuture
    }

    Await.result(
      validation
        .bufferTimedAndCounted(1.minute, 100000)
        .scan((0, 0)) {
          case ((count, _), items) => (count + items.length, items.last._1)
        }
        .foreach {
          case (count, last) => logger.debug(s"Checked $count cases, last seed: $last")
        },
      Duration.Inf
    )

    for (s <- f) {
      Await.result(s, 10.seconds)
      logger.info(s"Shutdown successful")
    }
  }

  def generateSignatures(
      randomSeed: Long,
      nativeProvider: Curve25519Provider,
      iterator: () => Iterator[Input],
      outputFileName: String
  ): Unit = {
    val SeedSeq = Longs.toByteArray(randomSeed).toSeq

    val generatedSignatures = Observable
      .fromIterator(Task(iterator()))
      .mapParallelUnordered(Runtime.getRuntime.availableProcessors())(
        (input: Input) =>
          Task {
            val random = new Array[Byte](64)
            ThreadLocalRandom.current().nextBytes(random)
            val seed            = allSeeds.computeIfAbsent(input.seedNr, mkAccountSeed(randomSeed)(_))
            val privateKey      = nativeProvider.generatePrivateKey(seed)
            val message         = allMessages.computeIfAbsent(input.msgLength, mkMessage(SeedSeq)(_))
            val nativeSignature = nativeProvider.calculateSignature(random, privateKey, message)

            (input.seedNr, input.msgLength, nativeSignature)
          }
      )

    val writeFuture = generatedSignatures.share
      .bufferTimedAndCounted(1.minute, 1000000)
      .consumeWith(Consumer.fromObserver(_ => new TestDataWriter(randomSeed, outputFileName)))
      .runToFuture

    generatedSignatures
      .scan(0) {
        case (count, _) => count + 1
      }
      .bufferTimedAndCounted(1.minute, 100000)
      .foreach(b => logger.info(s"Generated ${b.last} signatures"))

    Await.result(writeFuture, Duration.Inf)
  }

  def main(args: Array[String]): Unit = {
    val nativeProvider = provider[NativeCurve25519Provider]
    val javaProvider   = provider[JavaCurve25519Provider]

    val randomSeed = args(1) match {
      case HexPattern(n) => java.lang.Long.parseLong(n, 16)
      case other         => other.toLong
    }

    val startWith = args(2).toInt
    val modulus   = args(3).toInt

    args(0).toLowerCase match {
      case "check" =>
        logger.info(s"Random seed is 0x${randomSeed.toHexString}, starting with seed #$startWith, $modulus messages per step")
        check(randomSeed, nativeProvider, javaProvider, () => iter(modulus, startWith).take(args.lift(5).fold(Int.MaxValue)(_.toInt)), args.lift(4))
      case "generate" =>
        val outputFileName = args(4)
        val count          = args(5).toInt
        logger.info(
          s"Random seed is 0x${randomSeed.toHexString}, starting with seed #$startWith, $modulus messages per step, saving $count entries to $outputFileName"
        )
        generateSignatures(randomSeed, nativeProvider, () => iter(modulus, startWith).take(count), outputFileName)
    }

    if (args.length == 3) {} else if (args.length == 5) {}
  }
}
