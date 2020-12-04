package com.wavesplatform.curve25519.test

import java.io._
import java.util
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicLong

import com.google.common.io.{BaseEncoding, CountingOutputStream}
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.typesafe.scalalogging.StrictLogging
import monix.execution.Scheduler
import org.whispersystems.curve25519.{Curve25519Provider, JavaCurve25519Provider, NativeCurve25519Provider}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.reflect.ClassTag

object App extends StrictLogging {

  class Writer(queue: BlockingQueue[Seq[CheckResult]], latch: CountDownLatch, out: DataOutputStream) extends Runnable {
    override def run(): Unit = while (latch.getCount > 0) {
      for (r <- queue.take()) {
        out.writeInt(r.seedNr)
        out.writeInt(r.messageNr)
        out.write(r.signature)
      }
      out.flush()
    }
  }

  class Dispatcher(startWith: Int, modulus: Int, itemCount: Option[Int] = None) {
    @volatile var maxSeedNr = startWith
    private[this] val iterator =
      Iterator
        .from(startWith)
        .flatMap { maxSeedNr =>
          (0 until modulus * (maxSeedNr + 1)).iterator.map { msgLength =>
            Input(maxSeedNr, maxSeedNr - msgLength / modulus, msgLength)
          }
        }
        .zipWithIndex
        .takeWhile { case (_, seqNr) => itemCount.forall(_ > seqNr) }
        .map(_._1)

    def nextBatch(batchSize: Int): Seq[Input] = iterator.synchronized {
      val batch = iterator.take(batchSize).toSeq
      batch.lastOption.foreach(i => maxSeedNr = i.maxSeedNr)
      batch
    }
  }

  class SignatureDataReader(in: DataInputStream) {
    def nextBatch(batchSize: Int): Seq[(Int, Int, Array[Byte])] = in.synchronized {
      val buffer      = Seq.newBuilder[(Int, Int, Array[Byte])]
      var canContinue = true
      var counter     = 0
      while (canContinue && counter < batchSize) try {
        val seedNr    = in.readInt()
        val msgLength = in.readInt()
        val signature = new Array[Byte](64)
        in.readFully(signature)
        buffer += ((seedNr, msgLength, signature))
        counter += 1
      } catch {
        case _: EOFException =>
          canContinue = false
      }
      buffer.result()
    }
  }

  def mkMessageTemplate(randomSeed: Long): Array[Byte] = {
    val seedSeq   = Longs.toByteArray(randomSeed)
    val remainder = MaxMessageLength % Longs.BYTES
    Array.fill(MaxMessageLength / Longs.BYTES + (if (remainder > 0) 1 else 0))(seedSeq).flatten
  }

  def mkMsg(seqNr: Int, messageTemplate: Array[Byte]): Array[Byte] = {
    val length = seqNr % MaxMessageLength + 1
    val prefix = Ints.toByteArray(seqNr / MaxMessageLength).reverse
    val result = new Array[Byte](length)

    System.arraycopy(prefix, 0, result, 0, math.min(Ints.BYTES, length))
    if (length > Ints.BYTES) {
      System.arraycopy(messageTemplate, 0, result, Ints.BYTES, length - Ints.BYTES)
    }

    result
  }

  abstract class Worker(
      latch: CountDownLatch,
      queue: Option[BlockingQueue[Seq[CheckResult]]],
      counter: AtomicLong,
      randomSeed: Long,
      nextBatch: () => Seq[Input]
  ) extends Runnable {
    private val messageTemplate = mkMessageTemplate(randomSeed)

    def sign(seed: Array[Byte], message: Array[Byte]): Array[Byte]

    @tailrec
    private def checkTask(batch: Seq[Input]): Unit =
      if (batch.isEmpty) {
        logger.info("No more values to check")
      } else {
        queue.foreach(
          _.put(batch.map { input =>
            val signarure = sign(mkAccountSeed(randomSeed)(input.seedNr), mkMsg(input.msgLength, messageTemplate))
            CheckResult(input.seedNr, input.msgLength, signarure)
          })
        )

        counter.updateAndGet(_ + batch.size)
        checkTask(nextBatch())
      }

    override def run(): Unit = {
      checkTask(nextBatch())
      latch.countDown()
    }
  }

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

  private def signAndCheck(
      seed: Array[Byte],
      message: Array[Byte],
      nativeProvider: Curve25519Provider,
      javaProvider: Curve25519Provider
  ): Array[Byte] = {
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

    if (result != 0) {
      logger.error(s"""MISMATCH ($result):
                      |seed=${toHex(seed)}, message=${toHex(message)}
                      |  sk=${toHex(privateKey)}
                      |  pk=${toHex(publicKey)}
                      |native_sig=${toHex(nativeSignature)}
                      |  java_sig=${toHex(javaSignature)}""".stripMargin)
    }

    nativeSignature
  }

  private def signMessage(seed: Array[Byte], message: Array[Byte], provider: Curve25519Provider): Array[Byte] = {
    val privateKey = provider.generatePrivateKey(seed)
    val random     = new Array[Byte](64)
    ThreadLocalRandom.current().nextBytes(random)

    provider.calculateSignature(random, privateKey, message)
  }

  case class CheckResult(seedNr: Int, messageNr: Int, signature: Array[Byte])

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

  private val workerCount: Int = Runtime.getRuntime.availableProcessors()

  def main(args: Array[String]): Unit = {
    val nativeProvider = provider[NativeCurve25519Provider]
    val javaProvider   = provider[JavaCurve25519Provider]
    val latch      = new CountDownLatch(workerCount)

    args(0).toLowerCase match {
      case "verify" =>
        val in          = new DataInputStream(new BufferedInputStream(new FileInputStream(args(1)), 1024 * 1024))
        val randomSeed  = in.readLong()
        val msgTemplate = mkMessageTemplate(randomSeed)
        val reader      = new SignatureDataReader(in)

        @tailrec def verifySignatures(): Unit = {
          val batch = reader.nextBatch(1000)
          if (batch.isEmpty) {
            logger.info(s"No more signatures to verify")
          } else {
            for ((seedNr, msgLength, signature) <- batch) {
              val seed    = mkAccountSeed(randomSeed)(seedNr)
              val message = mkMsg(msgLength, msgTemplate)
              if (!nativeProvider.verifySignature(nativeProvider.generatePublicKey(nativeProvider.generatePrivateKey(seed)), message, signature)) {
                logger.error(s"Mismatch: $seedNr, $msgLength")
              }
            }
            verifySignatures()
          }
        }

        (1 to workerCount) foreach { i =>
          logger.info(s"Starting worker $i")
          val t = new Thread({ () =>
            verifySignatures()
            latch.countDown()
          })
          t.setDaemon(true)
          t.setName(s"worker-$i")
          t.start()
        }

        latch.await()
        logger.info("All signatures are valid")

      case action @ ("check" | "generate") =>
        val randomSeed = args(1) match {
          case HexPattern(n) => java.lang.Long.parseLong(n, 16)
          case other         => other.toLong
        }

        val startWith = args(2).toInt
        val modulus   = args(3).toInt

        val count = args.lift(5).map(_.toInt)
        logger.info(s"Item count: $count")
        val dispatcher = new Dispatcher(startWith, modulus, count)
        val counter    = new AtomicLong()

        val maybeOut = args.lift(4).map { outputFileName =>
          val queue      = new LinkedBlockingQueue[Seq[CheckResult]]()
          val out        = new CountingOutputStream(new BufferedOutputStream(new FileOutputStream(new File(outputFileName)), 100000))
          val dataStream = new DataOutputStream(out)
          dataStream.writeLong(randomSeed)
          dataStream.flush()
          val writer = new Thread(new Writer(queue, latch, dataStream))
          writer.setName("writer")
          writer.start()

          (out, writer, queue)
        }

        def newWorker() = action match {
          case "check" =>
            new Worker(latch, maybeOut.map(_._3), counter, randomSeed, () => dispatcher.nextBatch(1000)) {
              override def sign(seed: Array[Byte], message: Array[Byte]): Array[Byte] = signAndCheck(seed, message, nativeProvider, javaProvider)
            }
          case "generate" =>
            new Worker(latch, maybeOut.map(_._3), counter, randomSeed, () => dispatcher.nextBatch(1000)) {
              override def sign(seed: Array[Byte], message: Array[Byte]): Array[Byte] = signMessage(seed, message, nativeProvider)
            }
        }

        (1 to workerCount) foreach { i =>
          logger.info(s"Starting worker $i")
          val t = new Thread(newWorker())
          t.setDaemon(true)
          t.setName(s"worker-$i")
          t.start()
        }

        Scheduler.global.scheduleWithFixedDelay(1.minute, 1.minute)(
          logger.info(
            s"Max seed nr: ${dispatcher.maxSeedNr}. Checked ${counter.get()} values${maybeOut.fold("")(c => s", written ${c._1.getCount} bytes")}"
          )
        )
        latch.await()

        maybeOut.foreach {
          case (out, writer, _) =>
            writer.join()
            out.flush()
            out.close()
        }
    }
  }
}
