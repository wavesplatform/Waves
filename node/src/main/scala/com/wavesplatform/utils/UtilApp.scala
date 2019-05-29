package com.wavesplatform.utils
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.Version
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.utils.{Base58, Base64, FastBase58}
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsObject, Json}
import scopt.OParser

import scala.io.StdIn
import scala.util.Try

//noinspection ScalaStyle
// TODO: Consider remove implemented methods from REST API
object UtilApp {
  object Command {
    sealed trait Mode
    case object CompileScript   extends Mode
    case object DecompileScript extends Mode
    case object SignBytes       extends Mode
    case object VerifySignature extends Mode
    case object CreateKeyPair   extends Mode
    case object Hash            extends Mode
    case object SerializeTx     extends Mode
    case object SignTx          extends Mode
  }

  case class CompileOptions(assetScript: Boolean = false)
  case class SignOptions(privateKey: PrivateKey = null)
  case class VerifyOptions(publicKey: PublicKey = null, signature: Array[Byte] = Array.emptyByteArray)
  case class HashOptions(mode: String = "fast")
  case class SignTxOptions(signerAddress: String = "")

  case class Command(mode: Command.Mode = Command.CompileScript,
                     inputData: Option[String] = None,
                     inputFile: Option[String] = None,
                     outputFile: Option[String] = None,
                     format: String = "plain",
                     compileOptions: CompileOptions = CompileOptions(),
                     signOptions: SignOptions = SignOptions(),
                     verifyOptions: VerifyOptions = VerifyOptions(),
                     hashOptions: HashOptions = HashOptions(),
                     signTxOptions: SignTxOptions = SignTxOptions())

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) println(OParser.usage(commandParser))

    OParser.parse(commandParser, args, Command()) match {
      case Some(c) =>
        val inBytes = IO.readInput(c)
        val result = {
          val doF = c.mode match {
            case Command.CompileScript   => Actions.doCompile _
            case Command.DecompileScript => Actions.doDecompile _
            case Command.SignBytes       => Actions.doSign _
            case Command.VerifySignature => Actions.doVerify _
            case Command.CreateKeyPair   => Actions.doCreateKeyPair _
            case Command.Hash            => Actions.doHash _
            case Command.SerializeTx     => Actions.doSerializeTx _
            case Command.SignTx          => Actions.doSignTx _
          }
          doF(c, inBytes)
        }

        result match {
          case Left(value)     => System.err.println(s"Error executing command: $value")
          case Right(outBytes) => IO.writeOutput(c, outBytes)
        }

      case None =>
    }
  }

  private[this] lazy val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Command]
    import builder._

    OParser.sequence(
      programName("waves util"),
      head("Waves Util", Version.VersionString),
      OParser.sequence(
        opt[String]('s', name = "data")
          .text("Literal input data")
          .action((s, c) => c.copy(inputData = Some(s))),
        opt[String]('i', "input-file")
          .action((f, c) => c.copy(inputFile = Some(f)))
          .text("Input file name")
          .validate {
            case fs if fs.nonEmpty && Files.isRegularFile(Paths.get(fs)) => success
            case fs                                                      => failure(s"Invalid file: $fs")
          },
        opt[String]('o', "output-file")
          .action((f, c) => c.copy(outputFile = Some(f)))
          .text("Output file name"),
        opt[String]('f', "format")
          .action((f, c) => c.copy(format = f))
          .text("Output data format (plain/base58/base64)")
          .validate {
            case "base64" | "base58" | "plain" => success
            case fs                            => failure(s"Invalid format: $fs")
          }
      ),
      cmd("script").children(
        cmd("compile")
          .action((_, c) => c.copy(mode = Command.CompileScript))
          .text("Compiles RIDE script"),
        cmd("decompile")
          .action((_, c) => c.copy(mode = Command.DecompileScript))
          .text("Decompiles binary script to RIDE code")
      ),
      cmd("hash")
        .children(
          opt[String]('m', "mode")
            .action((m, c) => c.copy(hashOptions = c.hashOptions.copy(mode = m)))
        )
        .action((_, c) => c.copy(mode = Command.Hash)),
      cmd("crypto").children(
        cmd("sign")
          .children(
            opt[String]('k', "private-key")
              .text("Private key for signing")
              .required()
              .action((s, c) => c.copy(signOptions = c.signOptions.copy(privateKey = PrivateKey(Base58.decode(s)))))
          )
          .text("Sign bytes with provided private key")
          .action((_, c) => c.copy(mode = Command.SignBytes)),
        cmd("verify")
          .children(
            opt[String]('k', "public-key")
              .text("Public key for verification")
              .required()
              .action((s, c) => c.copy(verifyOptions = c.verifyOptions.copy(publicKey = PublicKey(Base58.decode(s))))),
            opt[String]('s', "signature")
              .text("Signature to verify")
              .required()
              .action((s, c) => c.copy(verifyOptions = c.verifyOptions.copy(signature = Base58.decode(s))))
          )
          .text("Sign bytes with provided private key")
          .action((_, c) => c.copy(mode = Command.SignBytes)),
        cmd("create-keys")
          .text("Generate key pair from seed")
          .action((_, c) => c.copy(mode = Command.CreateKeyPair))
      ),
      cmd("transaction").children(
        cmd("serialize")
          .text("Serialize JSON transaction")
          .action((_, c) => c.copy(mode = Command.SerializeTx)),
        cmd("sign")
          .text("Sign JSON transaction")
          .action((_, c) => c.copy(mode = Command.SignTx))
          .children(
            opt[String]('s', "signer")
              .text("Signer address")
              .action((a, c) => c.copy(signTxOptions = c.signTxOptions.copy(signerAddress = a)))
          )
      ),
      help("help").hidden()
    )
  }

  private[this] object NodeState {
    lazy val settings = WavesSettings.fromRootConfig(ConfigFactory.load())
    lazy val wallet   = Wallet(settings.walletSettings)
    lazy val time     = new NTP(settings.ntpServer)
  }

  private[this] object Actions {
    type ActionResult = Either[String, Array[Byte]]

    //noinspection ScalaDeprecation
    def doCompile(c: Command, str: Array[Byte]): ActionResult =
      ScriptCompiler(new String(str), c.compileOptions.assetScript)
        .map(_._1.bytes())

    def doDecompile(c: Command, data: Array[Byte]): ActionResult = {
      ScriptReader.fromBytes(data, checkComplexity = false) match {
        case Left(value) =>
          Left(value.m)
        case Right(value) =>
          val (scriptText, _) = Script.decompile(value)
          Right(scriptText.getBytes(StandardCharsets.UTF_8))
      }
    }

    def doSign(c: Command, data: Array[Byte]): ActionResult =
      Right(com.wavesplatform.crypto.sign(c.signOptions.privateKey, data))

    def doVerify(c: Command, data: Array[Byte]): ActionResult =
      Either.cond(com.wavesplatform.crypto.verify(c.verifyOptions.signature, data, c.verifyOptions.publicKey), data, "Invalid signature")

    def doCreateKeyPair(c: Command, data: Array[Byte]): ActionResult =
      KeyPair
        .fromSeed(new String(data))
        .left
        .map(_.toString)
        .map(kp => Json.toBytes(Json.toJson(kp)))

    def doHash(c: Command, data: Array[Byte]): ActionResult = c.hashOptions.mode match {
      case "fast"   => Right(com.wavesplatform.crypto.fastHash(data))
      case "secure" => Right(com.wavesplatform.crypto.secureHash(data))
      case m        => Left(s"Invalid hashing mode: $m")
    }

    def doSerializeTx(c: Command, data: Array[Byte]): ActionResult = {
      val jsv = Json.parse(data)
      TransactionFactory
        .fromSignedRequest(jsv)
        .left
        .map(_.toString)
        .map(_.bytes())
    }

    def doSignTx(c: Command, data: Array[Byte]): ActionResult =
      TransactionFactory
        .parseRequestAndSign(NodeState.wallet, c.signTxOptions.signerAddress, NodeState.time, Json.parse(data).as[JsObject])
        .left
        .map(_.toString)
        .map(tx => Json.toBytes(tx.json()))
  }

  private[this] object IO {
    def readInput(c: Command): Array[Byte] = {
      val inputBytes = c.inputData match {
        case Some(value) =>
          value.getBytes
        case None =>
          c.inputFile match {
            case Some(file) =>
              Files.readAllBytes(Paths.get(file))

            case None =>
              def readLinesUntil2Blank(): Iterator[String] =
                Iterator
                  .continually(StdIn.readLine())
                  .grouped(2)
                  .takeWhile {
                    case Seq("", "") => false
                    case _           => true
                  }
                  .flatten

              readLinesUntil2Blank().mkString("\n").getBytes(StandardCharsets.UTF_8)
          }
      }
      tryDecode(inputBytes)
    }

    def writeOutput(c: Command, result: Array[Byte]): Unit = c.outputFile match {
      case Some(value) =>
        Files.write(Paths.get(value), encode(result, c.format))

      case None =>
        println(new String(encode(result, c.format)))
    }

    private[this] def encode(v: Array[Byte], format: String) = format match {
      case "plain"  => v
      case "base64" => Base64.encode(v).getBytes(StandardCharsets.US_ASCII)
      case "base58" => Base58.encode(v).getBytes(StandardCharsets.US_ASCII)
      case _        => sys.error(s"Invalid format $format")
    }

    private[this] def tryDecode(v: Array[Byte]) =
      Try(FastBase58.decode(new String(v).replaceAll("\\s+", "")))
        .orElse(Try(Base64.decode(new String(v).replaceAll("\\s+", ""))))
        .getOrElse(v)
  }
}
