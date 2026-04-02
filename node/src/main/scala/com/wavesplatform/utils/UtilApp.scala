package com.wavesplatform.utils

import com.google.common.io.ByteStreams
import com.wavesplatform.account.{KeyPair, PKKeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.common.utils.{Base58, Base64, FastBase58}
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.crypto.{P256Curve, Sha256}
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{Application, Version}
import play.api.libs.json.{JsObject, Json}
import scopt.OParser

import java.io.{ByteArrayInputStream, File, FileInputStream, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util
import scala.annotation.nowarn
import scala.util.Random

object UtilApp {
  enum Mode {
    case CompileScript, DecompileScript, SignBytes, VerifySignature, CreateKeyPair, Hash, SerializeTx, SignTx, SmokeTest
  }

  case class CompileOptions(assetScript: Boolean = false)
  case class VerifyOptions(publicKey: PublicKey = null.asInstanceOf[PublicKey], signature: ByteStr = ByteStr.empty, checkWeakPk: Boolean = false)
  case class HashOptions(mode: String = "fast")
  case class SignTxOptions(signerAddress: String = "", currentHeight: Height = Height(1), finalityActivationHeight: Option[Height] = None)
  case class KeyPairOptions(seedType: String = "account", nonce: Int = 0)

  enum Input {
    case StdIn
    case File(file: String) extends Input
    case Str(str: String)   extends Input
  }

  case class Command(
      mode: Mode = null,
      configFile: Option[String] = None,
      inputData: Input = Input.StdIn,
      outputFile: Option[String] = None,
      inFormat: String = "plain",
      outFormat: String = "plain",
      compileOptions: CompileOptions = CompileOptions(),
      signOptions: String | KeyPair = "",
      verifyOptions: VerifyOptions = VerifyOptions(),
      hashOptions: HashOptions = HashOptions(),
      keyPairOptions: KeyPairOptions = KeyPairOptions()
  )

  private def maybeFindKeyPair(cmd: Command): Either[ValidationError, KeyPair] = {
    // we need to load application config to properly set chain ID
    val walletSettings = Application.loadApplicationConfig(cmd.configFile.map(new File(_))).walletSettings
    cmd.signOptions match {
      case signerAddress: String => Wallet(walletSettings).findPrivateKey(signerAddress)
      case kp: KeyPair           => Right(kp)
    }
  }

  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, Command()).foreach { cmd =>
      val inBytes = IO.readInput(cmd)
      val result = cmd.mode match {
        case Mode.CompileScript   => Actions.doCompile(Application.loadApplicationConfig(cmd.configFile.map(new File(_))))(cmd, inBytes)
        case Mode.DecompileScript => Actions.doDecompile(inBytes)
        case Mode.SignBytes       => maybeFindKeyPair(cmd).flatMap(Actions.doSign(_, inBytes))
        case Mode.VerifySignature => Actions.doVerify(cmd, inBytes)
        case Mode.CreateKeyPair   => Actions.doCreateKeyPair(cmd, inBytes)
        case Mode.Hash            => Actions.doHash(cmd, inBytes)
        case Mode.SerializeTx     => Actions.doSerializeTx(inBytes)
        case Mode.SignTx          => maybeFindKeyPair(cmd).flatMap(Actions.doSignTx(_, inBytes))
        case Mode.SmokeTest       => Actions.doSmokeTest()
      }

      result match {
        case Left(value)     => System.err.println(s"Error executing command: $value")
        case Right(outBytes) => IO.writeOutput(cmd, outBytes)
      }
    }
  }

  private lazy val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Command]
    import builder.*

    OParser.sequence(
      programName("waves util"),
      head("Waves Util", Version.VersionString),
      OParser.sequence(
        opt[String](name = "input-str")
          .abbr("is")
          .text("Literal input data")
          .action((s, c) => c.copy(inputData = Input.Str(s))),
        opt[String]('i', "input-file")
          .action((f, c) => c.copy(inputData = if (f.isEmpty || f == "-") Input.StdIn else Input.File(f)))
          .text("Input file name (- for stdin)")
          .validate {
            case fs if fs.isEmpty || fs == "-" || Files.isRegularFile(Paths.get(fs)) => success
            case fs                                                                  => failure(s"Invalid file: $fs")
          },
        opt[String]('o', "output-file")
          .action((f, c) => c.copy(outputFile = Some(f).filter(s => s != "-" && s.nonEmpty)))
          .text("Output file name (- for stdout)"),
        opt[String]("in-format")
          .abbr("fi")
          .action((f, c) => c.copy(inFormat = f))
          .text("Input data format (plain/base58/base64)")
          .validate {
            case "base64" | "base58" | "plain" => success
            case fs                            => failure(s"Invalid format: $fs")
          },
        opt[String]("out-format")
          .abbr("fo")
          .action((f, c) => c.copy(outFormat = f))
          .text("Output data format (plain/base58/base64)")
          .validate {
            case "base64" | "base58" | "plain" => success
            case fs                            => failure(s"Invalid format: $fs")
          },
        opt[String]('c', "config")
          .action((cf, c) => c.copy(configFile = Some(cf).filter(_.nonEmpty)))
          .text("Node config file path")
      ),
      cmd("script").children(
        cmd("compile")
          .action((_, c) => c.copy(mode = Mode.CompileScript))
          .text("Compiles RIDE script"),
        cmd("decompile")
          .action((_, c) => c.copy(mode = Mode.DecompileScript))
          .text("Decompiles binary script to RIDE code")
      ),
      cmd("hash")
        .children(
          opt[String]('m', "mode")
            .valueName("<fast|secure>")
            .action((m, c) => c.copy(hashOptions = c.hashOptions.copy(mode = m)))
        )
        .action((_, c) => c.copy(mode = Mode.Hash)),
      cmd("crypto").children(
        cmd("sign")
          .children(
            opt[String]('k', "private-key")
              .text("Private key for signing")
              .required()
              .action((s, c) => c.copy(signOptions = PKKeyPair(PrivateKey(Base58.decode(s)))))
          )
          .text("Sign bytes with provided private key")
          .action((_, c) => c.copy(mode = Mode.SignBytes)),
        cmd("verify")
          .children(
            opt[String]('k', "public-key")
              .text("Public key for verification")
              .required()
              .action((s, c) => c.copy(verifyOptions = c.verifyOptions.copy(publicKey = PublicKey(Base58.decode(s))))),
            opt[String]('s', "signature")
              .text("Signature to verify")
              .required()
              .action((s, c) => c.copy(verifyOptions = c.verifyOptions.copy(signature = ByteStr.decodeBase58(s).get))),
            opt[Boolean]("check-weak-pk")
              .abbr("cwpk")
              .text("Check for weak public key")
              .valueName("<true|false>")
              .action((checkPk, c) => c.copy(verifyOptions = c.verifyOptions.copy(checkWeakPk = checkPk)))
          )
          .text("Sign bytes with provided private key")
          .action((_, c) => c.copy(mode = Mode.SignBytes)),
        cmd("create-keys")
          .text("Generate key pair from seed")
          .action((_, c) => c.copy(mode = Mode.CreateKeyPair))
          .children(
            opt[String]("seed-type")
              .validate {
                case "account" | "wallet" => success
                case _                    => failure("Invalid seed format")
              }
              .action((t, c) => c.copy(keyPairOptions = c.keyPairOptions.copy(seedType = t))),
            opt[Int]("nonce")
              .action((n, c) => c.copy(keyPairOptions = c.keyPairOptions.copy(nonce = n)))
          )
      ),
      cmd("transaction").children(
        cmd("serialize")
          .text("Serialize JSON transaction")
          .action((_, c) => c.copy(mode = Mode.SerializeTx)),
        cmd("sign")
          .text("Sign JSON transaction")
          .action((_, c) => c.copy(mode = Mode.SignTx))
          .children(
            opt[String]("signer-address")
              .abbr("sa")
              .text("Signer address (requires corresponding key in wallet.dat)")
              .action((a, c) => c.copy(signOptions = a))
          ),
        cmd("sign-with-sk")
          .text("Sign JSON transaction with private key")
          .action((_, c) => c.copy(mode = Mode.SignTx))
          .children(
            opt[String]("private-key")
              .abbr("sk")
              .text("Private key")
              .action((a, c) => c.copy(signOptions = PKKeyPair(PrivateKey(Base58.decode(a)))))
          )
      ),
      cmd("smoke").action((_, c) => c.copy(mode = Mode.SmokeTest, inputData = Input.Str(""))),
      help("help").hidden(),
      checkConfig(_.mode match {
        case null => failure("Command should be provided")
        case _    => success
      })
    )
  }

  private object Actions {
    type ActionResult = Either[String, Array[Byte]]

    @nowarn("cat=deprecation")
    def doCompile(settings: WavesSettings)(c: Command, str: Array[Byte]): ActionResult = {
      ScriptCompiler(new String(str), c.compileOptions.assetScript, settings.estimator)
        .map(_._1.bytes().arr)
    }

    def doDecompile(data: Array[Byte]): ActionResult = {
      ScriptReader.fromBytes(data) match {
        case Left(value) =>
          Left(value.m)
        case Right(value) =>
          val (scriptText, _) = Script.decompile(value)
          Right(scriptText.getBytes(StandardCharsets.UTF_8))
      }
    }

    def doSign(keyPair: KeyPair, data: Array[Byte]): ActionResult =
      Right(com.wavesplatform.crypto.sign(keyPair.privateKey, data).arr)

    def doVerify(c: Command, data: Array[Byte]): ActionResult =
      Either.cond(
        com.wavesplatform.crypto.verify(c.verifyOptions.signature, data, c.verifyOptions.publicKey, c.verifyOptions.checkWeakPk),
        data,
        "Invalid signature"
      )

    def doCreateKeyPair(c: Command, data: Array[Byte]): ActionResult = {
      import com.wavesplatform.utils.byteStrFormat
      (c.keyPairOptions.seedType match {
        case "account" =>
          KeyPair.fromSeed(new String(data))
        case "wallet" =>
          Wallet(WalletSettings(None, Some("123"), Some(ByteStr(data))))
            .generateNewAccount(c.keyPairOptions.nonce)
            .toRight("Could not generate account")
      }).left
        .map(_.toString)
        .map(kp =>
          Json.toBytes(
            Json.obj(
              "publicKey"  -> kp.publicKey,
              "privateKey" -> kp.privateKey,
              "address"    -> kp.publicKey.toAddress,
              "walletSeed" -> ByteStr(data),
              "nonce"      -> c.keyPairOptions.nonce
            )
          )
        )
    }

    def doHash(c: Command, data: Array[Byte]): ActionResult = c.hashOptions.mode match {
      case "fast"   => Right(com.wavesplatform.crypto.fastHash(data))
      case "secure" => Right(com.wavesplatform.crypto.secureHash(data))
      case m        => Left(s"Invalid hashing mode: $m")
    }

    def doSerializeTx(data: Array[Byte]): ActionResult =
      TransactionFactory
        .parseRequest(Json.parse(data).as[JsObject])
        .left
        .map(_.toString)
        .map(_.bytes())

    def doSignTx(signerKeyPair: KeyPair, data: Array[Byte]): ActionResult = {
      import cats.syntax.either.*

      TransactionFactory
        .parseRequestAndSign(Json.parse(data).as[JsObject], signerKeyPair, None)
        .leftMap(_.toString)
        .map(_.json().toString().getBytes())
    }

    def doSmokeTest(): ActionResult = {
      val message = Base64.decode(
        "AgIZGwP/AAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFQAAAAAAAADnAAAAAAAAALeumraedvd5Slaw2xkVKB1DXUiMkdQG7TOnk5yvhzD4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADcnip8b5SPF0dONKf8Q+0DD3wVY/G6vd9jQMguDlSoxQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIABwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADQ0PMxCMjQs6H9Ericuy2oMAj6fPa7h5C7H86EurUIgwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      )
      val hash      = Base64.decode("byVxECFChy7CfjIuiAdGqX625cRK6npkOD1LUtqD4Yk=")
      val signature = Base64.decode("90ctulEo2RFhfKMLLgT9WHnx+TnmytOCWNSNwEWsVTjkEhNEMU0lyOtP2XESdwTFUAlRJwryIkWjYZR53H4FyQ==")
      val publicKey = Base64.decode("KdU/1vG5aM0TC1WRHYmV8ByD6oabSRj7vHVvqIWYn0h60Ihc/FT/NvVgBTMG8rnVnEF+AeojruMo22LjhGDo7A==")
      require(util.Arrays.equals(hash, Sha256.hash(message)), "hash mismatch")
      require(P256Curve.verify(message, signature, publicKey).explicitGet(), "invalid signature")

      val blsSK1bs = new Array[Byte](32)
      Random.nextBytes(blsSK1bs)
      val blsSK1  = BlsKeyPair(PrivateKey(blsSK1bs))
      val blsSig1 = blsSK1.sign(message)

      val blsSK2bs = new Array[Byte](32)
      Random.nextBytes(blsSK2bs)
      val blsSK2  = BlsKeyPair(PrivateKey(blsSK2bs))
      val blsSig2 = blsSK2.sign(message)

      val aggSig = BlsSignature.agg(Seq(blsSig1, blsSig2)).explicitGet()
      aggSig.verifyAgg(message, Seq(blsSK1.publicKey, blsSK2.publicKey)).explicitGet()

      Right(Array.emptyByteArray)
    }
  }

  private object IO {
    def readInput(c: Command): Array[Byte] = {
      val inputStream = c.inputData match {
        case Input.StdIn =>
          System.in

        case Input.Str(s) =>
          new ByteArrayInputStream(s.utf8Bytes)

        case Input.File(file) =>
          new FileInputStream(file)
      }

      toPlainBytes(c.inFormat, ByteStreams.toByteArray(inputStream))
    }

    def writeOutput(c: Command, result: Array[Byte]): Unit = {
      val outputStream = c.outputFile match {
        case Some(file) => new FileOutputStream(file)
        case None       => System.out
      }

      val encodedBytes = encode(result, c.outFormat)
      outputStream.write(encodedBytes)
    }

    private def encode(v: Array[Byte], format: String) = format match {
      case "plain"  => v
      case "base64" => Base64.encode(v).getBytes(StandardCharsets.US_ASCII)
      case "base58" => Base58.encode(v).getBytes(StandardCharsets.US_ASCII)
      case _        => sys.error(s"Invalid format $format")
    }

    private def toPlainBytes(inFormat: String, encodedBytes: Array[Byte]) = {
      lazy val strWithoutSpaces = new String(encodedBytes).replaceAll("\\s+", "")
      inFormat match {
        case "plain"  => encodedBytes
        case "base58" => FastBase58.decode(strWithoutSpaces)
        case "base64" => Base64.decode(strWithoutSpaces)
      }
    }
  }
}
