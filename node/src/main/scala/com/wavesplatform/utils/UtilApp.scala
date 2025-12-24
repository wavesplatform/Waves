package com.wavesplatform.utils

import com.google.common.io.ByteStreams
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.common.utils.{Base58, Base64, FastBase58}
import com.wavesplatform.crypto.{P256Curve, Sha256}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.{GenerationPeriod, Height}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{Transaction, TransactionFactory, TransactionSignOps, TransactionType}
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{Application, Version}
import play.api.libs.json.{JsObject, Json}
import scopt.OParser

import java.io.{ByteArrayInputStream, File, FileInputStream, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util
import scala.annotation.nowarn

//noinspection ScalaStyle
// TODO: Consider remove implemented methods from REST API
object UtilApp {
  enum Mode {
    case CompileScript, DecompileScript, SignBytes, VerifySignature, CreateKeyPair, Hash, SerializeTx, SignTx, SignTxWithSk, SmokeTest
  }

  case class CompileOptions(assetScript: Boolean = false)
  case class SignOptions(privateKey: PrivateKey = null.asInstanceOf[PrivateKey])
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
      signOptions: SignOptions = SignOptions(),
      verifyOptions: VerifyOptions = VerifyOptions(),
      hashOptions: HashOptions = HashOptions(),
      signTxOptions: SignTxOptions = SignTxOptions(),
      keyPairOptions: KeyPairOptions = KeyPairOptions()
  )

  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, Command()).foreach { cmd =>
      val inBytes = IO.readInput(cmd)
      val result = cmd.mode match {
        case Mode.CompileScript   => Actions.doCompile(Application.loadApplicationConfig(cmd.configFile.map(new File(_))))(cmd, inBytes)
        case Mode.DecompileScript => Actions.doDecompile(inBytes)
        case Mode.SignBytes       => Actions.doSign(cmd, inBytes)
        case Mode.VerifySignature => Actions.doVerify(cmd, inBytes)
        case Mode.CreateKeyPair   => Actions.doCreateKeyPair(cmd, inBytes)
        case Mode.Hash            => Actions.doHash(cmd, inBytes)
        case Mode.SerializeTx     => Actions.doSerializeTx(inBytes)
        case Mode.SignTx          => Actions.doSignTx(new NodeState(cmd))(cmd, inBytes)
        case Mode.SignTxWithSk    => Actions.doSignTxWithSK(cmd, inBytes)
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
              .action((s, c) => c.copy(signOptions = c.signOptions.copy(privateKey = PrivateKey(Base58.decode(s)))))
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
              .action((a, c) => c.copy(signTxOptions = c.signTxOptions.copy(signerAddress = a))),
            opt[Int]('h', "current-height")
              .text("Current height, required for signing CommitToGeneration transaction")
              .optional()
              .action((h, c) => c.copy(signTxOptions = c.signTxOptions.copy(currentHeight = Height(h)))),
            opt[Int]('f', "finality-activation-height")
              .text("Finality activation height, required for signing CommitToGeneration transaction. From preActivatedFeatures setting by default")
              .optional()
              .action((h, c) => c.copy(signTxOptions = c.signTxOptions.copy(finalityActivationHeight = Some(Height(h)))))
          ),
        cmd("sign-with-sk")
          .text("Sign JSON transaction with private key")
          .action((_, c) => c.copy(mode = Mode.SignTxWithSk))
          .children(
            opt[String]("private-key")
              .abbr("sk")
              .text("Private key")
              .action((a, c) => c.copy(signOptions = c.signOptions.copy(privateKey = PrivateKey(Base58.decode(a)))))
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

  // noinspection TypeAnnotation
  private final class NodeState(c: Command) {
    lazy val settings = Application.loadApplicationConfig(c.configFile.map(new File(_)))
    lazy val wallet   = Wallet(settings.walletSettings)
    lazy val time     = new NTP(settings.ntpServer)
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

    def doSign(c: Command, data: Array[Byte]): ActionResult =
      Right(com.wavesplatform.crypto.sign(c.signOptions.privateKey, data).arr)

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

    def doSerializeTx(data: Array[Byte]): ActionResult = {
      val jsv = Json.parse(data)
      TransactionFactory
        .fromSignedRequest(jsv)
        .left
        .map(_.toString)
        .map(_.bytes())
    }

    def doSignTx(ns: NodeState)(c: Command, data: Array[Byte]): ActionResult = {
      val unsignedTx = Json.parse(data).as[JsObject]

      val currentPeriod = for {
        finalityActivationHeight <- c.signTxOptions.finalityActivationHeight
          .orElse(
            ns.settings.blockchainSettings.functionalitySettings.preActivatedFeatures
              .get(BlockchainFeatures.DeterministicFinality.id)
              .map(Height.apply)
          )
        currentPeriod <- GenerationPeriod.from(c.signTxOptions.currentHeight, finalityActivationHeight, ns.settings)
      } yield currentPeriod

      val signedTx = for {
        tpe <- (unsignedTx \ "type").validate[Int].asEither.left.map { _ => s"Can't parse as transaction request: $unsignedTx" }
        currentPeriod <-
          if (tpe == TransactionType.CommitToGeneration.id)
            currentPeriod.toRight("Finality activation height is required for signing CommitToGeneration transaction")
          else Right(GenerationPeriod(Height(1), Height(1), 1))
        factory = TransactionFactory(ns.wallet, ns.time, Some(currentPeriod))
        signedTx <- factory.parseRequestAndSign(c.signTxOptions.signerAddress, unsignedTx)
      } yield signedTx

      signedTx.left.map(_.toString).map(tx => Json.toBytes(tx.json()))
    }

    def doSignTxWithSK(c: Command, data: Array[Byte]): ActionResult = {
      import cats.syntax.either.*
      import com.wavesplatform.api.http.requests.InvokeScriptRequest.signedInvokeScriptRequestReads
      import com.wavesplatform.api.http.requests.SponsorFeeRequest.signedSponsorRequestFormat
      import com.wavesplatform.transaction.TransactionType.*

      val json = Json.parse(data)
      (TransactionType((json \ "type").as[Int]) match {
        case Issue           => json.as[IssueRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case Transfer        => json.as[TransferRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case Reissue         => json.as[ReissueRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case Burn            => json.as[BurnRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case Exchange        => json.as[ExchangeRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case Lease           => json.as[LeaseRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case LeaseCancel     => json.as[LeaseCancelRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case CreateAlias     => json.as[CreateAliasRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case MassTransfer    => json.as[SignedMassTransferRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case Data            => json.as[SignedDataRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case SetScript       => json.as[SignedSetScriptRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case SponsorFee      => json.as[SignedSponsorFeeRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case SetAssetScript  => json.as[SignedSetAssetScriptRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case InvokeScript    => json.as[SignedInvokeScriptRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case UpdateAssetInfo => json.as[SignedUpdateAssetInfoRequest].toTx.map(_.signWith(c.signOptions.privateKey))
        case other           => GenericError(s"Signing $other is not supported").asLeft[Transaction]
      }).leftMap(_.toString).map(_.json().toString().getBytes())
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
