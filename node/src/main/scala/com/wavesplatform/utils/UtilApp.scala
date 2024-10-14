package com.wavesplatform.utils

import com.google.common.io.ByteStreams
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, FastBase58}
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.settings.WavesSettings
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
import scala.annotation.nowarn

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
    case object SignTxWithSk    extends Mode
  }

  case class CompileOptions(assetScript: Boolean = false)
  case class SignOptions(privateKey: PrivateKey = null)
  case class VerifyOptions(publicKey: PublicKey = null, signature: ByteStr = ByteStr.empty, checkWeakPk: Boolean = false)
  case class HashOptions(mode: String = "fast")
  case class SignTxOptions(signerAddress: String = "")

  sealed trait Input
  object Input {
    case object StdIn                   extends Input
    final case class File(file: String) extends Input
    final case class Str(str: String)   extends Input
  }

  case class Command(
      mode: Command.Mode = null,
      configFile: Option[String] = None,
      inputData: Input = Input.StdIn,
      outputFile: Option[String] = None,
      inFormat: String = "plain",
      outFormat: String = "plain",
      compileOptions: CompileOptions = CompileOptions(),
      signOptions: SignOptions = SignOptions(),
      verifyOptions: VerifyOptions = VerifyOptions(),
      hashOptions: HashOptions = HashOptions(),
      signTxOptions: SignTxOptions = SignTxOptions()
  )

  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, Command()) match {
      case Some(cmd) =>
        val settings = Application.loadApplicationConfig(cmd.configFile.map(new File(_)))
        val inBytes        = IO.readInput(cmd)
        val result = {
          val doAction = cmd.mode match {
            case Command.CompileScript   => Actions.doCompile(settings) _
            case Command.DecompileScript => Actions.doDecompile _
            case Command.SignBytes       => Actions.doSign _
            case Command.VerifySignature => Actions.doVerify _
            case Command.CreateKeyPair   => Actions.doCreateKeyPair _
            case Command.Hash            => Actions.doHash _
            case Command.SerializeTx     => Actions.doSerializeTx _
            case Command.SignTx          => Actions.doSignTx(new NodeState(cmd)) _
            case Command.SignTxWithSk    => Actions.doSignTxWithSK _
          }
          doAction(cmd, inBytes)
        }

        result match {
          case Left(value)     => System.err.println(s"Error executing command: $value")
          case Right(outBytes) => IO.writeOutput(cmd, outBytes)
        }

      case None =>
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
          .action((_, c) => c.copy(mode = Command.CompileScript))
          .text("Compiles RIDE script"),
        cmd("decompile")
          .action((_, c) => c.copy(mode = Command.DecompileScript))
          .text("Decompiles binary script to RIDE code")
      ),
      cmd("hash")
        .children(
          opt[String]('m', "mode")
            .valueName("<fast|secure>")
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
              .action((s, c) => c.copy(verifyOptions = c.verifyOptions.copy(signature = ByteStr.decodeBase58(s).get))),
            opt[Boolean]("check-weak-pk")
              .abbr("cwpk")
              .text("Check for weak public key")
              .valueName("<true|false>")
              .action((checkPk, c) => c.copy(verifyOptions = c.verifyOptions.copy(checkWeakPk = checkPk)))
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
            opt[String]("signer-address")
              .abbr("sa")
              .text("Signer address (requires corresponding key in wallet.dat)")
              .action((a, c) => c.copy(signTxOptions = c.signTxOptions.copy(signerAddress = a)))
          ),
        cmd("sign-with-sk")
          .text("Sign JSON transaction with private key")
          .action((_, c) => c.copy(mode = Command.SignTxWithSk))
          .children(
            opt[String]("private-key")
              .abbr("sk")
              .text("Private key")
              .action((a, c) => c.copy(signOptions = c.signOptions.copy(privateKey = PrivateKey(Base58.decode(a)))))
          )
      ),
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

    def doDecompile(c: Command, data: Array[Byte]): ActionResult = {
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

    def doSignTx(ns: NodeState)(c: Command, data: Array[Byte]): ActionResult =
      TransactionFactory
        .parseRequestAndSign(ns.wallet, c.signTxOptions.signerAddress, ns.time, Json.parse(data).as[JsObject])
        .left
        .map(_.toString)
        .map(tx => Json.toBytes(tx.json()))

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
