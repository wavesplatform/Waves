package com.wavesplatform.lang.impl

import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scala.scalajs.js.{Promise, UndefOr}

@js.native
@JSImport("@waves/ts-lib-crypto", JSImport.Namespace)
private object TsLibCrypto extends js.Object {
  def verifySignature(publicKey: Uint8Array, message: Uint8Array, signature: Uint8Array): Boolean                    = js.native
  def rsaVerify(publicKey: Uint8Array, message: Uint8Array, signature: Uint8Array, digest: UndefOr[String]): Boolean = js.native
  def keccak(bytes: Uint8Array): Uint8Array                                                                          = js.native
  def blake2b(bytes: Uint8Array): Uint8Array                                                                         = js.native
  def sha256(bytes: Uint8Array): String                                                                              = js.native
  def merkleVerify(rootHash: Uint8Array, merkleProof: Uint8Array, leafData: Uint8Array): Boolean                     = js.native
}

@js.native
@JSImport("axios", JSImport.Namespace)
private object Axios extends js.Object {
  def get(url: String, config: js.Dynamic): Promise[js.Dynamic] = js.native
}

object Global {
  private def toUint8Array(buf: ArrayBuffer): Uint8Array = new Uint8Array(buf)

  // ts-lib-crypto sha256 returns a hex string; decode it to ArrayBuffer
  private def hexToArrayBuffer(hex: String): ArrayBuffer = {
    val len    = hex.length / 2
    val result = new Uint8Array(len)
    var i      = 0
    while (i < len) {
      result(i) = (Integer.parseInt(hex.substring(i * 2, i * 2 + 2), 16) & 0xff).toByte
      i += 1
    }
    result.buffer
  }

  // Map DigestAlgorithm ADT to the string (or undefined) ts-lib-crypto expects
  private def digestToString(alg: DigestAlgorithm): UndefOr[String] = alg match {
    case RSA.NONE    => js.undefined
    case RSA.MD5     => "MD5"
    case RSA.SHA1    => "SHA1"
    case RSA.SHA224  => "SHA224"
    case RSA.SHA256  => "SHA256"
    case RSA.SHA384  => "SHA384"
    case RSA.SHA512  => "SHA512"
    case RSA.SHA3224 => "SHA3-224"
    case RSA.SHA3256 => "SHA3-256"
    case RSA.SHA3384 => "SHA3-384"
    case RSA.SHA3512 => "SHA3-512"
  }

  // Public API — identical signatures to the previous @JSGlobalScope object

  def curve25519verify(message: ArrayBuffer, sig: ArrayBuffer, pub: ArrayBuffer): Boolean =
    TsLibCrypto.verifySignature(toUint8Array(pub), toUint8Array(message), toUint8Array(sig))

  def rsaVerify(alg: DigestAlgorithm, message: ArrayBuffer, sig: ArrayBuffer, pub: ArrayBuffer): Boolean =
    TsLibCrypto.rsaVerify(toUint8Array(pub), toUint8Array(message), toUint8Array(sig), digestToString(alg))

  def keccak256(message: ArrayBuffer): ArrayBuffer  = TsLibCrypto.keccak(toUint8Array(message)).buffer
  def blake2b256(message: ArrayBuffer): ArrayBuffer = TsLibCrypto.blake2b(toUint8Array(message)).buffer
  def sha256(message: ArrayBuffer): ArrayBuffer     = hexToArrayBuffer(TsLibCrypto.sha256(toUint8Array(message)))

  def merkleVerify(root: ArrayBuffer, proof: ArrayBuffer, data: ArrayBuffer): Boolean =
    TsLibCrypto.merkleVerify(toUint8Array(root), toUint8Array(proof), toUint8Array(data))

  def httpGet(params: js.Dynamic): Promise[js.Dynamic] = {
    val url = params.url.asInstanceOf[UndefOr[String]]
    if (url.isEmpty) {
      js.Promise.resolve[js.Dynamic](
        js.Dynamic.literal("url" -> params.url, "status" -> 404, "body" -> "url is undefined")
      )
    } else {
      Axios
        .get(url.get, js.Dynamic.literal(validateStatus = ((_: Int) => true): js.Function1[Int, Boolean]))
        .`then`[js.Dynamic] { resp =>
          val status = resp.status.asInstanceOf[Int]
          val bodyStr =
            if (js.typeOf(resp.data) != "string") js.JSON.stringify(resp.data)
            else resp.data.asInstanceOf[String]
          js.Dynamic.literal("url" -> params.url, "status" -> status, "body" -> bodyStr)
        }
    }
  }
}
