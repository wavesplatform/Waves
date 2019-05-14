package com.wavesplatform.lang.impl

import com.wavesplatform.common.crypto.RSA.DigestAlgorithm

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.{Object, UndefOr, native}

@native
@JSGlobalScope
object Global extends Object {
  def base58Encode(input: ArrayBuffer): String          = native
  def base58Decode(input: String): UndefOr[ArrayBuffer] = native
  def base64Encode(input: ArrayBuffer): String          = native
  def base64Decode(input: String): UndefOr[ArrayBuffer] = native

  def curve25519verify(message: ArrayBuffer, sig: ArrayBuffer, pub: ArrayBuffer): Boolean                = native
  def rsaVerify(alg: DigestAlgorithm, message: ArrayBuffer, sig: ArrayBuffer, pub: ArrayBuffer): Boolean = native
  def keccak256(message: ArrayBuffer): ArrayBuffer                                                       = native
  def blake2b256(message: ArrayBuffer): ArrayBuffer                                                      = native
  def sha256(message: ArrayBuffer): ArrayBuffer                                                          = native

  def merkleVerify(root: ArrayBuffer, proof: ArrayBuffer, data: ArrayBuffer): Boolean = native
}
