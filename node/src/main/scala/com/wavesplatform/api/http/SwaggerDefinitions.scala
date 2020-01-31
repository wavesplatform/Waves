package com.wavesplatform.api.http

import io.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

object SwaggerDefinitions {

  @ApiModel("Deleted")
  case class DeletedDesc(deleted: Boolean)
  @ApiModel("ValidityCheck")
  case class ValidityCheckDesc(isValid: Boolean)
  @ApiModel("Seed")
  case class SeedDesc(address: String, seed: String)
  @ApiModel("Address")
  case class AddressDesc(address: String)

  @ApiModel("NxtConsensusData")
  class NxtConsensusDataDesc(
      @(ApiModelProperty @field)(name = "base-target", required = true)
      val baseTarget: Long,
      @(ApiModelProperty @field)(name = "generation-signature", required = true) val generationSignature: String
  )

  @ApiModel("BlockHeader")
  abstract class BlockHeaderDesc(
      val timestamp: Long,
      val version: Short,
      val height: Int,
      val totalFee: Int,
      val reference: String,
      val generator: String,
      val signature: String,
      @(ApiModelProperty @field)(name = "nxt-consensus", required = true)
      val consensusData: NxtConsensusDataDesc,
      val blocksize: Int,
      val transactionCount: Int,
      @(ApiModelProperty @field)(allowEmptyValue = true, dataType = "[Ljava.lang.Short;")
      val features: Set[Short],
      @(ApiModelProperty @field)(allowEmptyValue = true)
      val reward: Long,
      @(ApiModelProperty @field)(allowEmptyValue = true)
      val desiredReward: Long
  )

  @ApiModel("Transaction")
  abstract class TransactionDesc(
      val id: String,
      @(ApiModelProperty @field)(required = false)
      val version: Short,
      val timestamp: Long,
      `type`: Short,
      @(ApiModelProperty @field)(required = false)
      val chainId: Short
  )

  @ApiModel("Block")
  abstract class BlockDesc(
      val transactions: List[TransactionDesc],
      timestamp: Long,
      version: Short,
      height: Int,
      totalFee: Int,
      reference: String,
      generator: String,
      signature: String,
      consensusData: NxtConsensusDataDesc,
      blocksize: Int,
      transactionCount: Int,
      features: Set[Short],
      reward: Long,
      desiredReward: Long
  ) extends BlockHeaderDesc(
        timestamp,
        version,
        height,
        totalFee,
        reference,
        generator,
        signature,
        consensusData,
        blocksize,
        transactionCount,
        features,
        reward,
        desiredReward
      )

  @ApiModel("Delay")
  case class DelayDesc(delay: Long)
  @ApiModel("Height")
  case class HeightDesc(height: Int)
}
