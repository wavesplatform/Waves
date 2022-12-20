package com.wavesplatform.lang.estimator.dapp

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.test.PropSpec

class ContractGlobalDeclarationTest extends PropSpec {
  property("estimate contract script with user functions") {
    val neutrinoScript =
      """
        |# Main Smart Contract of Neutrino Protocol
        |# Implemented actions: Swap, Bond Liquidation, Leasing
        |#
        |let revisionNum = ""
        |
        |#-------------------Base functions----------------------
        |func getStringOrFail(address: Address, key: String)  = address.getString(key).valueOrErrorMessage(makeString(["mandatory ", address.toString(), ".", key, " is not defined"], ""))
        |
        |# workaround to reduce size
        |func lcalc(l: Lease) = {
        |  calculateLeaseId(l)
        |}
        |
        |func getNumberByKey(key: String) = {
        |    getInteger(this, key).valueOrElse(0)
        |}
        |func getStringByKey(key: String) = {
        |    getString(this, key).valueOrElse("")
        |}
        |func getBoolByKey(key: String) = {
        |    getBoolean(this, key).valueOrElse(false)
        |}
        |func getNumberByAddressAndKey(address: Address, key: String) = {
        |    getInteger(address, key).valueOrElse(0)
        |}
        |func getStringByAddressAndKey(address: String, key: String) = {
        |     getString(addressFromStringValue(address), key).valueOrElse("")
        |}
        |func getBoolByAddressAndKey(address: Address, key: String) = {
        |     getBoolean(address, key).valueOrElse(false)
        |}
        |func asAnyList(v: Any) = {
        |  match v {
        |    case l: List[Any] => l
        |    case _ => throw("fail to cast into List[Any]")
        |  }
        |}
        |func asString(v: Any) = {
        |  match v {
        |    case s: String => s
        |    case _ => throw("fail to cast into String")
        |  }
        |}
        |func asInt(v: Any) = {
        |  match v {
        |    case i: Int => i
        |    case _ => throw("fail to cast into Int")
        |  }
        |}
        |func asBytes(val: Any) = {
        |  match val {
        |    case valByte: ByteVector => valByte
        |    case _ => throw("fail to cast into ByteVector")
        |  }
        |}
        |func asPayment(v: Any) = {
        |  match v {
        |    case p: AttachedPayment => p
        |    case _ => throw("fail to cast into AttachedPayment")
        |  }
        |}
        |
        |func asSwapParamsSTRUCT(v: Any) = {
        |  match v {
        |    case struct: (Int, Int, Int, Int, Int, Int, Int) => struct
        |    case _ => throw("fail to cast into Tuple5 ints")
        |  }
        |}
        |
        |#-------------------Constants---------------------------
        |let SEP = "__"
        |let LISTSEP = ":"
        |let WAVELET = 100000000
        |let PAULI = 1000000
        |let PRICELET = 1000000 # 10^6
        |let DEFAULTSWAPFEE = 20000 # 0.02 * 1000000 or 2%
        |let BRPROTECTED = 100000 # if BR <= 10% then use SURF during swap USDN->WAVES
        |
        |let IdxNetAmount = 0
        |let IdxFeeAmount = 1
        |let IdxGrossAmount = 2
        |
        |# data indices from controlConfig
        |let IdxControlCfgNeutrinoDapp     = 1
        |let IdxControlCfgAuctionDapp      = 2
        |let IdxControlCfgRpdDapp          = 3
        |let IdxControlCfgMathDapp         = 4
        |let IdxControlCfgLiquidationDapp  = 5
        |let IdxControlCfgRestDapp         = 6
        |let IdxControlCfgNodeRegistryDapp = 7
        |let IdxControlCfgNsbtStakingDapp  = 8
        |let IdxControlCfgMediatorDapp     = 9
        |let IdxControlCfgSurfStakingDapp  = 10
        |let IdxControlCfgGnsbtControllerDapp = 11
        |let IdxControlCfgRestV2Dapp       = 12
        |let IdxControlCfgGovernanceDapp   = 13
        |
        |func keyControlAddress() = "%s%s__config__controlAddress"
        |func keyControlCfg()     = "%s__controlConfig"
        |
        |func readControlCfgOrFail(control: Address) = split_4C(control.getStringOrFail(keyControlCfg()), SEP)
        |func getContractAddressOrFail(cfg: List[String], idx: Int) = cfg[idx].addressFromString()
        |  .valueOrErrorMessage("Control cfg doesn't contain address at index " + idx.toString())
        |
        |# GLOBAL VARIABLES
        |let controlContract = this.getString(keyControlAddress()).valueOrElse("3P5Bfd58PPfNvBM2Hy8QfbcDqMeNtzg7KfP").addressFromStringValue()
        |let controlCfg = controlContract.readControlCfgOrFail()
        |let mathContract = controlCfg.getContractAddressOrFail(IdxControlCfgMathDapp)
        |let nsbtStakingContract = controlCfg.getContractAddressOrFail(IdxControlCfgNsbtStakingDapp)
        |let surfStakingContract = controlCfg.getContractAddressOrFail(IdxControlCfgSurfStakingDapp)
        |let gnsbtControllerContract = controlCfg.getContractAddressOrFail(IdxControlCfgGnsbtControllerDapp)
        |let auctionContract = controlCfg.getContractAddressOrFail(IdxControlCfgAuctionDapp)
        |let nodeRegistryContract = controlCfg.getContractAddressOrFail(IdxControlCfgNodeRegistryDapp)
        |let govContract = controlCfg.getContractAddressOrFail(IdxControlCfgGovernanceDapp)
        |
        |#-------------------Constructor-------------------------
        |let NeutrinoAssetIdKey = "neutrino_asset_id"
        |let BondAssetIdKey = "bond_asset_id"
        |let AuctionContractKey = "auction_contract"
        |let NsbtStakingContractKey = "nsbtStakingContract"
        |let LiquidationContractKey = "liquidation_contract"
        |let RPDContractKey = "rpd_contract"
        |let ContolContractKey = "control_contract"
        |let MathContractKey = "math_contract"
        |let BalanceWavesLockIntervalKey = "balance_waves_lock_interval"
        |let BalanceNeutrinoLockIntervalKey = "balance_neutrino_lock_interval"
        |let MinWavesSwapAmountKey = "min_waves_swap_amount"
        |let MinNeutrinoSwapAmountKey = "min_neutrino_swap_amount"
        |let NodeOracleProviderPubKeyKey = "node_oracle_provider"
        |let NeutrinoOutFeePartKey = "neutrinoOut_swap_feePart"
        |let WavesOutFeePartKey = "wavesOut_swap_feePart"
        |
        |#------Common----------------
        |
        |#---Nodes Registry contract--
        |func keyNodeRegistry(address: String)       = "%s__" + address
        |
        |#------Control contract-------
        |let PriceKey = "price"
        |
        |let PriceIndexKey = "price_index"
        |let IsBlockedKey = "is_blocked"
        |func getPriceHistoryKey(block: Int) = PriceKey + "_" + toString(block)
        |func getHeightPriceByIndexKey(index: Int) = PriceIndexKey + "_" + toString(index)
        |func getStakingNodeByIndex(idx: Int) = getStringByKey(makeString(["%s%d%s", "lease", toString(idx), "nodeAddress"], SEP))
        |func getStakingNodeAddressByIndex(idx: Int) = addressFromStringValue(getStakingNodeByIndex(idx))
        |
        |func getReservedAmountForSponsorship() =
        |    getInteger(this, makeString(["%s%s", "lease", "sponsorshipWavesReserve"], SEP)).valueOrElse(1000 * WAVELET)
        |
        |#------This contract----------
        |#-------------------Keys-------------------
        |# TODO need to move into zero
        |func getBalanceUnlockBlockKey(owner: String)               = "balance_unlock_block_" + owner
        |func getLeaseIdKey(nodeIndex: Int)                         = makeString(["%s%d%s", "lease", toString(nodeIndex), "id"], SEP)
        |func getLeaseIdByAddressKey(nodeAddress: String)           = makeString(["%s%s%s", "leaseByAddress", nodeAddress, "id"], SEP)
        |func getLeaseAmountKey(nodeIndex: Int)                     = makeString(["%s%d%s", "lease", toString(nodeIndex), "amount"], SEP)
        |func getLeaseAmountByAddressKey(nodeAddress: String)       = makeString(["%s%s%s", "leaseByAddress", nodeAddress, "amount"], SEP)
        |func getLeaseGroupNodeListKey(groupNum: Int)               = makeString(["%s%d%s", "leaseGroup", groupNum.toString(), "nodeList"], SEP)
        |
        |func minSwapAmountKEY(swapType: String)                    = "min_" + swapType + "_swap_amount"
        |func totalLockedKEY(swapType: String)                      = "balance_lock_" + swapType
        |func totalLockedByUserKEY(swapType: String, owner: String) = makeString(["balance_lock", swapType, owner], "_")
        |func balanceLockIntervalKEY(swapType: String)              = "balance_" + swapType + "_lock_interval" # number of blocks after user could withdraw funds
        |func nodeBalanceLockIntervalKEY()                          = "balance_node_lock_interval"
        |func outFeePartKEY(swapType: String)                       = swapType + "Out_swap_feePart"
        |func swapsTimeframeKEY()                                   = "swaps_timeframe"
        |func brProtectedKEY()                                      = "min_BR_protection_level"
        |
        |#-------------------State Reading functions-------------------
        |func minSwapAmountREAD(swapType: String) = this.getInteger(minSwapAmountKEY(swapType)).valueOrElse(0)
        |func swapsTimeframeREAD() = this.getInteger(swapsTimeframeKEY()).valueOrElse(1440)
        |func totalLockedREAD(swapType: String) = this.getInteger(totalLockedKEY(swapType)).valueOrElse(0)
        |func totalLockedByUserREAD(swapType: String, owner: String) = this.getInteger(totalLockedByUserKEY(swapType, owner)).valueOrElse(0)
        |func balanceLockIntervalREAD(swapType: String) = this.getInteger(balanceLockIntervalKEY(swapType)).valueOrElse(1440) # number og blocks after user could withdraw funds
        |func nodeBalanceLockIntervalREAD() = this.getInteger(nodeBalanceLockIntervalKEY()).valueOrElse(1)
        |func keySwapUserSpentInPeriod(userAddress: String) = ["%s%s", "swapUserSpentInPeriod", userAddress].makeString(SEP)
        |func keyUserLastSwapHeight(userAddress: String) = ["%s%s", "userLastSwapHeight", userAddress].makeString(SEP)
        |
        |#-------------------Convert functions-------------------
        |func convertNeutrinoToWaves(amount: Int, price: Int) = fraction(fraction(amount, PRICELET, price),WAVELET, PAULI)
        |func convertWavesToNeutrino(amount: Int, price: Int) = fraction(fraction(amount, price, PRICELET), PAULI, WAVELET)
        |func convertWavesToBond(amount: Int, price: Int) = convertWavesToNeutrino(amount, price) # it's here to be more explicit with convertation
        |func convertJsonArrayToList(jsonArray: String) = {
        |   jsonArray.split(",")
        |}
        |
        |#-------------------Failures-------------------
        |func minSwapAmountFAIL(swapType: String, minSwapAmount: Int) = throw("The specified amount in " + swapType + " swap is less than the required minimum of " + toString(minSwapAmount))
        |func emergencyShutdownFAIL() = throw("contract is blocked by EMERGENCY SHUTDOWN actions untill reactivation by emergency oracles")
        |
        |func priceIndexFAIL(index: Int, pi: Int, ih: Int, uh: Int, pih: Int) =
        |            throw("invalid price history index: index=" + toString(index)
        |                + " priceIndex=" + toString(pi)
        |                + " indexHeight=" + toString(ih)
        |                + " unlockHeight=" + toString(uh)
        |                + " prevIndexHeight=" + toString(pih))
        |
        |#-------------------Global vars-------------------------
        |
        |let neutrinoAssetId = getStringByKey(NeutrinoAssetIdKey).fromBase58String()
        |let priceIndex = getNumberByAddressAndKey(controlContract, PriceIndexKey) # Last price history iterator from control.ride
        |let isBlocked = getBoolByAddressAndKey(controlContract, IsBlockedKey) # Checks for contract locks that might happen after attacks.  The var is read from control contract
        |let nodeOracleProviderPubKey = fromBase58String(getStringByKey(NodeOracleProviderPubKeyKey))
        |let bondAssetId = fromBase58String("6nSpVyNH7yM69eg446wrQR94ipbbcmZMU1ENPwanC97g") # NSBT with 6 decimals as USDN does
        |let deprecatedBondAssetId = fromBase58String("975akZBfnMj513U7MZaHKzQrmsEx5aE3wdWKTrHBhbjF") # USDNB with 0 decimals
        |
        |let neutrinoContract = this
        |#-------------------Global vars deficit, locked & supply -------------------------
        |let currentPrice = getNumberByAddressAndKey(controlContract, PriceKey) # The value from control.ride
        |
        |#-------------------Verifier Functions----------------------
        |func checkIsValidMinSponsoredFee(tx: SponsorFeeTransaction) = {
        |    let MINTRANSFERFEE = 100000 #wavelets (to support smart assets)
        |    let SponsoredFeeUpperBound = 1000 # % of fee profits higther than real fee for transfer
        |    let realNeutrinoFee = convertWavesToNeutrino(MINTRANSFERFEE, currentPrice) # in paulis
        |    let minNeutrinoFee = realNeutrinoFee * 2 # 100%
        |    let maxNeutrinoFee = fraction(realNeutrinoFee, SponsoredFeeUpperBound, 100)
        |
        |    let inputFee = tx.minSponsoredAssetFee.value()
        |
        |    inputFee >= minNeutrinoFee && inputFee <= maxNeutrinoFee && tx.assetId == neutrinoAssetId
        |}
        |
        |#------Control contract------
        |# The func is reading price from control.ride price history
        |func getPriceHistory(block: Int) = getNumberByAddressAndKey(controlContract, getPriceHistoryKey(block))
        |# The func is reading from control.ride price history heights
        |func getHeightPriceByIndex(index: Int) = getNumberByAddressAndKey(controlContract, getHeightPriceByIndexKey(index))
        |
        |#------NSBT Staking contract------
        |func keyLockParamUserAmount(userAddress: String) = ["%s%s%s", "paramByUser", userAddress, "amount"].makeString(SEP)
        |
        |
        |#------This contract---------
        |let sIdxSwapType                 = 1
        |let sIdxStatus                   = 2
        |let sIdxInAmount                 = 3
        |let sIdxStartHeight              = 7
        |let sIdxStartTimestamp           = 8
        |let sIdxSelfUnlockHeight         = 11
        |let sIdxMinRand                  = 15
        |let sIdxMaxRand                  = 16
        |
        |func swapKEY(userAddress: String, txId: String) = {
        |  makeString(["%s%s", userAddress, txId], SEP)
        |}
        |
        |func strSwapDATA(swapType: String, status: String, inAmount: String, price: String, outNetAmount: String, outFeeAmount: String,
        |                 startHeight: String, startTimestamp: String, endHeight: String, endTimestamp: String,
        |                 selfUnlockHeight: String, randUnlockHeight: String, index: String, withdrawTxId: String,
        |                 randMin: String, randMax: String, outSurfAmt: String, br: String) = {
        |  makeString(["%s%s%d%d%d%d%d%d%d%d%d%d%d%s%d%d%d%d",
        |      swapType,                     # 1
        |      status,                       # 2
        |      inAmount,                     # 3
        |      price,                        # 4
        |      outNetAmount,                 # 5
        |      outFeeAmount,                 # 6
        |      startHeight,                  # 7
        |      startTimestamp,               # 8
        |      endHeight,                    # 9
        |      endTimestamp,                 # 10
        |      selfUnlockHeight,             # 11
        |      randUnlockHeight,             # 12
        |      index,                        # 13
        |      withdrawTxId,                 # 14
        |      randMin,                      # 15
        |      randMax,                      # 16
        |      outSurfAmt,                   # 17
        |      br                            # 18
        |      ],
        |  SEP)
        |}
        |
        |func pendingSwapDATA(swapType: String, inAssetAmount: Int, selfUnlockHeight: Int) = {
        |  strSwapDATA(
        |      swapType,                       # 1
        |      "PENDING",                      # 2
        |      inAssetAmount.toString(),       # 3
        |      "0",                            # 4
        |      "0",                            # 5
        |      "0",                            # 6
        |      height.toString(),              # 7
        |      lastBlock.timestamp.toString(), # 8
        |      "0",                            # 9
        |      "0",                            # 10
        |      selfUnlockHeight.toString(),    # 11
        |      "0",                            # 12
        |      "0",                            # 13
        |      "NULL",                         # 14
        |      "0",                            # 15
        |      "0",                            # 16
        |      "0",                            # 17
        |      "0"                             # 18
        |  )
        |}
        |
        |func finishSwapDATA(dataArray: List[String], price: Int, outNetAmount: Int, outFeeAmount: Int, randUnlockHeight: Int,
        |                    index: Int, withdrawTxId: String, outSurfAmt: Int, br: Int) = {
        |  strSwapDATA(
        |      dataArray[sIdxSwapType],        # 1
        |      "FINISHED",                     # 2
        |      dataArray[sIdxInAmount],        # 3
        |      price.toString(),               # 4
        |      outNetAmount.toString(),        # 5
        |      outFeeAmount.toString(),        # 6
        |      dataArray[sIdxStartHeight],     # 7
        |      dataArray[sIdxStartTimestamp],  # 8
        |      height.toString(),              # 9
        |      lastBlock.timestamp.toString(), # 10
        |      dataArray[sIdxSelfUnlockHeight],# 11
        |      randUnlockHeight.toString(),    # 12
        |      index.toString(),               # 13
        |      withdrawTxId,                   # 14
        |      dataArray[sIdxMinRand],         # 15
        |      dataArray[sIdxMaxRand],         # 16
        |      outSurfAmt.toString(),          # 17
        |      br.toString()                   # 18
        |  )
        |}
        |
        |func swapDataFailOrREAD(userAddress: String, swapTxId: String) = {
        |  let swapKey = swapKEY(userAddress, swapTxId)
        |  this.getString(swapKey)
        |    .valueOrErrorMessage("no swap data for " + swapKey)
        |    .split(SEP)
        |}
        |
        |func applyFees(amountOutGross: Int, inAmtToSURF: Int, feePart: Int) = {
        |  let feeAmount = fraction(amountOutGross, feePart, PAULI)
        |  [amountOutGross - feeAmount, feeAmount]
        |}
        |
        |func abs(x: Int) = if (x < 0) then -x else x
        |
        |func selectNode(unleaseAmount: Int) = {
        |    let amountToLease = wavesBalance(neutrinoContract).available - unleaseAmount - getReservedAmountForSponsorship()
        |
        |    let oldLeased0 = getNumberByKey(getLeaseAmountKey(0))
        |    let oldLeased1 = getNumberByKey(getLeaseAmountKey(1))
        |    let newLeased0 = amountToLease + oldLeased0
        |    let newLeased1 = amountToLease + oldLeased1
        |
        |    if (newLeased0 > 0 || newLeased1 > 0) then {
        |        # balancing the nodes
        |        let delta0 = abs(newLeased0 - oldLeased1)
        |        let delta1 = abs(newLeased1 - oldLeased0)
        |        # 0 node is a priority
        |        if (delta0 <= delta1) then (0, newLeased0) else (1, newLeased1)
        |    } else (-1, 0)
        |}
        |
        |func thisOnly(i: Invocation) = {
        |  if (i.caller != this) then {
        |    throw("Permission denied: this contract only allowed")
        |  } else true
        |}
        |
        |# prepare list of actions to lease available waves or cancel lease in case of usdn2waves swap
        |func prepareUnleaseAndLease(unleaseAmount: Int) = {
        |    let nodeTuple       = selectNode(unleaseAmount) # balancing waves by 2 nodes
        |    let nodeIndex       = nodeTuple._1
        |    let newLeaseAmount  = nodeTuple._2
        |
        |    if (newLeaseAmount > 0) then {
        |        let leaseIdKey = getLeaseIdKey(nodeIndex)
        |        let oldLease = getBinary(this, leaseIdKey)
        |        let unleaseOrEmpty = if (oldLease.isDefined()) then [LeaseCancel(oldLease.value())] else []
        |        let leaseAmountKey = getLeaseAmountKey(nodeIndex)
        |        let lease = Lease(getStakingNodeAddressByIndex(nodeIndex), newLeaseAmount)
        |
        |        unleaseOrEmpty ++ [
        |            lease,
        |            BinaryEntry(leaseIdKey, lcalc(lease)),
        |            IntegerEntry(getLeaseAmountKey(nodeIndex), newLeaseAmount)]
        |    } else []
        |}
        |
        |func readNodeInfo(nodeIdx: Int) = {
        |  let nodeAddress = getStakingNodeAddressByIndex(nodeIdx)
        |  let leasedAmtKEY = getLeaseAmountKey(nodeIdx)
        |  let leasedAmt = leasedAmtKEY.getNumberByKey()
        |
        |  let leaseIdKEY = getLeaseIdKey(nodeIdx)
        |  let leaseId = this.getBinary(leaseIdKEY).value()
        |
        |  (nodeAddress, leasedAmtKEY, leasedAmt, leaseIdKEY, leaseId)
        |}
        |
        |#-------------------MAIN LOGIC----------------------
        |
        |func commonSwap(swapType: String, pmtAmount: Int, userAddressStr: String, txId58: String, swapParamsByUserSYSREADONLY: (Int,Int,Int,Int,Int,Int,Int)) = {
        |  let swapLimitSpent    = swapParamsByUserSYSREADONLY._2
        |  let blcks2LmtReset    = swapParamsByUserSYSREADONLY._3
        |  let wavesSwapLimitMax = swapParamsByUserSYSREADONLY._6
        |  let usdnSwapLimitMax  = swapParamsByUserSYSREADONLY._7
        |
        |  let minSwapAmount         = minSwapAmountREAD(swapType)
        |  let totalLocked           = totalLockedREAD(swapType)
        |  let totalLockedByUser     = totalLockedByUserREAD(swapType, userAddressStr)
        |  let nodeAddress           = getStakingNodeByIndex(0)
        |  let priceByIndex          = priceIndex.getHeightPriceByIndex().getPriceHistory()
        |  let isSwapByNode          = nodeAddress == userAddressStr
        |
        |  let balanceLockMaxInterval = if (isSwapByNode) then nodeBalanceLockIntervalREAD() else balanceLockIntervalREAD(swapType)
        |  let selfUnlockHeight       = height + balanceLockMaxInterval
        |  let swapUsdnVolume         = if (swapType == "neutrino") then pmtAmount else pmtAmount.convertWavesToNeutrino(priceByIndex)
        |  let swapLimitMax           = if (swapType == "neutrino") then usdnSwapLimitMax else wavesSwapLimitMax.convertWavesToNeutrino(priceByIndex)
        |
        |  if (pmtAmount < minSwapAmount) then minSwapAmountFAIL(swapType, minSwapAmount) else
        |  if (!isSwapByNode && swapLimitSpent > 0) then throw("You have exceeded swap limit! Next allowed swap height is " + (height + blcks2LmtReset).toString()) else
        |  if (!isSwapByNode && swapUsdnVolume > swapLimitMax) then throw("You have exceeded your swap limit! Requested: "+ toString(swapUsdnVolume) + ", available: " + toString(swapLimitMax)) else
        |  if (isBlocked) then emergencyShutdownFAIL() else  # see control.ride
        |
        |  let leasePart = if (swapType == "waves") then prepareUnleaseAndLease(0) else []
        |
        |  ([
        |      IntegerEntry(keySwapUserSpentInPeriod(userAddressStr), swapUsdnVolume),
        |      IntegerEntry(keyUserLastSwapHeight(userAddressStr), height),
        |      IntegerEntry(totalLockedByUserKEY(swapType, userAddressStr), totalLockedByUser + pmtAmount),
        |      IntegerEntry(getBalanceUnlockBlockKey(userAddressStr), selfUnlockHeight),
        |      IntegerEntry(totalLockedKEY(swapType), totalLocked + pmtAmount),
        |      StringEntry(
        |        swapKEY(userAddressStr, txId58),
        |        pendingSwapDATA(swapType, pmtAmount, selfUnlockHeight))
        |    ] ++ leasePart, unit)
        |
        |}
        |
        |#indices for calcNeutinoMetricsREADONLY result array
        |let nMetricIdxPrice = 0
        |let nMetricIdxUsdnLockedBalance = 1
        |let nMetricIdxWavesLockedBalance = 2
        |let nMetricIdxReserve = 3
        |let nMetricIdxReserveInUsdn = 4
        |let nMetricIdxUsdnSupply = 5
        |let nMetricIdxSurplus = 6
        |let nMetricIdxSurplusPercent = 7
        |let nMetricIdxBR = 8 # BR with 6 decimals
        |let nMetricIdxNsbtSupply = 9
        |let nMetricIdxMaxNsbtSupply = 10
        |let nMetricIdxSurfSupply = 11
        |
        |# surfFunctionREADONLY result array indices
        |let bFuncIdxSurf = 0
        |let bFuncIdxWaves = 1
        |let bFuncIdxUsdn = 2
        |let bFuncIdxReserveStart = 3
        |let bFuncIdxSupplyStart = 4
        |let bFuncIdxBRStart = 5
        |let bFuncIdxReserveEnd = 6
        |let bFuncIdxSupplyEnd = 7
        |let bFuncIdxBREnd = 8
        |let bFuncIdxRest = 9
        |let bFuncIdxWavesPrice = 10
        |
        |func calcWithdrawW2U(wavesIn: Int, price: Int) = {
        |  let outAmtGross = convertWavesToNeutrino(wavesIn, price)
        |  (
        |    outAmtGross,      # gross outAmount (fees are not applied yet)
        |    neutrinoAssetId,  # outAssetId is USDN
        |    0,                # part of inAmount that is converted into SURF to protect BR
        |    unit,             # inAssetId is WAVES
        |    0,                # amount to unlease
        |    wavesIn,          # debug - part of inAmount that is swapped into out asset
        |    0,                # debug - max allowed usdn amount to reach BR protection level
        |    0,                # debug - part of inAmount that is used BEFORE reaching BR protection level
        |    0                 # debug - part of inAmount that is used AFTER reaching BR protection level
        |  )
        |}
        |
        |func calcWithdrawU2W(usdnIn: Int, price: Int, br: Int, reservesInUsdn: Int, usdnSupply: Int) = {
        |  let brProtected       = this.getInteger(brProtectedKEY()).valueOrElse(BRPROTECTED)
        |
        |  let maxAllowedUsdnBeforeMinBr = if (br <= brProtected) then 0 else {
        |    fraction(reservesInUsdn - fraction(brProtected, usdnSupply, PAULI), PAULI, PAULI - brProtected)
        |  }
        |
        |  let allowedUsdnBeforeMinBr =
        |      if (usdnIn > maxAllowedUsdnBeforeMinBr) then maxAllowedUsdnBeforeMinBr else usdnIn
        |
        |  let allowedUsdnAfterMinBr =
        |      if (usdnIn > maxAllowedUsdnBeforeMinBr) then fraction(usdnIn - maxAllowedUsdnBeforeMinBr, br, PAULI) else 0
        |
        |  let allowedUsdn = allowedUsdnBeforeMinBr + allowedUsdnAfterMinBr
        |  let usdn2SURF = usdnIn - allowedUsdn
        |
        |  let outAmtGross = convertNeutrinoToWaves(allowedUsdn, price)
        |  (
        |    outAmtGross,                # gross outAmount (fees are not applied yet)
        |    unit,                       # waves_id
        |    usdn2SURF,                  # part of inAmount that is converted into SURF to protect BR
        |    neutrinoAssetId,            # inAssetId is WAVES
        |    outAmtGross,                # amount to unlease
        |    allowedUsdn,                # debug - part of inAmount that is swapped into out asset
        |    maxAllowedUsdnBeforeMinBr,  # debug - max allowed usdn amount to reach BR protection level
        |    allowedUsdnBeforeMinBr,     # debug - part of inAmount that is used BEFORE reaching BR protection level
        |    allowedUsdnAfterMinBr       # debug - part of inAmount that is used AFTER reaching BR protection level
        |  )
        |}
        |
        |func calcWithdraw(swapType: String, inAmount: Int, price: Int, neutrinoMetrics: List[Any]) = {
        |  let outFeePart        = this.getInteger(outFeePartKEY(swapType)).valueOrElse(DEFAULTSWAPFEE)
        |  if (outFeePart < 0 || outFeePart >= PAULI) then throw("invalid outFeePart config for " + swapType + " swap: outFeePart=" + outFeePart.toString()) else
        |
        |  let brProtected       = this.getInteger(brProtectedKEY()).valueOrElse(BRPROTECTED)
        |
        |  let BR                = neutrinoMetrics[nMetricIdxBR].asInt()
        |  let reservesInUsdn    = neutrinoMetrics[nMetricIdxReserveInUsdn].asInt()
        |  let usdnSupply        = neutrinoMetrics[nMetricIdxUsdnSupply].asInt()
        |
        |  let outDataTuple =
        |    if (swapType == "waves")    then calcWithdrawW2U(inAmount, price) else
        |    if (swapType == "neutrino") then calcWithdrawU2W(inAmount, price, BR, reservesInUsdn, usdnSupply)
        |    else throw("Unsupported swap type " + swapType)
        |
        |  let outAmtGross       = outDataTuple._1
        |  let outAssetId        = outDataTuple._2
        |  let inAmtToSurfPart   = outDataTuple._3
        |  let inAssetId         = outDataTuple._4
        |  let unleaseAmt        = outDataTuple._5
        |
        |  let payoutsArray = applyFees(outAmtGross, inAmtToSurfPart, outFeePart)
        |  let outNetAmt = payoutsArray[IdxNetAmount]
        |  let outFeeAmt = payoutsArray[IdxFeeAmount]
        |
        |  let outSurfAmt = if (inAmtToSurfPart <= 0) then 0 else {
        |    let surfResult = mathContract.invoke("surfFunctionREADONLY", [inAmtToSurfPart, inAssetId], []).asAnyList()
        |    surfResult[bFuncIdxSurf].asInt()
        |  }
        |
        |  # WARNING: if u modify then need to check RestV2
        |  (outNetAmt, outAssetId, outSurfAmt, inAmtToSurfPart, unleaseAmt, outFeeAmt, outAmtGross)
        |}
        |
        |# TODO move everything into withdraw - no need to keep separate function
        |func commonWithdraw(account : String, index: Int, swapTxId: String, withdrawTxId: String, neutrinoMetrics: List[Any]) = {
        |    let userAddress = addressFromStringValue(account)
        |
        |    let dataArray         = swapDataFailOrREAD(account, swapTxId)
        |    let selfUnlockHeight  = dataArray[sIdxSelfUnlockHeight].parseIntValue()
        |    let swapType          = dataArray[sIdxSwapType]
        |    let inAmount          = dataArray[sIdxInAmount].parseIntValue()
        |    let swapStatus        = dataArray[sIdxStatus]
        |    let startHeight       = dataArray[sIdxStartHeight].parseIntValue()
        |
        |    let outFeePart        = this.getInteger(outFeePartKEY(swapType)).valueOrElse(DEFAULTSWAPFEE)
        |    let totalLocked       = totalLockedREAD(swapType)
        |    let totalLockedByUser = totalLockedByUserREAD(swapType, account)
        |
        |    let unlockHeight = selfUnlockHeight
        |
        |    let indexHeight = getHeightPriceByIndex(index)
        |    let prevIndexHeight = getHeightPriceByIndex(index-1)
        |    let priceByIndex = getPriceHistory(indexHeight)
        |
        |    if (isBlocked) then emergencyShutdownFAIL() else
        |    if (swapStatus != "PENDING") then throw("swap has been already processed") else
        |    if (unlockHeight > height) then throw("please wait for: " + toString(unlockHeight) + " block height to withdraw funds") else
        |    if (index > priceIndex
        |          || indexHeight < unlockHeight
        |          || (prevIndexHeight != 0 && unlockHeight <= prevIndexHeight)) then priceIndexFAIL(index, priceIndex, indexHeight, unlockHeight, prevIndexHeight) else
        |
        |    let withdrawTuple = calcWithdraw(swapType, inAmount, priceByIndex, neutrinoMetrics)
        |    let outNetAmount    = withdrawTuple._1
        |    let outAssetId      = withdrawTuple._2
        |    let outSurfAmt      = withdrawTuple._3
        |    #let inAmtToSurfPart = withdrawTuple._4
        |    let unleaseAmt      = withdrawTuple._5
        |    let outFeeAmount    = withdrawTuple._6
        |    let outAmtGross     = withdrawTuple._7
        |
        |    if (outAmtGross <= 0) then throw("balance equals zero") else
        |
        |    let BR = neutrinoMetrics[nMetricIdxBR].asInt()
        |    let state = [
        |      IntegerEntry(totalLockedByUserKEY(swapType, account), totalLockedByUser - inAmount),
        |      IntegerEntry(totalLockedKEY(swapType), totalLocked - inAmount),
        |      ScriptTransfer(userAddress, outNetAmount, outAssetId),
        |      StringEntry(
        |        swapKEY(account, swapTxId),
        |        finishSwapDATA(dataArray, priceByIndex, outNetAmount, outFeeAmount, unlockHeight, index, withdrawTxId, outSurfAmt, BR))
        |    ]
        |
        |    strict surfCondition = if (outSurfAmt > 0) then {
        |       strict issueResult = auctionContract.invoke("issueSurf", [outSurfAmt, account], [])
        |       0
        |    } else 0
        |
        |    (state, AttachedPayment(outAssetId, outFeeAmount), unleaseAmt)
        |}
        |
        |# governance contract
        |func keyApplyInProgress() = "%s__applyInProgress"
        |func keyProposalDataById(proposalId: Int) = "%s%d__proposalData__" + proposalId.toString()
        |
        |# indices to access proposal data fields (static)
        |let govIdxTxIds = 9
        |
        |# The transaction cannot be added to the blockchain if the timestamp value is more than 2 hours behind
        |# or 1.5 hours ahead of current block timestamp
        |func validateUpdate(tx: Transaction|Order) = {
        |    match(tx) {
        |        case o: Order => throw("Orders aren't allowed")
        |        case t: Transaction => {
        |            let txId = toBase58String(t.id)
        |            let proposalId = govContract.getInteger(keyApplyInProgress()).valueOrErrorMessage("Apply is not happening")
        |            let txList = govContract.getStringOrFail(keyProposalDataById(proposalId)).split(SEP)[govIdxTxIds].split(LISTSEP)
        |            if (!txList.indexOf(txId).isDefined()) then throw("Unknown txId: " + txId + " for proposalId=" + proposalId.toString()) else
        |
        |            true
        |        }
        |    }
        |}
        |
        |#-------------------Callable----------------------
        |
        |@Callable(i)
        |func constructor(
        |  neutrinoAssetIdPrm: String,
        |  bondAssetIdPrm: String,
        |  auctionContractPrm: String,
        |  liquidationContractPrm: String,
        |  rpdContractPrm: String,
        |  nodeOracleProviderPubKeyPrm: String,
        |  balanceWavesLockIntervalPrm: Int,
        |  balanceNeutrinoLockIntervalPrm: Int,
        |  minWavesSwapAmountPrm: Int,
        |  minNeutrinoSwapAmountPrm: Int,
        |  neutrinoOutFeePartPrm: Int,
        |  wavesOutFeePartPrm: Int) = {
        |
        |  strict checkCaller = i.thisOnly()
        |  if (i.payments.size() != 0) then throw("no payments allowed") else
        |
        |  [
        |    StringEntry(NeutrinoAssetIdKey, neutrinoAssetIdPrm),
        |    StringEntry(BondAssetIdKey, bondAssetIdPrm),
        |    StringEntry(AuctionContractKey, auctionContractPrm), # ignored
        |    StringEntry(LiquidationContractKey, liquidationContractPrm), # ignored
        |    StringEntry(RPDContractKey, rpdContractPrm), #ignored
        |    StringEntry(NodeOracleProviderPubKeyKey, nodeOracleProviderPubKeyPrm),
        |    IntegerEntry(BalanceWavesLockIntervalKey, balanceWavesLockIntervalPrm),
        |    IntegerEntry(BalanceNeutrinoLockIntervalKey, balanceNeutrinoLockIntervalPrm),
        |    IntegerEntry(MinWavesSwapAmountKey, minWavesSwapAmountPrm),
        |    IntegerEntry(MinNeutrinoSwapAmountKey, minNeutrinoSwapAmountPrm),
        |    IntegerEntry(NeutrinoOutFeePartKey, neutrinoOutFeePartPrm),
        |    IntegerEntry(WavesOutFeePartKey, wavesOutFeePartPrm)
        |  ]
        |}
        |
        |@Callable(i)
        |func constructorV2(mathContract: String, nsbtStakingContract: String, swapsTimeframeBlocks: Int) = {
        |  strict checkCaller = i.thisOnly()
        |  if (i.payments.size() != 0) then throw("no payments allowed") else
        |  [
        |    StringEntry(MathContractKey, mathContract),
        |    StringEntry(NsbtStakingContractKey, nsbtStakingContract),
        |    IntegerEntry(swapsTimeframeKEY(), swapsTimeframeBlocks)
        |  ]
        |}
        |
        |# Instant swap of WAVES to Neutrino token at the current price on the smart contract
        |# [called by user]
        |@Callable(i)
        |func swapWavesToNeutrino() = {
        |    if (i.payments.size() != 1) then throw("swapWavesToNeutrino require only one payment") else
        |    let pmt = i.payments[0].value()
        |    if (isDefined(pmt.assetId)) then throw("Only Waves token is allowed for swapping.") else
        |
        |    let userAddress = i.caller.toString()
        |    let txId58 = i.transactionId.toBase58String()
        |
        |    let swapParamsSTRUCT = this.invoke("swapParamsByUserSYSREADONLY", [userAddress, 0], []).asSwapParamsSTRUCT()
        |
        |    let commonSwapResult =  commonSwap("waves", pmt.amount, userAddress, txId58, swapParamsSTRUCT)
        |    commonSwapResult
        |}
        |
        |# Swap request of Neutrino to WAVES. After {balanceLockInterval} blocks, WAVES tokens will be available for withdrawal
        |# via {withdraw(account : String)} method at the price that is current at the time when {balanceLockInterval} is reached
        |# [called by user]
        |@Callable(i)
        |func swapNeutrinoToWaves() = {
        |    if (i.payments.size() != 1) then throw("swapNeutrinoToWaves require only one payment") else
        |    let pmt = i.payments[0].value()
        |    if (pmt.assetId != neutrinoAssetId) then throw("Only appropriate Neutrino tokens are allowed for swapping.") else
        |
        |    let userAddress = i.caller.toString()
        |    let txId58 = i.transactionId.toBase58String()
        |
        |    let swapParamsSTRUCT = this.invoke("swapParamsByUserSYSREADONLY", [userAddress, 0], []).asSwapParamsSTRUCT()
        |
        |    let commonSwapResult = commonSwap("neutrino", pmt.amount, userAddress, txId58, swapParamsSTRUCT)
        |    commonSwapResult
        |}
        |
        |# Withdraw WAVES from smart contract after {swapNeutrinoToWaves()} request has reached {balanceLockInterval} height
        |# at the price that is current at the time when {balanceLockInterval} is reached
        |# [called by user]
        |@Callable(i)
        |func withdraw(account: String, index: Int, swapTxId: String) = {
        |    let txId = i.transactionId.toBase58String()
        |    if (i.payments.size() != 0) then throw("no payments allowed") else
        |
        |    let neutrinoMetrics = mathContract.invoke("calcNeutinoMetricsREADONLY", [], []).asAnyList()
        |    let BR = neutrinoMetrics[nMetricIdxBR].asInt()
        |
        |    let commonTuple = commonWithdraw(account, index, swapTxId, txId, neutrinoMetrics)
        |    let state       = commonTuple._1
        |    let fee         = commonTuple._2
        |    let unleaseAmt  = commonTuple._3
        |
        |    strict unleaseInvOrEmpty = this.invoke("internalUnleaseAndLease", [unleaseAmt], [])
        |    let gnsbtData = gnsbtControllerContract.invoke("gnsbtInfoSYSREADONLY", ["", 0, 0], []).asAnyList()
        |    let gnsbtAmtTotal           = gnsbtData[1].asInt()
        |    let gnsbtAmtFromSurfTotal   = gnsbtData[3].asAnyList()[3].asInt()
        |
        |    let surfFeeAmt1 = if (gnsbtAmtTotal != 0) then fraction(fee.amount, gnsbtAmtFromSurfTotal, gnsbtAmtTotal) else 0
        |    let surfFeeAmt2 = if (gnsbtAmtTotal != 0) then fraction(fee.amount, PAULI - BR, PAULI) else 0
        |    let surfFeeAmt = max([surfFeeAmt1, surfFeeAmt2])
        |    let nsbtFeeAmt = fee.amount - surfFeeAmt
        |
        |    strict surfDeposit = if (surfFeeAmt > 0) then {
        |      strict surfInv = surfStakingContract.invoke("deposit", [], [AttachedPayment(fee.assetId, surfFeeAmt)])
        |      []
        |    } else {[]}
        |
        |    strict nsbtDeposit = if (nsbtFeeAmt > 0) then {
        |      strict nsbtInv = nsbtStakingContract.invoke("deposit", [], [AttachedPayment(fee.assetId, nsbtFeeAmt)])
        |      []
        |    } else {[]}
        |
        |    state
        |}
        |
        |@Callable(i)
        |func internalUnleaseAndLease(unleaseAmount: Int) = {
        |  if (i.caller != this) then throw("internalUnleaseAndLease is not public method") else
        |  prepareUnleaseAndLease(unleaseAmount)
        |}
        |
        |# Callback for auction contract to transfer USDN to user
        |@Callable(i)
        |func transferUsdnToUser(amount: Int, addr: String) = {
        |    if (i.caller != auctionContract) then throw("Only auction contract is authorized") else
        |
        |    [ScriptTransfer(addressFromStringValue(addr), amount, neutrinoAssetId)]
        |}
        |
        |# Accept waves from auction after buyNsbt/buySurf to lease them immediately
        |# also from governance after creating new voting
        |@Callable(i)
        |func acceptWaves() = {
        |    if (i.caller != auctionContract && i.caller != govContract)
        |        then throw("Currently only auction and governance contracts are allowed to call")
        |    else
        |        (prepareUnleaseAndLease(0), "success")
        |}
        |
        |@Callable(i)
        |func approveLeasings(nListS: String, groupNum: Int, lAmt: Int) = {
        |  let nIdxs = [0, 1, 2, 3, 4, 5, 6, 7]
        |
        |  let mngPubS = getString("%s%s__cfg__leasingManagerPub").valueOrElse("7AUMX54ukYMYvPmma7yoFf5NjZhs4Bu5nz3Ez9EV8sur")
        |  let mngPub = mngPubS.fromBase58String()
        |
        |  let nodeRegAddrStr = getString("%s%s__cfg__nodesRegistryAddress").valueOrElse("3P9vKqQKjUdmpXAfiWau8krREYAY1Xr69pE")
        |  let nodeRegAddr = nodeRegAddrStr.addressFromStringValue()
        |
        |  let lGroupNodeListKEY = getLeaseGroupNodeListKey(groupNum)
        |  let lGrNodeOpt = this.getString(lGroupNodeListKEY)
        |  if (lGrNodeOpt.isDefined()) then throw("group " + groupNum.toString() + " already initialized") else
        |
        |  let nList = nListS.split(SEP)
        |  let expCount = nIdxs.size()
        |
        |  if (i.callerPublicKey != mngPub) then throw("approveLeasings not authorized") else
        |
        |  let (nAddr0, lAmtKEY0, lAmt0, lIdKEY0, lId0) = readNodeInfo(0)
        |
        |  let newL0 = Lease(nAddr0, lAmt0 - lAmt * expCount)
        |
        |  strict validation = nodeRegAddr.invoke("validateAndApproveLeasings", [nListS], [])
        |
        |  func forEachNodeValidateAndGenerateLease(a: List[Lease|BinaryEntry|IntegerEntry], i: Int) = {
        |    let node = nList[i]
        |    let la = Lease(node.addressFromStringValue(), lAmt)
        |    a++[la,
        |        BinaryEntry(getLeaseIdByAddressKey(node), lcalc(la)),
        |        IntegerEntry(getLeaseAmountByAddressKey(node), lAmt)]
        |  }
        |
        |  [StringEntry(lGroupNodeListKEY, nListS),
        |    BinaryEntry(lIdKEY0, lcalc(newL0)),
        |    IntegerEntry(lAmtKEY0, newL0.amount),
        |    LeaseCancel(lId0),
        |    newL0
        |  ]
        |    ++ FOLD<8>(nIdxs, [], forEachNodeValidateAndGenerateLease)
        |}
        |
        |@Callable(i)
        |func rebalanceLeasings(amount: Int, groupNum: Int) = {
        |  let nIdxs = [0, 1, 2, 3, 4, 5, 6, 7]
        |
        |  let mngPubS = getString("%s%s__cfg__leasingManagerPub").valueOrElse("7AUMX54ukYMYvPmma7yoFf5NjZhs4Bu5nz3Ez9EV8sur")
        |  let mngPub = mngPubS.fromBase58String()
        |
        |  let lGroupNodeListKEY = getLeaseGroupNodeListKey(groupNum)
        |  let nListS = this.getStringOrFail(lGroupNodeListKEY)
        |  let nList = nListS.split(SEP)
        |
        |  if (i.callerPublicKey != mngPub) then throw("rebalanceLeasings not authorized") else
        |
        |  let unleaseAmt = amount / nList.size() + 1
        |  let (nAddr0, lAmtKEY0, lAmt0, lIdKEY0, lId0) = readNodeInfo(0)
        |
        |  let newL0 = Lease(nAddr0, lAmt0 + unleaseAmt * nList.size())
        |
        |  func forEachNodeDoUnlease(a: List[Lease|BinaryEntry|IntegerEntry], i: Int) = {
        |    let node = nList[i]
        |    let lIdKEY = getLeaseIdByAddressKey(node)
        |    let lId = this.getBinaryValue(lIdKEY)
        |    let lAmtKEY = getLeaseAmountByAddressKey(node)
        |    let lAmt = this.getIntegerValue(lAmtKEY)
        |
        |    let ula = LeaseCancel(lId.value())
        |    let la  = Lease(node.addressFromStringValue(), lAmt - unleaseAmt)
        |    a++[LeaseCancel(lId.value()),
        |        la,
        |        BinaryEntry(lIdKEY, lcalc(la)),
        |        IntegerEntry(lAmtKEY, la.amount)]
        |  }
        |
        |  FOLD<8>(nIdxs, [], forEachNodeDoUnlease)
        |    ++ [
        |      BinaryEntry(lIdKEY0, lcalc(newL0)),
        |      IntegerEntry(lAmtKEY0, newL0.amount),
        |      LeaseCancel(lId0),
        |      newL0]
        |}
        |
        |# READONLY methods
        |@Callable(i)
        |func swapParamsByUserSYSREADONLY(userAddressStr: String, gnsbtDiff: Int) = {
        |  let gnsbtData = gnsbtControllerContract.invoke("gnsbtInfoSYSREADONLY", [userAddressStr, 0, 0], []).asAnyList()
        |
        |  let gnsbtAmt      = gnsbtData[0].asInt() + gnsbtDiff
        |  let gnsbtAmtTotal = gnsbtData[1].asInt() + gnsbtDiff
        |
        |  let swapLimitData = mathContract.invoke("calcSwapLimitREADONLY", [gnsbtAmt], []).asAnyList()
        |  let wavesSwapLimitInUsdnMax = swapLimitData[0].asInt()
        |  let wavesSwapLimitMax       = swapLimitData[1].asInt()
        |  let usdnSwapLimitMax        = swapLimitData[2].asInt()
        |
        |  let lastSwapHeight = this.getInteger(keyUserLastSwapHeight(userAddressStr)).valueOrElse(0)
        |  let swapLimitTimelifeBlocks = swapsTimeframeREAD()
        |  let passedBlocksAfterLastSwap = height - lastSwapHeight
        |  let isSwapTimelifeNew = passedBlocksAfterLastSwap >= swapLimitTimelifeBlocks
        |  let swapLimitSpentInUsdn = if (isSwapTimelifeNew) then 0 else this.getInteger(keySwapUserSpentInPeriod(userAddressStr)).valueOrElse(0)
        |  let blcks2LmtReset = if (isSwapTimelifeNew) then 0 else swapLimitTimelifeBlocks - passedBlocksAfterLastSwap
        |
        |  # WARNING if you change returned value - MUST have to change "asSwapParamsSTRUCT" function
        |  ([], (wavesSwapLimitInUsdnMax, swapLimitSpentInUsdn, blcks2LmtReset, gnsbtAmt, gnsbtAmtTotal, wavesSwapLimitMax, usdnSwapLimitMax))
        |}
        |
        |@Callable(i)
        |func calcWithdrawResultSYSREADONLY(swapType: String, inAmount: Int, price: Int) = {
        |  let neutrinoMetrics = mathContract.invoke("calcNeutinoMetricsREADONLY", [], []).asAnyList()
        |  ([], calcWithdraw(swapType, inAmount, price, neutrinoMetrics))
        |}
        |
        |@Callable(i)
        |func replaceCommunityNode(oldAddrStr: String, newAddrStr: String, groupNum: Int, penaltyAmount: Int) = {
        |  let mngPubS = getString("%s%s__cfg__leasingManagerPub").valueOrElse("7AUMX54ukYMYvPmma7yoFf5NjZhs4Bu5nz3Ez9EV8sur")
        |  let mngPub = mngPubS.fromBase58String()
        |  if (i.callerPublicKey != mngPub) then throw("replaceCommunityNode not authorized") else
        |
        |  let groupKey = getLeaseGroupNodeListKey(groupNum)
        |  let groupNodeListS = this.getStringOrFail(groupKey)
        |  if (!groupNodeListS.contains(oldAddrStr)) then throw("Group " + groupNum.toString() + " does not contain address " + oldAddrStr) else
        |
        |  strict doReplace = nodeRegistryContract.invoke("replaceApprovedNode", [oldAddrStr, newAddrStr, groupNum, penaltyAmount], [])
        |
        |  let oldLeaseIdKey = getLeaseIdByAddressKey(oldAddrStr)
        |  let oldLeaseAmtKey = getLeaseAmountByAddressKey(oldAddrStr)
        |  let leaseAmt = getIntegerValue(oldLeaseAmtKey)
        |  let newLeaseIdKey = getLeaseIdByAddressKey(oldAddrStr)
        |  let newLeaseAmtKey = getLeaseAmountByAddressKey(oldAddrStr)
        |  let newLease = Lease(newAddrStr.addressFromStringValue(), leaseAmt)
        |  let updatedGroupNodeListS = groupNodeListS.split(oldAddrStr).makeString(newAddrStr)
        |
        |  ([LeaseCancel(getBinaryValue(oldLeaseIdKey)),
        |    DeleteEntry(oldLeaseIdKey),
        |    DeleteEntry(oldLeaseAmtKey),
        |    StringEntry(groupKey, updatedGroupNodeListS),
        |    newLease,
        |    BinaryEntry(newLeaseIdKey, lcalc(newLease)),
        |    IntegerEntry(newLeaseAmtKey, leaseAmt)
        |  ], unit)
        |}
        |
        |@Verifier(tx)
        |func verify() = {
        |    let pubKeyAdminsListStr = makeString([
        |        "GJdLSaLiv5K7xuejac8mcRcHoyo3dPrESrvktG3a6MAR",
        |        "EYwZmURd5KKaQRBjsVa6g8DPisFoS6SovRJtFiL5gMHU",
        |        "DtmAfuDdCrHK8spdAeAYzq6MsZegeD9gnsrpuTRkCbVA",
        |        "5WRXFSjwcTbNfKcJs8ZqXmSSWYsSVJUtMvMqZj5hH4Nc"
        |    ], SEP)
        |
        |    let pubKeyAdminsList = controlContract.getString("%s__multisig")
        |          .valueOrElse(pubKeyAdminsListStr)
        |          .split(SEP)
        |
        |    let count =
        |        (if(sigVerify(tx.bodyBytes, tx.proofs[0], fromBase58String(pubKeyAdminsList[0]))) then 1 else 0) +
        |        (if(sigVerify(tx.bodyBytes, tx.proofs[1], fromBase58String(pubKeyAdminsList[1]))) then 1 else 0) +
        |        (if(sigVerify(tx.bodyBytes, tx.proofs[2], fromBase58String(pubKeyAdminsList[2]))) then 1 else 0) +
        |        (if(sigVerify(tx.bodyBytes, tx.proofs[3], fromBase58String(pubKeyAdminsList[3]))) then 2 else 0)
        |
        |    if (isBlocked &&
        |      controlContract.getStringValue("is_blocked_caller") == govContract.toString()) then validateUpdate(tx) else {
        |        match tx {
        |            case sponsorTx: SponsorFeeTransaction =>
        |                checkIsValidMinSponsoredFee(sponsorTx) && count >= 3
        |            case _ =>
        |                count >= 3
        |        }
        |    }
        |}
      """.stripMargin

    val dApp      = TestCompiler(V6).compileContract(neutrinoScript).expr
    val estimator = ScriptEstimatorV3(fixOverflow = true, overhead = false)

    val oldFunctionsCosts = ContractScript.oldGlobalFunctionsCosts(V6, dApp, estimator).explicitGet()
    val oldLetsCosts      = ContractScript.oldGlobalLetsCosts(V6, dApp, estimator).explicitGet()
    val (oldMaxAnnotatedComplexity, oldAnnotatedComplexities) =
      ContractScript.estimateComplexityExact(V6, dApp, estimator, fixEstimateOfVerifier = true).explicitGet()

    val newCosts = ContractScript.estimateFully(V6, dApp, estimator).explicitGet()
    newCosts.globalFunctionsCosts shouldBe oldFunctionsCosts
    newCosts.globalLetsCosts shouldBe oldLetsCosts
    newCosts.maxAnnotatedComplexity shouldBe oldMaxAnnotatedComplexity
    newCosts.annotatedComplexities shouldBe oldAnnotatedComplexities
  }
}
