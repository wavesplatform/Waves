package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EXPR}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease, Recipient}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxValidationError.WithLog
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.InvokeScriptTrace
import org.scalatest.OptionValues

class SyncDAppErrorLogTest extends PropSpec with WithDomain with OptionValues {

  val invoker: KeyPair = TxHelpers.signer(1)
  val dApp1: KeyPair   = TxHelpers.signer(2)
  val dApp2: KeyPair   = TxHelpers.signer(3)
  val dApp3: KeyPair   = TxHelpers.signer(4)

  val balances: Seq[AddrWithBalance] =
    Seq(invoker, dApp1, dApp2, dApp3).map(acc => AddrWithBalance(acc.toAddress, 10.waves))

  val settings: WavesSettings = DomainPresets.RideV6

  val errorLogLimit = 1024

  property(
    "correct error log for FailedTransactionError"
  ) {
    createTestCase(
      "testCase",
      Seq(CONST_BOOLEAN(true))
    )((tx, leaseId, assetId) =>
      s"""FailedTransactionError(code = 1, error = AccountBalanceError(VectorMap(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log = 
         |	@invokedDApp = Address(
         |		bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |	)
         |	@invokedFuncName = "testCase"
         |	i = Invocation(
         |		originCaller = Address(
         |			bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |		)
         |		payments = []
         |		callerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |		feeAssetId = Unit
         |		originCallerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |		transactionId = base58'${tx.id()}'
         |		caller = Address(
         |			bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |		)
         |		fee = 500000
         |	)
         |	testCase.@args = [
         |		true
         |	]
         |	invoke.@args = [
         |		Address(
         |			bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |		),
         |		"nested11",
         |		[],
         |		[]
         |	]
         |	invoke.@complexity = 75
         |	@complexityLimit = 51925
         |	nested11.@stateChanges = StateChanges(
         |		leases = [
         |			Lease(
         |				recipient = Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				)
         |				amount = 2
         |				nonce = 0
         |				id = base58'$leaseId'
         |			)
         |		]
         |		reissues = [
         |			Reissue(
         |				id = base58'$assetId'
         |				isReissuable = true
         |				quantity = 10
         |			)
         |		]
         |		data = [
         |			DeleteEntry(
         |				key = "nested12_key_str"
         |			)
         |		]
         |		issues = [
         |			Issue(
         |				isReissuable = true
         |				nonce = 0
         |				description = "desc"
         |				id = base58'$assetId'
         |				decimals = 0
         |				name = "testAsset"
         |				quantity = 20
         |			)
         |		]
         |		sponsorFees = [
         |			SponsorFee(
         |				id = base58'$assetId'
         |				minSponsoredAssetFee = 1
         |			)
         |		]
         |		leaseCancels = [
         |			LeaseCancel(
         |				id = base58'$leaseId'
         |			)
         |		]
         |		transfers = [
         |			ScriptTransfer(
         |				amount = 3
         |				recipient = Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				)
         |				asset = base58'$assetId'
         |			)
         |		]
         |		burns = [
         |			Burn(
         |				id = base58'$assetId'
         |				quantity = 1
         |			)
         |		]
         |		invokes = [
         |			Invoke(
         |				dApp = Address(
         |					bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |				)
         |				call = Call(
         |					function = "nested12"
         |					args = [
         |						"test_str",
         |						123
         |					]
         |				)
         |				stateChanges = StateChanges(
         |					leases = []
         |					reissues = []
         |					data = [
         |						StringEntry(
         |							key = "nested12_key_str"
         |							value = "test_str"
         |						),
         |						IntegerEntry(
         |							key = "nested12_key_int"
         |							value = 123
         |						)
         |					]
         |					issues = []
         |					sponsorFees = []
         |					leaseCancels = []
         |					transfers = []
         |					burns = []
         |					invokes = []
         |				)
         |				payments = []
         |			)
         |		]
         |	)
         |	nested11.@complexity = 111
         |	@complexityLimit = 51814
         |	nested11 = 5
         |	==.@args = [
         |		5,
         |		5
         |	]
         |	==.@complexity = 1
         |	@complexityLimit = 51813
         |	Address.@args = [
         |		base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	]
         |	dapp2 = Address(
         |		bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	)
         |	Address.@complexity = 1
         |	@complexityLimit = 51812
         |	shouldFail = true
         |	message = base58'emsY'
         |	sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |	pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	sigVerify.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	sigVerify.@complexity = 180
         |	@complexityLimit = 51632
         |	sigVerify.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	sigVerify.@complexity = 180
         |	@complexityLimit = 51452
         |	sigVerify.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	sigVerify.@complexity = 180
         |	@complexityLimit = 51272
         |	sigVerify_64Kb.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	sigVerify_64Kb.@complexity = 93
         |	@complexityLimit = 51179
         |	complex = true
         |	identityBool.@args = [
         |		true
         |	]
         |	t = true
         |	identityBool.@complexity = 1
         |	@complexityLimit = 51178
         |	cons.@args = [
         |		true,
         |		[]
         |	]
         |	cons.@complexity = 1
         |	@complexityLimit = 51177
         |	invoke.@args = [
         |		Address(
         |			bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |		),
         |		"testCase",
         |		[
         |			true
         |		],
         |		[]
         |	]
         |	invoke.@complexity = 75
         |	@complexityLimit = 51102
         |	inv = FailedTransactionError(code = 1, error = AccountBalanceError(VectorMap(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log = 
         |		@invokedDApp = Address(
         |			bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |		)
         |		@invokedFuncName = "testCase"
         |		i = Invocation(
         |			originCaller = Address(
         |				bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |			)
         |			payments = []
         |			callerPublicKey = base58'5UMMCMELXL4yZKaUiuATw6H2xoeY2k93NwzCxiMoTbrK'
         |			feeAssetId = Unit
         |			originCallerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |			transactionId = base58'${tx.id()}'
         |			caller = Address(
         |				bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |			)
         |			fee = 500000
         |		)
         |		testCase.@args = [
         |			true
         |		]
         |		Address.@args = [
         |			base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |		]
         |		dapp3 = Address(
         |			bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |		)
         |		Address.@complexity = 1
         |		@complexityLimit = 51101
         |		cons.@args = [
         |			true,
         |			[]
         |		]
         |		cons.@complexity = 1
         |		@complexityLimit = 51100
         |		AttachedPayment.@args = [
         |			Unit,
         |			10
         |		]
         |		AttachedPayment.@complexity = 1
         |		@complexityLimit = 51099
         |		cons.@args = [
         |			AttachedPayment(
         |				assetId = Unit
         |				amount = 10
         |			),
         |			[]
         |		]
         |		cons.@complexity = 1
         |		@complexityLimit = 51098
         |		invoke.@args = [
         |			Address(
         |				bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |			),
         |			"nested31",
         |			[
         |				true
         |			],
         |			[
         |				AttachedPayment(
         |					assetId = Unit
         |					amount = 10
         |				)
         |			]
         |		]
         |		invoke.@complexity = 75
         |		@complexityLimit = 51023
         |		nested31.@stateChanges = StateChanges(
         |			leases = []
         |			reissues = []
         |			data = [
         |				BooleanEntry(
         |					key = "nested31_key_bool"
         |					value = true
         |				)
         |			]
         |			issues = []
         |			sponsorFees = []
         |			leaseCancels = []
         |			transfers = []
         |			burns = []
         |			invokes = []
         |		)
         |		nested31.@complexity = 2
         |		@complexityLimit = 51021
         |		nested31 = Unit
         |		==.@args = [
         |			Unit,
         |			Unit
         |		]
         |		==.@complexity = 1
         |		@complexityLimit = 51020
         |		shouldFail = true
         |		message = base58'emsY'
         |		sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |		pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |		sigVerify_8Kb.@args = [
         |			base58'emsY',
         |			base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |			base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |		]
         |		sigVerify_8Kb.@complexity = 43
         |		@complexityLimit = 50977
         |		complexCase1 = true
         |		cons.@args = [
         |			true,
         |			[]
         |		]
         |		cons.@complexity = 1
         |		@complexityLimit = 50976
         |		invoke.@args = [
         |			Address(
         |				bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |			),
         |			"testCase",
         |			[
         |				true
         |			],
         |			[]
         |		]
         |		invoke.@complexity = 75
         |		@complexityLimit = 50901
         |		inv = FailedTransactionError(code = 1, error = AccountBalanceError(VectorMap(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log = 
         |			@invokedDApp = Address(
         |				bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |			)
         |			@invokedFuncName = "testCase"
         |			i = Invocation(
         |				originCaller = Address(
         |					bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |				)
         |				payments = []
         |				callerPublicKey = base58'5h6zeBqbczVqDG82kzFhgyeuWTv1pgKbMvGrhPq6WLwz'
         |				feeAssetId = Unit
         |				originCallerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |				transactionId = base58'${tx.id()}'
         |				caller = Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				)
         |				fee = 500000
         |			)
         |			testCase.@args = [
         |				true
         |			]
         |			cons.@args = [
         |				base58'aaa',
         |				[]
         |			]
         |			cons.@complexity = 1
         |			@complexityLimit = 50900
         |			invoke.@args = [
         |				Address(
         |					bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |				),
         |				"nested32",
         |				[
         |					base58'aaa'
         |				],
         |				[]
         |			]
         |			invoke.@complexity = 75
         |			@complexityLimit = 50825
         |			nested32.@stateChanges = StateChanges(
         |				leases = []
         |				reissues = []
         |				data = [
         |					BinaryEntry(
         |						key = "nested32_key_bin"
         |						value = base58'aaa'
         |					)
         |				]
         |				issues = []
         |				sponsorFees = []
         |				leaseCancels = []
         |				transfers = []
         |				burns = []
         |				invokes = []
         |			)
         |			nested32.@complexity = 2
         |			@complexityLimit = 50823
         |			nested32 = Unit
         |			==.@args = [
         |				Unit,
         |				Unit
         |			]
         |			==.@complexity = 1
         |			@complexityLimit = 50822
         |			message = base58'emsY'
         |			sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |			pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			sigVerify.@args = [
         |				base58'emsY',
         |				base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |				base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			]
         |			sigVerify.@complexity = 180
         |			@complexityLimit = 50642
         |			ScriptTransfer.@args = [
         |				Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				),
         |				99900000000,
         |				Unit
         |			]
         |			ScriptTransfer.@complexity = 1
         |			@complexityLimit = 50641
         |			cons.@args = [
         |				ScriptTransfer(
         |					recipient = Address(
         |						bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |					)
         |					amount = 99900000000
         |					asset = Unit
         |				),
         |				[]
         |			]
         |			cons.@complexity = 1
         |			@complexityLimit = 50640
         |		)
         |	)
         |)""".stripMargin
    )
  }

  property(
    "correct error log for ScriptExecutionError"
  ) {
    createTestCase(
      "testCase",
      Seq(CONST_BOOLEAN(false))
    )((tx, leaseId, assetId) =>
      s"""InvokeRejectError(error = AccountBalanceError(VectorMap(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log = 
         |	@invokedDApp = Address(
         |		bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |	)
         |	@invokedFuncName = "testCase"
         |	i = Invocation(
         |		originCaller = Address(
         |			bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |		)
         |		payments = []
         |		callerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |		feeAssetId = Unit
         |		originCallerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |		transactionId = base58'${tx.id()}'
         |		caller = Address(
         |			bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |		)
         |		fee = 500000
         |	)
         |	testCase.@args = [
         |		false
         |	]
         |	invoke.@args = [
         |		Address(
         |			bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |		),
         |		"nested11",
         |		[],
         |		[]
         |	]
         |	invoke.@complexity = 75
         |	@complexityLimit = 51925
         |	nested11.@stateChanges = StateChanges(
         |		leases = [
         |			Lease(
         |				recipient = Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				)
         |				amount = 2
         |				nonce = 0
         |				id = base58'$leaseId'
         |			)
         |		]
         |		reissues = [
         |			Reissue(
         |				id = base58'$assetId'
         |				isReissuable = true
         |				quantity = 10
         |			)
         |		]
         |		data = [
         |			DeleteEntry(
         |				key = "nested12_key_str"
         |			)
         |		]
         |		issues = [
         |			Issue(
         |				isReissuable = true
         |				nonce = 0
         |				description = "desc"
         |				id = base58'$assetId'
         |				decimals = 0
         |				name = "testAsset"
         |				quantity = 20
         |			)
         |		]
         |		sponsorFees = [
         |			SponsorFee(
         |				id = base58'$assetId'
         |				minSponsoredAssetFee = 1
         |			)
         |		]
         |		leaseCancels = [
         |			LeaseCancel(
         |				id = base58'$leaseId'
         |			)
         |		]
         |		transfers = [
         |			ScriptTransfer(
         |				amount = 3
         |				recipient = Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				)
         |				asset = base58'$assetId'
         |			)
         |		]
         |		burns = [
         |			Burn(
         |				id = base58'$assetId'
         |				quantity = 1
         |			)
         |		]
         |		invokes = [
         |			Invoke(
         |				dApp = Address(
         |					bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |				)
         |				call = Call(
         |					function = "nested12"
         |					args = [
         |						"test_str",
         |						123
         |					]
         |				)
         |				stateChanges = StateChanges(
         |					leases = []
         |					reissues = []
         |					data = [
         |						StringEntry(
         |							key = "nested12_key_str"
         |							value = "test_str"
         |						),
         |						IntegerEntry(
         |							key = "nested12_key_int"
         |							value = 123
         |						)
         |					]
         |					issues = []
         |					sponsorFees = []
         |					leaseCancels = []
         |					transfers = []
         |					burns = []
         |					invokes = []
         |				)
         |				payments = []
         |			)
         |		]
         |	)
         |	nested11.@complexity = 111
         |	@complexityLimit = 51814
         |	nested11 = 5
         |	==.@args = [
         |		5,
         |		5
         |	]
         |	==.@complexity = 1
         |	@complexityLimit = 51813
         |	Address.@args = [
         |		base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	]
         |	dapp2 = Address(
         |		bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	)
         |	Address.@complexity = 1
         |	@complexityLimit = 51812
         |	shouldFail = false
         |	cons.@args = [
         |		false,
         |		[]
         |	]
         |	cons.@complexity = 1
         |	@complexityLimit = 51811
         |	invoke.@args = [
         |		Address(
         |			bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |		),
         |		"testCase",
         |		[
         |			false
         |		],
         |		[]
         |	]
         |	invoke.@complexity = 75
         |	@complexityLimit = 51736
         |	inv = FailedTransactionError(code = 1, error = AccountBalanceError(VectorMap(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log = 
         |		@invokedDApp = Address(
         |			bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |		)
         |		@invokedFuncName = "testCase"
         |		i = Invocation(
         |			originCaller = Address(
         |				bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |			)
         |			payments = []
         |			callerPublicKey = base58'5UMMCMELXL4yZKaUiuATw6H2xoeY2k93NwzCxiMoTbrK'
         |			feeAssetId = Unit
         |			originCallerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |			transactionId = base58'${tx.id()}'
         |			caller = Address(
         |				bytes = base58'3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy'
         |			)
         |			fee = 500000
         |		)
         |		testCase.@args = [
         |			false
         |		]
         |		Address.@args = [
         |			base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |		]
         |		dapp3 = Address(
         |			bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |		)
         |		Address.@complexity = 1
         |		@complexityLimit = 51735
         |		cons.@args = [
         |			true,
         |			[]
         |		]
         |		cons.@complexity = 1
         |		@complexityLimit = 51734
         |		AttachedPayment.@args = [
         |			Unit,
         |			10
         |		]
         |		AttachedPayment.@complexity = 1
         |		@complexityLimit = 51733
         |		cons.@args = [
         |			AttachedPayment(
         |				assetId = Unit
         |				amount = 10
         |			),
         |			[]
         |		]
         |		cons.@complexity = 1
         |		@complexityLimit = 51732
         |		invoke.@args = [
         |			Address(
         |				bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |			),
         |			"nested31",
         |			[
         |				true
         |			],
         |			[
         |				AttachedPayment(
         |					assetId = Unit
         |					amount = 10
         |				)
         |			]
         |		]
         |		invoke.@complexity = 75
         |		@complexityLimit = 51657
         |		nested31.@stateChanges = StateChanges(
         |			leases = []
         |			reissues = []
         |			data = [
         |				BooleanEntry(
         |					key = "nested31_key_bool"
         |					value = true
         |				)
         |			]
         |			issues = []
         |			sponsorFees = []
         |			leaseCancels = []
         |			transfers = []
         |			burns = []
         |			invokes = []
         |		)
         |		nested31.@complexity = 2
         |		@complexityLimit = 51655
         |		nested31 = Unit
         |		==.@args = [
         |			Unit,
         |			Unit
         |		]
         |		==.@complexity = 1
         |		@complexityLimit = 51654
         |		shouldFail = false
         |		cons.@args = [
         |			false,
         |			[]
         |		]
         |		cons.@complexity = 1
         |		@complexityLimit = 51653
         |		invoke.@args = [
         |			Address(
         |				bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |			),
         |			"testCase",
         |			[
         |				false
         |			],
         |			[]
         |		]
         |		invoke.@complexity = 75
         |		@complexityLimit = 51578
         |		inv = FailedTransactionError(code = 1, error = AccountBalanceError(VectorMap(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log = 
         |			@invokedDApp = Address(
         |				bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |			)
         |			@invokedFuncName = "testCase"
         |			i = Invocation(
         |				originCaller = Address(
         |					bytes = base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'
         |				)
         |				payments = []
         |				callerPublicKey = base58'5h6zeBqbczVqDG82kzFhgyeuWTv1pgKbMvGrhPq6WLwz'
         |				feeAssetId = Unit
         |				originCallerPublicKey = base58'8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr'
         |				transactionId = base58'${tx.id()}'
         |				caller = Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				)
         |				fee = 500000
         |			)
         |			testCase.@args = [
         |				false
         |			]
         |			cons.@args = [
         |				base58'aaa',
         |				[]
         |			]
         |			cons.@complexity = 1
         |			@complexityLimit = 51577
         |			invoke.@args = [
         |				Address(
         |					bytes = base58'3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM'
         |				),
         |				"nested32",
         |				[
         |					base58'aaa'
         |				],
         |				[]
         |			]
         |			invoke.@complexity = 75
         |			@complexityLimit = 51502
         |			nested32.@stateChanges = StateChanges(
         |				leases = []
         |				reissues = []
         |				data = [
         |					BinaryEntry(
         |						key = "nested32_key_bin"
         |						value = base58'aaa'
         |					)
         |				]
         |				issues = []
         |				sponsorFees = []
         |				leaseCancels = []
         |				transfers = []
         |				burns = []
         |				invokes = []
         |			)
         |			nested32.@complexity = 2
         |			@complexityLimit = 51500
         |			nested32 = Unit
         |			==.@args = [
         |				Unit,
         |				Unit
         |			]
         |			==.@complexity = 1
         |			@complexityLimit = 51499
         |			message = base58'emsY'
         |			sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |			pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			sigVerify.@args = [
         |				base58'emsY',
         |				base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |				base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			]
         |			sigVerify.@complexity = 180
         |			@complexityLimit = 51319
         |			ScriptTransfer.@args = [
         |				Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				),
         |				99900000000,
         |				Unit
         |			]
         |			ScriptTransfer.@complexity = 1
         |			@complexityLimit = 51318
         |			cons.@args = [
         |				ScriptTransfer(
         |					recipient = Address(
         |						bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |					)
         |					amount = 99900000000
         |					asset = Unit
         |				),
         |				[]
         |			]
         |			cons.@complexity = 1
         |			@complexityLimit = 51317
         |		)
         |	)
         |)""".stripMargin
    )
  }

  property(s"very big error logs are shortened correctly to $errorLogLimit}") {
    val dApp = TxHelpers.signer(0)

    val setScript = TxHelpers.setScript(
      dApp,
      TestCompiler(V6).compileContract(
        s"""
           |@Callable(inv)
           |func foo() = {
           |  let a = "1".size()
           |  strict res = invoke(this, "foo", [], [])
           |  []
           |}
           | """.stripMargin
      ),
      fee = 1.waves,
      version = TxVersion.V2,
      timestamp = 0
    )

    val invoke = TxHelpers.invoke(dApp.toAddress, Some("foo"), Seq.empty, invoker = dApp, timestamp = 0)

    assertDiffEiTraced(
      Seq(
        TestBlock.create(Seq(TxHelpers.genesis(dApp.toAddress, timestamp = 0))),
        TestBlock.create(Seq(setScript))
      ),
      TestBlock.create(Seq(invoke)),
      settings.blockchainSettings.functionalitySettings,
      enableExecutionLog = true
    ) { result =>
      result.trace
        .collectFirst { case invokeTrace: InvokeScriptTrace =>
          invokeTrace.resultE match {
            case Left(w: WithLog) => w.toStringWithLog(errorLogLimit)
            case _                => fail("Result should be error with log")
          }
        }
        .foreach { error =>
          val expectedError =
            s"""FailedTransactionError(code = 1, error = ScriptRunsLimitError(DApp calls limit = 100 is exceeded), log = 
               |	@invokedDApp = Address(
               |		bytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'
               |	)
               |	@invokedFuncName = "foo"
               |	inv = Invocation(
               |		originCaller = Address(
               |			bytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'
               |		)
               |		payments = []
               |		callerPublicKey = base58'9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ'
               |		feeAssetId = Unit
               |		originCallerPublicKey = base58'9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ'
               |		transactionId = base58'CgfvaFiiQXPqvFytezf8iAAastJu5qdbr1ysPXtvpPgz'
               |		caller = Address(
               |			bytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'
               |		)
               |		fee = 500000
               |	)
               |	foo.@args = []
               |	invoke.@args = [
               |		Address(
               |			bytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'
               |		),
               |		"foo",
               |		[],
               |		[]
               |	]
               |	invoke.@complexity = 75
               |	@complexityLimit = 51925
               |	res = FailedTransactionError(code = 1, error = ScriptRunsLimitError(DApp calls limit = 100 is exceeded), log = 
               |		@invokedDApp = Address(
               |			bytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'
               |		)
               |		@invokedFuncName = "foo"
               |...
               |	)
               |)""".stripMargin

          expectedError.dropWhile(_ != '\n').tail.length shouldBe <(errorLogLimit)
          error shouldBe expectedError
        }
    }
  }

  private def createTestCase(
      funcName: String,
      args: Seq[EXPR]
  )(expectedResult: (InvokeScriptTransaction, ByteStr, ByteStr) => String): Unit = {
    withDomain(settings, balances) { d =>
      val invoke = TxHelpers.invoke(dApp1.toAddress, func = Some(funcName), args = args, invoker = invoker)
      d.appendBlock(
        TxHelpers.setScript(dApp1, dAppContract1(dApp2.toAddress)),
        TxHelpers.setScript(dApp2, dAppContract2(dApp3.toAddress)),
        TxHelpers.setScript(dApp3, dAppContract3)
      )
      d.transactionDifferWithLog(invoke)
        .trace
        .collectFirst { case invokeTrace: InvokeScriptTrace =>
          invokeTrace.resultE match {
            case Left(w: WithLog) => w.toStringWithLog(Int.MaxValue)
            case _                => fail("Result should be error with log")
          }
        }
        .foreach { error =>
          val leaseId = Lease.calculateId(Lease(Recipient.Address(ByteStr(dApp2.toAddress.bytes)), 2, 0), invoke.id())
          val assetId = Issue.calculateId(0, "desc", isReissuable = true, "testAsset", 20, 0, invoke.id())

          error shouldBe expectedResult(invoke, leaseId, assetId)
        }
    }
  }

  private def dAppContract1(dApp2: Address): Script =
    TestCompiler(V6).compileContract(
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |let dapp2 = Address(base58'${dApp2.toString}')
         |  
         |let message = base58'emsY'
         |let pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |let sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |
         |let complex = sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify(message, sig, pub) && sigVerify_64Kb(message, sig, pub) 
         |
         |func identityBool(t: Boolean) = t
         |
         |@Callable(i)
         |func nested12(str: String, int: Int) = {
         |  [StringEntry("nested12_key_str", str), IntegerEntry("nested12_key_int", int)]
         |}
         |
         |@Callable(i)
         |func nested11() = {
         |  strict nested12 = invoke(this, "nested12", ["test_str", 123], [])
         |  let issue = Issue("testAsset", "desc", 20, 0, true)
         |  let assetId = calculateAssetId(issue)
         |  let lease = Lease(dapp2, 2)
         |  (
         |    [
         |      DeleteEntry("nested12_key_str"), 
         |      issue,
         |      Burn(assetId, 1),
         |      Reissue(assetId, 10, true),
         |      lease,
         |      LeaseCancel(calculateLeaseId(lease)),
         |      ScriptTransfer(dapp2, 3, assetId),
         |      SponsorFee(assetId, 1)
         |    ], 
         |    5
         |  )
         |}
         |
         |@Callable(i)
         |func testCase(shouldFail: Boolean) = {
         |  strict nested11 = invoke(this, "nested11", [], [])
         |  strict inv = invoke(dapp2, "testCase", [shouldFail && complex && identityBool(true)], [])
         |  []
         |}
         |""".stripMargin
    )

  private def dAppContract2(dApp3: Address): Script =
    TestCompiler(V6).compileContract(
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |let dapp3 = Address(base58'${dApp3.toString}')
         |  
         |let message = base58'emsY'
         |let pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |let sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |
         |let complexCase1 = sigVerify_8Kb(message, sig, pub) 
         |
         |@Callable(i)
         |func testCase(shouldFail: Boolean) = {
         |  strict nested31 = invoke(dapp3, "nested31", [true], [AttachedPayment(unit, 10)])
         |  strict inv = invoke(dapp3, "testCase", [shouldFail && complexCase1], [])
         |  []
         |}
         |""".stripMargin
    )

  private def dAppContract3: Script =
    TestCompiler(V6).compileContract(
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |  
         |let message = base58'emsY'
         |let pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |let sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |
         |@Callable(i)
         |func nested31(bool: Boolean) = {
         |  [BooleanEntry("nested31_key_bool", bool)]
         |}
         |
         |@Callable(i)
         |func nested32(bs: ByteVector) = {
         |  [BinaryEntry("nested32_key_bin", bs)]
         |}
         |
         |@Callable(i)
         |func testCase(bool:Boolean) = {
         |  strict nested32 = invoke(this, "nested32", [base58'aaa'], [])
         |  if (sigVerify(message, sig, pub)) then
         |    [ScriptTransfer(i.caller, 99900000000, unit)]
         |  else []
         |}
         |""".stripMargin
    )
}
