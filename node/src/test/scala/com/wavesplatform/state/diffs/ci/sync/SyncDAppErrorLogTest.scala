package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EXPR}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease, Recipient}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.InvokeScriptTrace

class SyncDAppErrorLogTest extends PropSpec with WithDomain {

  val invoker: KeyPair = TxHelpers.signer(1)
  val dApp1: KeyPair   = TxHelpers.signer(2)
  val dApp2: KeyPair   = TxHelpers.signer(3)
  val dApp3: KeyPair   = TxHelpers.signer(4)

  val balances: Seq[AddrWithBalance] =
    Seq(invoker, dApp1, dApp2, dApp3).map(acc => AddrWithBalance(acc.toAddress, 10.waves))

  val settings: WavesSettings = DomainPresets.RideV6

  property(
    "correct error log for FailedTransactionError"
  ) {
    createTestCase(
      "testCase",
      Seq(CONST_BOOLEAN(true))
    )((tx, leaseId, assetId) =>
      s"""Left(FailedTransactionError(code = 1, error = AccountBalanceError(Map(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log =
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
         |	nested11 = 5
         |	==.@args = [
         |		5,
         |		5
         |	]
         |	Address.@args = [
         |		base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	]
         |	dapp2 = Address(
         |		bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	)
         |	shouldFail = true
         |	message = base58'emsY'
         |	sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |	pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	sigVerify.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	sigVerify.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	sigVerify.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	sigVerify_64Kb.@args = [
         |		base58'emsY',
         |		base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |		base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |	]
         |	complex = true
         |	identityBool.@args = [
         |		true
         |	]
         |	t = true
         |	cons.@args = [
         |		true,
         |		[]
         |	]
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
         |	inv = Left(CommonError(FailedTransactionError(code = 1, error = AccountBalanceError(Map(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log =
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
         |		cons.@args = [
         |			true,
         |			[]
         |		]
         |		AttachedPayment.@args = [
         |			Unit,
         |			10
         |		]
         |		cons.@args = [
         |			AttachedPayment(
         |				assetId = Unit
         |				amount = 10
         |			),
         |			[]
         |		]
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
         |		nested31 = Unit
         |		==.@args = [
         |			Unit,
         |			Unit
         |		]
         |		shouldFail = true
         |		message = base58'emsY'
         |		sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |		pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |		sigVerify_8Kb.@args = [
         |			base58'emsY',
         |			base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |			base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |		]
         |		complexCase1 = true
         |		cons.@args = [
         |			true,
         |			[]
         |		]
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
         |		inv = Left(CommonError(FailedTransactionError(code = 1, error = AccountBalanceError(Map(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log =
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
         |			nested32 = Unit
         |			==.@args = [
         |				Unit,
         |				Unit
         |			]
         |			message = base58'emsY'
         |			sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |			pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			sigVerify.@args = [
         |				base58'emsY',
         |				base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |				base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			]
         |			ScriptTransfer.@args = [
         |				Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				),
         |				99900000000,
         |				Unit
         |			]
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
         |)))
         |)))
         |))""".stripMargin
    )
  }

  private def createTestCase(
      funcName: String,
      args: Seq[EXPR]
  )(expectedResult: (InvokeScriptTransaction, ByteStr, ByteStr) => String): Unit =
    withDomain(settings, balances) { d =>
      val invoke = TxHelpers.invoke(dApp1.toAddress, func = Some(funcName), args = args, invoker = invoker)
      d.appendBlock(
        TxHelpers.setScript(dApp1, dAppContract1(dApp2.toAddress)),
        TxHelpers.setScript(dApp2, dAppContract2(dApp3.toAddress)),
        TxHelpers.setScript(dApp3, dAppContract3)
      )
      d.transactionDiffer(invoke).trace.collectFirst { case invokeTrace: InvokeScriptTrace => invokeTrace.resultE.toString }.foreach { error =>
        val leaseId = Lease.calculateId(Lease(Recipient.Address(ByteStr(dApp2.toAddress.bytes)), 2, 0), invoke.id())
        val assetId = Issue.calculateId(0, "desc", isReissuable = true, "testAsset", 20, 0, invoke.id())

        error shouldBe expectedResult(invoke, leaseId, assetId)
      }
    }

  property(
    "correct error log for ScriptExecutionError"
  ) {
    createTestCase(
      "testCase",
      Seq(CONST_BOOLEAN(false))
    )((tx, leaseId, assetId) =>
      s"""Left(ScriptExecutionError(error = AccountBalanceError(Map(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), type = Account, log = 
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
         |	nested11 = 5
         |	==.@args = [
         |		5,
         |		5
         |	]
         |	Address.@args = [
         |		base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	]
         |	dapp2 = Address(
         |		bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |	)
         |	shouldFail = false
         |	cons.@args = [
         |		false,
         |		[]
         |	]
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
         |	inv = Left(CommonError(FailedTransactionError(code = 1, error = AccountBalanceError(Map(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log =
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
         |		cons.@args = [
         |			true,
         |			[]
         |		]
         |		AttachedPayment.@args = [
         |			Unit,
         |			10
         |		]
         |		cons.@args = [
         |			AttachedPayment(
         |				assetId = Unit
         |				amount = 10
         |			),
         |			[]
         |		]
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
         |		nested31 = Unit
         |		==.@args = [
         |			Unit,
         |			Unit
         |		]
         |		shouldFail = false
         |		cons.@args = [
         |			false,
         |			[]
         |		]
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
         |		inv = Left(CommonError(FailedTransactionError(code = 1, error = AccountBalanceError(Map(3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM -> negative waves balance: 3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM, old: 999000010, new: -98900999990)), log =
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
         |			nested32 = Unit
         |			==.@args = [
         |				Unit,
         |				Unit
         |			]
         |			message = base58'emsY'
         |			sig = base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg'
         |			pub = base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			sigVerify.@args = [
         |				base58'emsY',
         |				base58'4uXfw7162zaopAkTNa7eo6YK2mJsTiHGJL3dCtRRH63z1nrdoHBHyhbvrfZovkxf2jKsi2vPsaP2XykfZmUiwPeg',
         |				base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof'
         |			]
         |			ScriptTransfer.@args = [
         |				Address(
         |					bytes = base58'3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L'
         |				),
         |				99900000000,
         |				Unit
         |			]
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
         |)))
         |)))
         |))""".stripMargin
    )
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
