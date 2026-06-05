// Port of testHelpers/GeneratorContractsForBuiltInFunctions.scala
import { V3 } from "./jsTestBase";

export class ContractGenerator {
  constructor(
    public readonly dataType: string,
    private readonly libVersion: number
  ) {}

  codeFromMatchingAndCase(testData: string, func: string, testDataForV3: string, testDataForGreaterV3: string): string {
    const caseForVersion = this.caseForVersions(testDataForV3, testDataForGreaterV3);
    return `

@Callable(i)
        func expression() = {
            let callerTestData = ${testData}
            let valueOrUnit = ${func}
            let throwMessage = "not ${this.dataType}"
            let val = match(valueOrUnit) {
              case b:${this.dataType} => b
              case _ => throwMessage.throw()
            }
            ${caseForVersion}
        }
`;
  }

  codeOwnData(ownDataFunction: string, testDataForV3: string, testDataForGreaterV3: string): string {
    const caseForVersion = this.caseForVersions(testDataForV3, testDataForGreaterV3);
    return `

@Callable(i)
        func expression() = {
            let valueOrUnit = ${ownDataFunction}
            let val = match(valueOrUnit) {
              case b:${this.dataType} => b
              case _ => throw("not ${this.dataType}")
            }
            ${caseForVersion}
        }
`;
  }

  codeWithoutMatcher(testData: string, func: string, testDataForV3: string, testDataForGreaterV3: string): string {
    const caseForVersion = this.caseForVersions(testDataForV3, testDataForGreaterV3);
    return `

@Callable(i)
        func expression() = {
            let callerTestData = ${testData}
            let val = ${func}
            ${caseForVersion}
        }
`;
  }

  codeOwnDataWithoutMatcher(ownDataFunction: string, caseForVersions: string): string {
    return `

 @Callable(i)
        func expression() = {
            let val = ${ownDataFunction}
            ${caseForVersions}
        }
`;
  }

  onlyMatcherContract(testData: string, func: string): string {
    return `

 let callerTestData = ${testData}
        let x = match ${func} {
            case h:${this.dataType} => h
            case _ => throw("not ${this.dataType}")
        }
`;
  }

  simpleRideCode(foo: string, bar: string, testFunction: string): string {
    return `

let foo = ${foo}
let bar = ${bar}
let callerTestData = ${testFunction}
`;
  }

  codeForDAppInvocation(byteVector: string, payment: string, func: string): string {
    return `

func foo(dapp2: String, a: Int, key1: String, key2: String) = {
        let byteVector = ${byteVector}
        let payment = ${payment}
        strict res = ${func}
        match res {
            case r : Int =>
            (
                [
                    IntegerEntry(key1, r),
                    IntegerEntry(key2, wavesBalance(addressFromStringValue(dapp2)).regular)
                ],
                unit
            )
                case _ => throw("Incorrect invoke result")
            }
        }

        @Callable(i)
        func bar(a: Int) = {
        (
            [
                ScriptTransfer(i.caller, 100000000, unit)
            ],
                a * 2
            )
        }
`;
  }

  codeForAddressFromRecipient(addressOrAlias: string, func: string, address: string): string {
    return `
let addressOrAlias = ${addressOrAlias};
        match (tx) {
            case t: TransferTransaction => ${func} == ${address}
            case _ => false
        }
`;
  }

  codeForCalculateAssetId(testData: string, func: string): string {
    return `
@Callable(inv)
    func issueAndId() = {
      let issue = ${testData}
      let id = ${func}
        ([issue])}
`;
  }

  codeForCalculateLeaseId(testData: string, func: string): string {
    return `
@Callable(i)
  func foo() = {
      let lease = Lease(${testData}, 100000000)
      let id = ${func}
      ([
              lease,
              BinaryEntry("lease", id)
      ], unit)
  }
`;
  }

  private caseForVersions(testDataForV3: string, testDataForGreaterV3: string): string {
    return this.libVersion > V3 ? testDataForGreaterV3 : testDataForV3;
  }
}
