export interface ICompilationResult {
    result: {
        ast: object
        base64: string
        bytes: Uint8Array
        size: number
        complexity: number
        verifierComplexity?: number
        callableComplexity?: Record<string, number>
        userFunctionsComplexity?: Record<string, number>
    }
}

export interface ICompilationError {
    error: string
}

export interface IDecompilationResult {
    result: string
}

export interface IDecompilationError {
    error: any
}

export type TType = TList | TStruct | TUnion | TPrimitive

export type TPrimitive = string;

export type TStructField = { name: string, type: TType };

export type TStruct = {
    typeName: string
    fields: TStructField[]
};

export type TList = {
    'listOf': TType
};

export type TUnionItem = TStruct | TPrimitive | TList
export type TUnion = TUnionItem[]

export type TFunction = {
    name: string
    doc: string
    resultType: TType
    args: TFunctionArgument[]
};

export type TFunctionArgument = {
    name: string
    type: TType
    doc: string
};

export interface IVarDoc {
    name: string
    type: TType
    doc: string
}

export interface IScriptInfo {
    stdLibVersion: number,
    contentType: number,
    scriptType: number
    imports: string[]
}

export interface IFlattenedCompilationResult {
    ast?: object
    base64?: string
    bytes?: Uint8Array
    size?: number
    complexity?: number
    verifierComplexity?: number
    callableComplexities?: Record<string, number>
    userFunctionComplexities?: Record<string, number>
    error?: string
}

export function compile(
    code: string,
    estimatorVersion?: number,
    needCompaction?: boolean,
    removeUnusedCode?: boolean,
    libraries?: Record<string, string>
): ICompilationResult | ICompilationError;

export function flattenCompilationResult(compiled: ICompilationResult | ICompilationError): IFlattenedCompilationResult

export function parseAndCompile(
    code: string,
    estimatorVersion?: number,
    needCompaction?: boolean,
    removeUnusedCode?: boolean,
    libs?: Record<string, string>
): IParseAndCompileResult | ICompilationError;

export function scriptInfo(code: string): IScriptInfo | ICompilationError;

export function getTypes(stdlibVersion?: number, isTokenContext?: boolean, isContract?: boolean): TStructField[];

export function getVarsDoc(stdlibVersion?: number, isTokenContext?: boolean, isContract?: boolean): IVarDoc[];

export function getFunctionsDoc(stdlibVersion?: number, isTokenContext?: boolean, isContract?: boolean): TFunction[];

export function decompile(compiledCode: string): IDecompilationResult | IDecompilationError;

export interface IReplOptions {
    nodeUrl: string
    chainId: string
    address: string
}

export function repl(opts?: IReplOptions): {
    reconfigure: (opts: IReplOptions) => ReturnType<typeof repl>
    evaluate: (expr: string) => Promise<IDecompilationResult | IDecompilationError>,
    clear: () => void,
    test: (str: string) => Promise<string>,
    info: (s: string) => string,
    totalInfo: () => string,
};

export const version: string;
export const contractLimits: {
    MaxComplexityByVersion: (v: number) => number,
    MaxExprSizeInBytes: number,
    MaxContractSizeInBytes: number,
    MaxContractInvocationArgs: number,
    MaxContractInvocationSizeInBytes: number,
    MaxWriteSetSizeInBytes: number,
    MaxPaymentAmount: number
};

export interface IPos {
    posStart: number,
    posEnd: number
}

export interface IName extends IPos {
    value: string,
}

export interface IContext extends IPos {
    name: string
}


export interface IConstByteStr extends IExprNode {
    type: 'CONST_BYTESTR'
}

export interface IConstLong extends IExprNode {
    type: 'CONST_LONG'
}

export interface IConstStr extends IExprNode {
    type: 'CONST_STRING'
}

export interface ITrue extends IExprNode {
    type: 'TRUE'
}

export interface IFalse extends IExprNode {
    type: 'FALSE'
}

export interface IRef extends IExprNode {
    type: 'REF'
    name: string
}

export interface IBlock extends IExprNode {
    type: 'BLOCK'
    dec: TDecl
    body: TExpr
}


export interface IIf extends IExprNode {
    type: 'IF'
    cond: TExpr
    ifTrue: TExpr
    ifFalse: TExpr
}


export interface IGetter extends IExprNode {
    type: 'GETTER'
    ref: TExpr
    field: IName
    name: string
}

export interface IMatch extends IExprNode {
    type: 'MATCH'
    expr: TExpr
    cases: IMatchCase[]
}

//------------------------
export interface IMatchCase extends INode {
    type: 'MATCH_CASE'
    expr: TExpr
}

export interface ILet extends INode {
    type: 'LET'
    name: IName
    expr: TExpr
    dec?: TDecl
}

export interface IScript extends Exclude<INode, 'resultType'> {
    type: 'SCRIPT'
    expr: TExpr
}

export interface IDApp extends Exclude<INode, 'resultType'> {
    type: 'DAPP'
    decList: (ILet | IFunc)[]
    annFuncList: IAnnotatedFunc[]
}

export interface IAnnotatedFunc extends Exclude<INode, 'resultType'> {
    type: 'ANNOTATEDFUNC',
    annList: IAnnotation[],
    func: IFunc
}

export interface IAnnotation extends Exclude<INode, 'resultType'> {
    type: 'ANNOTATION',
    name: IName,
    argList: IName[]
}

export interface IFunc extends INode {
    type: 'FUNC'
    name: IName
    expr: TExpr
    argList: TArgument[]
    body: IBlock | IFunctionCall
}

export type TArgument = { argName: IName, type: TArgumentType }
export type TArgumentType = { typeName: IName, typeParam?: ITypeParam }

export interface ITypeParam extends IPos {
    value: { isUnion: boolean, typeList: TArgumentType[] }
}

export interface IFunctionCall extends IExprNode {
    type: 'FUNCTION_CALL'
    name: IName
    args: TExpr[]
}

export interface INode extends IPos {
    type: TNodeType
    resultType: string
    ctx: IContext[]
}

export type TExprResultType = { type: string } | { unionTypes: TExprResultType[] } | { listOf: TExprResultType }

export interface IExprNode extends Omit<INode, 'resultType'> {
    resultType: TExprResultType
}

export interface IError extends IPos {
    msg: string
}

export interface IParseAndCompileResult {
    result: ArrayBuffer
    complexity: number
    errorList: IError[]
    exprAst?: IScript
    dAppAst?: IDApp
}

export type TPrimitiveNode = IConstStr | IConstLong | IConstByteStr | ITrue | IFalse

export type TNode =
    | IBlock
    | IConstByteStr
    | IIf
    | IFunctionCall
    | IConstLong
    | IRef
    | IConstStr
    | ITrue
    | IFalse
    | IGetter
    | IMatch
    | ILet
    | IMatchCase
    | IFunc
    | IScript
    | IDApp
    | IAnnotatedFunc
    | IAnnotation
export type TNodeType =
    | 'BLOCK'
    | 'LET'
    | 'CONST_BYTESTR'
    | 'IF'
    | 'FUNCTION_CALL'
    | 'CONST_LONG'
    | 'REF'
    | 'CONST_STRING'
    | 'TRUE'
    | 'FALSE'
    | 'GETTER'
    | 'MATCH'
    | 'MATCH_CASE'
    | 'FUNC'
    | 'SCRIPT'
    | 'DAPP'
    | 'ANNOTATEDFUNC'
    | 'ANNOTATION'
export type TDecl = ILet | IFunc
export type TExpr =
    | IBlock
    | IConstByteStr
    | IIf
    | IFunctionCall
    | IConstLong
    | IConstStr
    | ITrue
    | IFalse
    | IGetter
    | IMatch
    | IRef
