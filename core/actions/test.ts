import { verifyObjectMatchesProto, VerifyProtoErrorBehaviour } from "df/common/protos";
import { StringifiedMap } from "df/common/strings/stringifier";
import { ActionBuilder, ITableContext } from "df/core/actions";
import * as table from "df/core/actions/table";
import { View } from "df/core/actions/view";
import { Contextable, ICommonContext, INamedConfig, Resolvable } from "df/core/common";
import { Session } from "df/core/session";
import { targetStringifier } from "df/core/targets";
import {
  ambiguousActionNameMsg,
  checkExcessProperties,
  resolvableAsTarget,
  strictKeysOf,
  stringifyResolvable,
  toResolvable
} from "df/core/utils";
import { dataform } from "df/protos/ts";

/**
 * Configuration options for unit tests.
 */
export interface ITestConfig extends INamedConfig {
  /**
   * The dataset that this unit test tests.
   */
  dataset?: Resolvable;
}

const ITestConfigProperties = strictKeysOf<ITestConfig>()(["type", "dataset", "name"]);

/**
 * @hidden
 */
export class Test extends ActionBuilder<dataform.Test> {
  // TODO(ekrekr): make this field private, to enforce proto update logic to happen in this class.
  public proto: dataform.ITest = dataform.Test.create();

  public session: Session;
  public contextableInputs = new StringifiedMap<
    dataform.ITarget,
    Contextable<ICommonContext, string>
  >(targetStringifier);

  private datasetToTest: Resolvable;
  private contextableQuery: Contextable<ICommonContext, string>;

  constructor(session?: Session) {
    super(session);
  }

  public config(config: ITestConfig) {
    checkExcessProperties(
      (e: Error) => this.session.compileError(e),
      config,
      ITestConfigProperties,
      "test config"
    );
    if (config.dataset) {
      this.dataset(config.dataset);
    }
    return this;
  }

  public dataset(ref: Resolvable) {
    this.datasetToTest = ref;
    return this;
  }

  public input(refName: string | string[], contextableQuery: Contextable<ICommonContext, string>) {
    this.contextableInputs.set(resolvableAsTarget(toResolvable(refName)), contextableQuery);
    return this;
  }

  public expect(contextableQuery: Contextable<ICommonContext, string>) {
    this.contextableQuery = contextableQuery;
    return this;
  }

  /**
   * @hidden
   */
  public getFileName() {
    return this.proto.fileName;
  }

  /**
   * @hidden
   */
  public getTarget(): undefined {
    // The test action type has no target because it is not processed during regular execution.
    return undefined;
  }

  public compile() {
    const testContext = new TestContext(this);
    if (!this.datasetToTest) {
      this.session.compileError(
        new Error("Tests must operate upon a specified dataset."),
        this.proto.fileName
      );
    } else {
      const allResolved = this.session.indexedActions.find(resolvableAsTarget(this.datasetToTest));
      if (allResolved.length > 1) {
        this.session.compileError(
          new Error(ambiguousActionNameMsg(this.datasetToTest, allResolved)),
          this.proto.fileName
        );
      }
      const dataset = allResolved.length > 0 ? allResolved[0] : undefined;
      if (!(dataset && (dataset instanceof table.Table || dataset instanceof View))) {
        this.session.compileError(
          new Error(`Dataset ${stringifyResolvable(this.datasetToTest)} could not be found.`),
          this.proto.fileName
        );
      } else if (dataset.proto.enumType === dataform.TableType.INCREMENTAL) {
        this.session.compileError(
          new Error("Running tests on incremental datasets is not yet supported."),
          this.proto.fileName
        );
      } else {
        const refReplacingContext = new RefReplacingContext(testContext);
        this.proto.testQuery = refReplacingContext.apply(dataset.contextableQuery);
      }
    }
    this.proto.expectedOutputQuery = testContext.apply(this.contextableQuery);

    return verifyObjectMatchesProto(
      dataform.Test,
      this.proto,
      VerifyProtoErrorBehaviour.SUGGEST_REPORTING_TO_DATAFORM_TEAM
    );
  }
}

/**
 * @hidden
 */
export class TestContext {
  public readonly test: Test;
  constructor(test: Test) {
    this.test = test;
  }

  public apply<T>(value: Contextable<ICommonContext, T>): T {
    if (typeof value === "function") {
      return (value as any)(this);
    } else {
      return value;
    }
  }
}

/**
 * @hidden
 */
class RefReplacingContext implements ITableContext {
  private readonly testContext: TestContext;

  constructor(testContext: TestContext) {
    this.testContext = testContext;
  }

  public ref(ref: Resolvable | string[], ...rest: string[]) {
    return this.resolve(ref, ...rest);
  }

  public resolve(ref: Resolvable | string[], ...rest: string[]) {
    const target = resolvableAsTarget(toResolvable(ref, rest));
    if (!this.testContext.test.contextableInputs.has(target)) {
      this.testContext.test.session.compileError(
        new Error(
          `Input for dataset "${JSON.stringify(
            target
          )}" has not been provided. Provided inputs: ${Array.from(
            this.testContext.test.contextableInputs.keys()
          ).map(providedTarget => JSON.stringify(providedTarget))}`
        )
      );
      return "";
    }
    return `(${this.testContext.apply(this.testContext.test.contextableInputs.get(target))})`;
  }

  public apply<T>(value: Contextable<ITableContext, T>): T {
    if (typeof value === "function") {
      return (value as any)(this);
    } else {
      return value;
    }
  }

  public config(config: any) {
    return "";
  }

  public self() {
    return "";
  }

  public name() {
    return "";
  }

  public type(type: table.TableType) {
    return "";
  }

  public schema() {
    return "";
  }

  public database() {
    return "";
  }

  public where(where: Contextable<ITableContext, string>) {
    return "";
  }

  public when(cond: boolean, trueCase: string, falseCase: string = "") {
    return cond ? trueCase : falseCase;
  }

  public incremental() {
    return false;
  }

  public preOps(statement: Contextable<ITableContext, string | string[]>) {
    return "";
  }

  public postOps(statement: Contextable<ITableContext, string | string[]>) {
    return "";
  }

  public disabled() {
    return "";
  }

  public dependencies(name: string) {
    return "";
  }

  public tags(tags: string[]) {
    return "";
  }
}
