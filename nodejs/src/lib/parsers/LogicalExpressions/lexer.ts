import {
  LogicalOperand,
  LogicalOperator,
  LogicalOperatorTypes,
  LogicalOperations,
  TLogicalExpressionTokens,
} from '../../../types';

const OPER = ['&', '|', '~', '>', '+', '*', ',', '(', ')'];

export class Operand implements LogicalOperand {
  index: number;
  name: string;
  value?: boolean;
  constructor(index: number, name: string, value?: boolean) {
    this.index = index;
    this.name = name;
    this.value = value;
  }
}

export class Operator implements LogicalOperator {
  get operation() {
    return this._operation;
  }
  get type() {
    return this._type;
  }
  constructor(
    private readonly _type: LogicalOperatorTypes,
    private readonly _operation?: LogicalOperations,
  ) {}
}

export function tokenize(expr: string): {
  symbols: Map<string, LogicalOperand>;
  tokens: TLogicalExpressionTokens;
} {
  const symbols: Map<string, LogicalOperand> = new Map<
    string,
    LogicalOperand
  >();
  const tokens: TLogicalExpressionTokens = [];
  for (const char of expr) {
    if (!OPER.includes(char)) {
      if (!symbols.has(char)) {
        symbols.set(char, new Operand(symbols.size, char));
      }
      tokens.push(symbols.get(char) as LogicalOperand);
    } else {
      switch (char) {
        case '&': {
          tokens.push(
            new Operator(LogicalOperatorTypes.BINARY, LogicalOperations.AND),
          );
          break;
        }
        case '|': {
          tokens.push(
            new Operator(LogicalOperatorTypes.BINARY, LogicalOperations.OR),
          );
          break;
        }
        case '~': {
          tokens.push(
            new Operator(LogicalOperatorTypes.UNARY, LogicalOperations.NOT),
          );
          break;
        }
        case '>': {
          tokens.push(
            new Operator(LogicalOperatorTypes.BINARY, LogicalOperations.IMPL),
          );
          break;
        }
        case '+': {
          tokens.push(
            new Operator(LogicalOperatorTypes.BINARY, LogicalOperations.XOR),
          );
          break;
        }
        case '*': {
          tokens.push(
            new Operator(LogicalOperatorTypes.BINARY, LogicalOperations.EQU),
          );
          break;
        }
        case ',': {
          tokens.push(new Operator(LogicalOperatorTypes.COMMA));
          break;
        }
        case '(': {
          tokens.push(new Operator(LogicalOperatorTypes.LPAR));
          break;
        }
        case ')': {
          tokens.push(new Operator(LogicalOperatorTypes.RPAR));
          break;
        }
        default:
          break;
      }
    }
  }
  return {
    tokens,
    symbols,
  };
}
