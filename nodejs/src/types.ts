export enum LogicalOperatorTypes {
  BINARY,
  UNARY,
  LPAR,
  RPAR,
  COMMA,
}

export enum LogicalOperations {
  AND = '&',
  OR = '|',
  NOT = '~',
  IMPL = '>',
  XOR = '+',
  EQU = '*',
}

export interface LogicalOperand {
  index: number;
  name: string;
  value?: boolean;
}

export interface LogicalOperator {
  type: LogicalOperatorTypes;
  operation?: LogicalOperations;
}

export type TLogicalExpressionTokens = Array<LogicalOperand | LogicalOperator>;
