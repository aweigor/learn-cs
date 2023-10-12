enum OperatorTypes {
  BINARY,
  UNARY,
  LPAR,
  RPAR,
  COMMA,
}

enum Operations {
  AND,
  OR,
  NOT,
  IMPL,
  XOR,
  EQU,
}

class Operand {
  index: number;
  name: string;
  value?: boolean;
  constructor(index: number, name: string, value?: boolean) {
    this.index = index;
    this.name = name;
    this.value = value;
  }
}

class Operator {
  get operation() {
    return this._operation;
  }
  get type() {
    return this._type;
  }
  constructor(
    private readonly _type: OperatorTypes,
    private readonly _operation?: Operations,
  ) {}
}

export function tokenize(expr: string) {
  const errors = [];
  const result = [];
  for (const char of expr) {
  }
  return {
    result,
    errors,
  };
}
