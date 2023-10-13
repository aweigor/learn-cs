import { buildGenericTree } from './parser';
import { BinarySearchTree } from '../../structures/BinarySearchTree';
import { tokenize } from './lexer';
import { LogicalOperand, LogicalOperator } from '../../../types';

export class LogicalProcessor {
  expTree: BinarySearchTree<LogicalOperand | LogicalOperator>;
  symbols: Map<string, LogicalOperand>;
  get exp() {
    return this._exp;
  }
  constructor(private readonly _exp: string) {
    const { symbols, tokens } = tokenize(this.exp);
    this.symbols = symbols;
    this.expTree = buildGenericTree(tokens);
  }
}
