import {
  TLogicalExpressionTokens,
  LogicalOperand,
  LogicalOperatorTypes,
  LogicalOperator,
} from '../../../types';
import { Operator, Operand } from './lexer';
import {
  LogicalExpressionTree,
  LogicalBinaryOperatorNode,
  LogicalUnaryOperatorNode,
  LogicalOperandNode,
} from '../../structures/BinarySearchTree';

export function buildGenericTree(
  expressionTokens: TLogicalExpressionTokens,
): LogicalExpressionTree {
  const tree = new LogicalExpressionTree();
  tree.root = buildTerm(expressionTokens);

  function buildTerm(
    expressionTokens: TLogicalExpressionTokens,
  ):
    | LogicalBinaryOperatorNode
    | LogicalUnaryOperatorNode
    | LogicalOperandNode
    | null {
    let term:
      | LogicalBinaryOperatorNode
      | LogicalUnaryOperatorNode
      | LogicalOperandNode
      | null = null;

    const children: Array<
      LogicalBinaryOperatorNode | LogicalUnaryOperatorNode | LogicalOperandNode
    > = [];
    for (const token of expressionTokens) {
      if (token instanceof Operator) {
        const operator: LogicalOperator = token;
        if (operator.type === LogicalOperatorTypes.UNARY) {
          children.push();
        }
      } else if (token instanceof Operand) {
        if (term === null) {
          term = new LogicalOperandNode(token);
        }
      }
    }
    return term;
  }

  return tree;
}
