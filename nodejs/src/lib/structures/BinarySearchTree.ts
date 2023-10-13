import {
  LogicalOperand,
  LogicalUnaryOperator,
  LogicalBinaryOperator,
} from '../../types';

interface IBinaryNode {
  data: any;
  left: IBinaryNode | null;
  right: IBinaryNode | null;
}

interface BinaryTree {
  root: IBinaryNode | null;
}

class BinaryNode<TreeDataType> implements IBinaryNode {
  data: TreeDataType;
  left: BinaryNode<TreeDataType> | null;
  right: BinaryNode<TreeDataType> | null;
  constructor(data: TreeDataType) {
    this.data = data;
    this.left = null;
    this.right = null;
  }
}

interface IGenericNode<DT> {
  data: DT;
  firstChild: IGenericNode<any> | null;
  nextSibling: IGenericNode<DT> | null;
  allowChildren: boolean;
  allowSiblings: boolean;
}

class LogicalUnaryOperatorNode implements IGenericNode<LogicalUnaryOperator> {
  data: LogicalUnaryOperator;
  firstChild: IGenericNode<LogicalBinaryOperator | LogicalOperand> | null;
  nextSibling: null = null;
  allowChildren: boolean = true;
  allowSiblings: boolean = true;
  constructor(data: LogicalUnaryOperator) {
    this.data = data;
    this.firstChild = null;
  }
}

class LogicalBinaryOperatorNode implements IGenericNode<LogicalBinaryOperator> {
  data: LogicalBinaryOperator;
  firstChild: IGenericNode<LogicalUnaryOperator | LogicalOperand> | null;
  nextSibling: IGenericNode<LogicalBinaryOperator> | null = null;
  allowChildren: boolean = true;
  allowSiblings: boolean = true;
  constructor(data: LogicalBinaryOperator) {
    this.data = data;
    this.firstChild = null;
  }
}

class LogicalOperandNode implements IGenericNode<LogicalOperand> {
  data: LogicalOperand;
  firstChild: null = null;
  nextSibling: null = null;
  allowChildren: boolean = false;
  allowSiblings: boolean = true;
  constructor(data: LogicalOperand) {
    this.data = data;
  }
}

export class LogicalExpressionTree {
  root:
    | LogicalBinaryOperatorNode
    | LogicalUnaryOperatorNode
    | LogicalOperandNode
    | null = null;

  insertNextSibling(
    node: IGenericNode<
      LogicalBinaryOperator | LogicalUnaryOperator | LogicalOperand
    >,
    next: IGenericNode<
      LogicalBinaryOperator | LogicalUnaryOperator | LogicalOperand
    >,
  ): IGenericNode<
    LogicalBinaryOperator | LogicalUnaryOperator | LogicalOperand
  > {
    if (!node.nextSibling) return (node.nextSibling = next);
    return this.insertNextSibling(node.nextSibling, next);
  }

  insertNextChild(
    node: IGenericNode<LogicalBinaryOperator | LogicalUnaryOperator>,
    childNode: IGenericNode<
      LogicalBinaryOperator | LogicalUnaryOperator | LogicalOperand
    >,
  ): IGenericNode<
    LogicalBinaryOperator | LogicalUnaryOperator | LogicalOperand
  > | null {
    if (!node.firstChild) return (node.firstChild = childNode);
    if (node instanceof LogicalUnaryOperatorNode) return null;
    return this.insertNextSibling(node.firstChild, childNode);
  }
}
