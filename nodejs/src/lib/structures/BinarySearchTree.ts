class Node<TreeDataType> {
  data: TreeDataType;
  left: Node<TreeDataType> | null;
  right: Node<TreeDataType> | null;
  constructor(data: TreeDataType) {
    this.data = data;
    this.left = null;
    this.right = null;
  }
}

export class BinarySearchTree<TreeDataType> {
  root: Node<TreeDataType> | null = null;
  constructor() {}
  insert(data: TreeDataType) {
    const newNode = new Node<TreeDataType>(data);
    if (this.root === null) this.root = newNode;
    else this.insertNode(this.root, newNode);
  }
  insertNode(node: Node<TreeDataType>, newNode: Node<TreeDataType>) {
    if (newNode.data < node.data) {
      if (node.left === null) node.left = newNode;
      else this.insertNode(node.left, newNode);
    } else {
      if (node.right === null) node.right = newNode;
      else this.insertNode(node.right, newNode);
    }
  }
}
