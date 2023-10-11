export class Stack<DT> {
  data: DT[] = [];
  conscructor() {}
  push(item: DT): void {
    this.data.push(item);
  }
  pop(): DT | undefined {
    return this.data.pop();
  }
}
