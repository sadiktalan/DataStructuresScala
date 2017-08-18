class StackOfInts {
  private var size: Int = 0
  private var topNode: Option[Node] = None

  /**
    * Creates a new node and pushes it to stack
    *
    * @param x is value
    */
  def push(x: Int): Unit = {
    val node = new Node(x)
    if(!isEmpty)
      node.next = topNode
    topNode = Some(node)
    size += 1
  }

  /**
    * Pops the element from stack
    *
    * @return value of the node that popped
    */
  def pop(): Int = {
    val value = topNode.get.itemValue
    topNode = topNode.get.next
    size -= 1
    value
  }

  /**
    * @return (without popping) value of node that top of stack
    */
  def peek(): Int = {
    topNode match {
      case None => throw new Exception("Empty Stack!")
      case Some(topNode) => topNode.itemValue
    }
  }

  /**
    * calls printValue(Option[Node]) method
    *@see printValue()
    */
  def print(): Unit = printValue(topNode)

  /**
    * takes a Node parameter and prints its value 'till end of the stack
    * @param node
    */
  private def printValue(node : Option[Node]): Unit = {
  node match {
      case None =>println()
      case Some(node) =>{
        Console.print(node.itemValue + ", ")
        printValue(node.next)
      }
    }
  }

  /**
    * @return stack empty or not
    */
  private def isEmpty = size == 0

  /**
    * Node class for each element
    * @param x is the value
    */
  private class Node(x: Int) {
    val itemValue: Int = x
    var next: Option[Node] = None

  }
}