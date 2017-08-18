class DoubleLinkedListOfInts {
  /**
    * Node implementation
    * @param x is the value of created node
    */
  private class Node(x: Int) {
    var itemValue: Int = x
    var next: Option[Node] = None
    var prev: Option[Node] = None
  }

  private var head: Option[Node] = None
  private var tail: Option[Node] = None
  private var size: Int = 0
  private var isReversed = false

  /**
    * checks if the linked list is empty
    * @return boolean
    */
  private def isEmpty :Boolean = size == 0

  /**
    * adds a new node to tail of the LinkedList
    *
    * @param x is the value
    */
  def append(x: Int): Unit = {
    val node = Some(new Node(x))
    if (isEmpty) addFirstElement(node)
    else {
      tail.get.next = node
      node.get.prev = tail
      tail = node
    }
    size += 1
    }

  /**
    * adds a new node to tail of the LinkedList
    *
    * @param x is the value
    */
  def prepend(x: Int): Unit = {
    val node = Some(new Node(x))
    if (isEmpty) {
      addFirstElement(node)
    } else {
      node.get.next = head
      head.get.prev = node
      head = node
    }
    size += 1
  }

  /**
    * Inserts a new node into given index
    *
    * @param x is the value
    * @param i is the index
    */
  def insert(x: Int, i: Int): Unit = {
    val validation: Boolean = i < size || i > 0
    assert(validation, "Index out of bound : " + i + "should be between 0 and " + size)
    if (validation) {
      var tempNode = head
      if (i == 1)
        prepend(x)
      else if (i == size + 1)
        append(x)
      else {
        val node = Some(new Node(x))
        var index = 1
        while (index != i) {
          tempNode = tempNode.get.next
          index += 1
        }
        insertInto(tempNode,node)
        size += 1
      }
    }
  }

  def insertInto(after: Option[Node],current: Option[Node]): Unit ={
    current.get.prev = after.get.prev
    current.get.next = after
    after.get.prev.get.next =current
    after.get.prev = current
  }

  /**
    * Deletes a node from the LinkedList
    *
    * @param x is the value of the deleted node
    */
  def delete(x: Int): Unit = {
    if (isEmpty) throw new Exception("Delete : Empty Stack!")
    else {
      deletion(head,x)
    }
  }

  /**
    *
    */
  def reverse(): Unit = {
    isReversed = !isReversed
    val temp: Option[Node] = head
    head = tail
    tail = temp

  }

  /**
    * prints all items in the linkedList
    */
  def print(): Unit = printNode(head)

  private def printNode(node: Option[Node]): Unit ={
    if(node.isDefined){
      Console.print(node.get.itemValue+",")
      if(isReversed) printNode(node.get.prev)
      else printNode(node.get.next)
    }
  }

  private def addFirstElement(x : Option[Node]) : Unit = {
    head = x
    tail = x
  }

  private def deletion(current: Option[Node], x: Int): Unit = {
     current match {
       case Some(node) => {
         if(node.itemValue.equals(x)) {
           if(node.prev.isDefined) node.prev.get.next = node.next
           else head = head.get.next /** deletion from head */
           if(node.next.isDefined) node.next.get.prev = node.prev
           else tail = tail.get.prev /** deletion from tail */
         }else {
           deletion(node.next,x)
         }
       }
       case None => throw new Exception("Deletion : Element not found!")
     }
  }

}