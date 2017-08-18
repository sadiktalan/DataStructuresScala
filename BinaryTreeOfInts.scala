class BinaryTreeOfInts {

  class Node(x: Int) {
    var itemValue: Int = x
    var leftChild: Option[Node] = None
    var rightChild: Option[Node] = None
    var parent: Option[Node] = None
  }

  private var root: Option[Node] = None

  /**
    * Enum for in-order type
    */
  private object orderType extends Enumeration{
    val Ascending,Descending = Value
  }

  /**
    * add a new node to the tree(Iterative way)
    * @param x value of the adding node
    */
  def add(x: Int): Unit = {
    val node = Some(new Node(x))
    if (root.isEmpty) root = node
    else insert(root, node)
  }

  /** önce bulması gerek
    * eğer cocuk yoksa direkt sil
    * eğer sağda çocugu varsa sağdaki childinin en soluna git silinecek node yerine bağla eski yerini sil
    * @param x silinecek data
    */
  def delete(x: Int): Unit = {
    val temp: Option[Node] = findNode(root, x)
    temp match {
      case Some(temp) => {
        /** if it is leaf node */
        if (temp.leftChild.isEmpty && temp.rightChild.isEmpty) {
          if (temp.parent.get.rightChild == Option(temp)) temp.parent.get.rightChild = None
          else temp.parent.get.leftChild = None
          /** else if there is 1 child; removes node and replace child node*/
        }else if (temp.leftChild.isEmpty || temp.rightChild.isEmpty) {
          if (temp.leftChild.isDefined) {
            if (temp.parent.get.leftChild ==Option(temp)) /** if removing node is left child and removing node has left child*/
              temp.parent.get.leftChild = temp.leftChild
            else temp.parent.get.rightChild = temp.leftChild /** if removing node is left child and removing node has right child*/
          } else if (temp.rightChild.isDefined) {
            if (temp.parent.get.leftChild.isDefined) /** if removing node is right child and removing node has left child*/
              temp.parent.get.leftChild = temp.leftChild
            else temp.parent.get.rightChild = temp.rightChild /** if removing node is right child and removing node has right child*/
          }
          /** if there are 2 children node then removes the node and replace it with right child's most left grandchild*/
        }else {
          var newNode: Option[Node] = temp.rightChild
          while (newNode.get.leftChild.isDefined) { /** right child's most left grandchild*/
            newNode = newNode.get.leftChild
          }
          if (temp.parent.get.leftChild == Option(temp)) temp.parent.get.leftChild = newNode
          else temp.parent.get.rightChild = newNode
          newNode.get.parent.get.leftChild = None
          newNode.get.parent = temp.parent
          temp.rightChild.get.parent = newNode
          temp.leftChild.get.parent = newNode
          newNode.get.rightChild = temp.rightChild
          newNode.get.leftChild = temp.leftChild
        }
      }
      case _ => throw new NoSuchElementException("delete : Element couldn't found!")
    }
  }

  /**
    * searches the given value in the tree that is exist or not
    * @param x value
    * @return {boolean} exist/not exist
    */
  def search(x: Int): Boolean = {
    var isFound = false
    if (root.isDefined && findNode(root, x).isDefined) isFound = true
    isFound
  }

  /**
    * prints all values in ascending order
    */
  def printAsc() = printTree(root, orderType.Ascending)

  /**
    * prints all values in descending order
    */
  def printDesc() = printTree(root, orderType.Descending)

  private def printTree(node: Option[Node], order: orderType.Value): Unit = {
    if (node.isDefined) {
      printTree(if (order == orderType.Descending) node.get.rightChild else node.get.leftChild, order)
      print(node.get.itemValue + ", ")
      printTree(if(order == orderType.Descending) node.get.leftChild else node.get.rightChild, order)
    }
  }

  /**
    * recursive insertion of the tree
    * @param recent is the last checked node
    * @param node   is the new node
    */
  private def insert(recent: Option[Node], node: Option[Node]): Unit = {
    node match {
      case Some(node) =>{
        if (recent.get.itemValue > node.itemValue) {
          if (recent.get.leftChild.isEmpty) {
            recent.get.leftChild = Some(node)
            node.parent = recent
          } else insert(recent.get.leftChild, Some(node))
        } else if (recent.get.itemValue < node.itemValue) {
          if (recent.get.rightChild.isEmpty) {
            recent.get.rightChild = Some(node)
            node.parent = recent
          } else insert(recent.get.rightChild, Some(node))
        }
      }
    }
  }

  private def findNode(recent: Option[Node], x: Int): Option[Node]= {
    if (recent.isEmpty) None
    else if (recent.get.itemValue == x) recent
    else if (recent.get.itemValue < x) findNode(recent.get.rightChild, x)
    else findNode(recent.get.leftChild, x)
  }

}
