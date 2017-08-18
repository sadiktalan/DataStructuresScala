/**
  * Initial size of the new Integer Array
  *
  * @param initSize beginning size
  */
class ArrayOfInts(initSize: Int) {
  private var array: Array[Int] = Array.ofDim(initSize)
  private var index: Int = 0
  private var size = initSize

  /**
    * Takes an integer
    *
    * @param x and puts it to given index of the array
    *          increases index
    *          if index = size then make a new array of double size and copies all elements
    */
  def append(x: Int): Unit = {
    adjustArray()
    array(index) = x
    index += 1
  }

  /**
    * Takes an integer
    *
    * @param x and puts it to first index
    *          shifts all elements' index +1
    *          increases index
    */
  def prepend(x: Int): Unit = {
    adjustArray()
    for (a <- index to 1 by -1) array(a) = array(a - 1)
    array(0) = x
    index += 1
  }

  /** Takes an integer
    *
    * @param x and find its index in the array
    *          shifts all elements from last to founded index
    *          decreases index
    */
  def delete(x: Int): Unit = {
    if (index > 0) {
      val itemIndex = array.indexOf(x)
      for (a <- itemIndex to index) array(a) = array(a + 1)
      index -= 1
    }
    throw new Exception("Array is empty!")
  }

  /**
    * Insertion sort
    */
  def sort(): Unit = {
    for (a <- 1 until index) {
      var b = a - 1
      val key = array(a)
      while (b >= 0 && array(b) > key) {
        array(b + 1) = array(b)
        b = b - 1
      }
      array(b + 1) = key
    }
  }

  /**
    * prints all elements
    */
  def printAll(): Unit = {
    for (i <- 0 until index) println(array(i))
  }

  /** creates a new array that x2 size
    * copies all elements to new array
    */
  private def adjustArray(): Unit = { // private
    if (index == size) {
      val newArray: Array[Int] = Array.ofDim(size * 2) // System.Arraycopy
      for (a <- 0 to size) newArray(a) = array(a)
      array = newArray
      size *= 2
    }
  }
}
