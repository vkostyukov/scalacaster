/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Binary Search Tree http://en.wikipedia.org/wiki/Binary_search_tree
 *
 * Insert - O(log n)
 * Lookup - O(log n)  
 * Remove - O(log n)
 *
 */

abstract class Tree[+A <% Ordered[A]] {
  def value: A
  def left: Tree[A]
  def right: Tree[A]

  def isEmpty: Boolean

  def isValid: Boolean =
    if (isEmpty) true
    else if (left.isEmpty && right.isEmpty) true
    else if (left.isEmpty) right.value >= value && right.isValid
    else if (right.isEmpty) left.value <= value && left.isValid
    else left.value <= value && right.value >= value && left.isValid && right.isValid

  def isBalanced: Boolean = 
    math.abs(left.height - right.height) <= 1

  def add[B >: A <% Ordered[B]](x: B): Tree[B]  =
    if (isEmpty) new Node(x, Leaf, Leaf)
    else if(x <= value) new Node(value, left.add(x), right)
    else new Node(value, left, right.add(x))

  def remove[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) new Node(value, left.remove(x), right)
    else if (x > value) new Node(value, left, right.remove(x))
    else {
      if (left.isEmpty && right.isEmpty) Leaf
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val succ = successor(x)
        new Node(succ, left, right.remove(succ))
      }
    }

  def contains[B >: A <% Ordered[B]](x: B): Boolean =
    if (isEmpty) false
    else if (x < value) left.contains(x)
    else if (x > value) right.contains(x)
    else true

  def subtree[B >: A <% Ordered[B]](x: B): Tree[B] =
    if (isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")
    else if (x < value) left.subtree(x)
    else if (x > value) right.subtree(x)
    else this

  def isSubtree[B >: A <% Ordered[B]](t: Tree[B]): Boolean = ???

  def walk(f: (A) => Unit): Unit = 
    if (!isEmpty) {
      left.walk(f)
      f(value)
      right.walk(f)
    }

  // def fold[B >: A](n: B)(f: (B, B) => B): B = 
  //   if (isEmpty) n
  //   else f(f(value, left.fold(n)), right.fold(n))

  // def sum(implicit num: math.Numeric[A]): A = 
  //   if (isEmpty) num.zero
  //   else num.plus(num.plus(left.sum, value), right.sum)

  def size: Int =
    if (isEmpty) 0
    else 1 + left.size + right.size

  def min: A = 
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (left.isEmpty) value
    else left.min

  def max: A = 
    if (isEmpty) throw new NoSuchElementException("Tree is empty.")
    else if (right.isEmpty) value
    else right.max

  def height: Int =
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)

  def successor[B >: A <% Ordered[B]](x: B): A = {
    var path: List[Tree[A]] = Nil
    var these = this
    while (!these.isEmpty && these.value != x) {
      path = these :: path
      if (x < these.value) these = these.left
      else if (x > these.value) these = these.right
    }

    if (these.isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")

    if (!these.right.isEmpty) these.right.min
    else {
      while (!path.isEmpty && these == path.head.right) {
        these = path.head
        path = path.tail
      }

      if (!path.isEmpty) path.head.value
      else throw new NoSuchElementException("The " + x + " doesn't have an accessor.")
    }
  }

  def predecessor[B >: A <% Ordered[B]](x: B): A = {
    var path: List[Tree[A]] = Nil
    var these = this
    while (!these.isEmpty && these.value != x) {
      path = these :: path
      if (x < these.value) these = these.left
      else if (x > these.value) these = these.right
    }

    if (these.isEmpty) throw new NoSuchElementException("Can't find " + x + " in this tree.")

    if (!these.left.isEmpty) these.left.max
    else {
      while (!path.isEmpty && these == path.head.left) {
        these = path.head
        path = path.tail
      }

      if (!path.isEmpty) path.head.value
      else throw new NoSuchElementException("The " + x + " doesn't have an accessor.")
    }
  }

  // def nthMax(n: Int): A = ???
  // def nthMin(n: Int): A = ???

  // def takeLargest(n: Int): List[A] = ???
  // def takeSmallest(n: Int): List[A] = ???

  // def apply(n: Int): A = nthMin(n)
}

object Leaf extends Tree[Nothing] {
  def value: Nothing = throw new NoSuchElementException("Leaf.value")
  def left: Tree[Nothing] = throw new NoSuchElementException("Leaf.left")
  def right: Tree[Nothing] = throw new NoSuchElementException("Leaf.right")

  def isEmpty: Boolean = true
}

class Node[A <% Ordered[A]](v: A, l: Tree[A], r: Tree[A]) extends Tree[A] {
  def value: A = v
  def left: Tree[A] = l
  def right: Tree[A] = r

  def isEmpty: Boolean = false
}

object Tree {
  def apply[A <% Ordered[A]](xs: A*): Tree[A] = {
    var r: Tree[A] = Leaf
    for (x <- xs) r = r.add(x)
    r
  }

  def fromSortedArray[A <% Ordered[A]](a: Array[A]): Tree[A] = ???
}

var t = Tree(20, 10, 30)
println(t.height)
println(t.isBalanced)
println(t.isValid)