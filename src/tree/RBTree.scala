/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Red-Black Tree http://en.wikipedia.org/wiki/Red-black_tree
 *
 * Insert - O(log n)
 * Lookup - O(log n)  
 * Remove - O(log n)
 *
 * -Notes-
 *
 * This is an implementation of Okasaki's red-black tree introduced in his fantastic book
 * "Purely Functional Data Structures and Algorithms". Here is the short link to article:
 * 
 *            www.ccs.neu.edu/course/cs3500wc/jfp99redblack.pdf
 *
 * As well, as Okasaki's tree this implementation doesn't contain a 'remove()' method, 
 * since it totally madness to understand and write such code. There is one CS hero, 
 * who handled that task - Matt Might. He described a new approach of removing node 
 * from red-black tree by using additional node colors (double black, negate black)
 * and new phase - 'bubbling'. Anyway, these things are a bit more then 'just-for-fun' 
 * package. Here is the link to Matt's research:
 *
 *              http://matt.might.net/articles/red-black-delete/
 *
 * Chris Okasaki said in his blog that removing is awkward operation for functional
 * data structures due to persistence. What does 'remove' mean in a functional world?
 * It means that the client wants to get a new collection without one element. In functional 
 * settings we can store both states of collection - without element 'x' (before insertion) 
 * and with 'x' (after insertion). So, when the removing is needed for 'x' the previous
 * state of collection can be used instead.
 *
 * Anyway, red-black trees is a great data structure with fantastic running time 
 * algorithms, but it's implementation is so complicated that you might use simple
 * BST instead. This is why, Robert Sedgewick invented left-leaning red-black trees as
 * simple (in terms of implementation) replacement for red-black trees. Here is his 
 * awesome trees description:
 * 
 *               http://www.cs.princeton.edu/~rs/talks/LLRB/LLRB.pdf
 *
 * There are also two ways to improve current implementation. First of them - modify
 * 'balance()' method to expect violations only along the search path. Second - use 
 * 'two-way-comparison' that is invented by Anre Andersonin method 'contains()'.
 * Here is the explanation of this idea:
 *
 *                  http://user.it.uu.se/~arnea/ps/searchproc.pdf
 *
 * This tree implementation doesn't inherit loads a useful methods from binary search
 * tree (see 'src/tree/Tree.scala') since is not necessary to have these methods in 
 * both trees (it also brakes DRY principle). The main goal of this project - is to
 * provide a clear functional approaches of implementation functional data structures,
 * not to provide a completely stable collections and algorithms framework.
 *
 *
 * PS: I would be happy, if someone decided to add Matt's 'remove()' method into this
 *     implementation.
 */

/** 
 * A color for RB-Tree's nodes. 
 */
abstract sealed class Color
case object Red extends Color
case object Black extends Color

/**
 * A Red-Black Tree.
 */
abstract sealed class Tree[+A <% Ordered[A]] {

  /**
   * The color of this tree.
   */
  def color: Color

  /**
   * The value of this tree.
   */
  def value: A

  /**
   * The left child of this tree.
   */
  def left: Tree[A]

  /**
   * The right child of this tree.
   */
  def right: Tree[A]

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Adds given element 'x' into this tree.
   *
   * Exercise 3.10a @ PFDS.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def add[B >: A <% Ordered[B]](x: B): Tree[B] = {
    def balancedAdd(t: Tree[A]): Tree[B] =
      if (t.isEmpty) Tree.make(Red, x)
      else if (x < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right)
      else if (x > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def balanceLeft(c: Color, x: A, l: Tree[B], r: Tree[A]) = (c, l, r) match {
      case (Black, Branch(Red, y, Branch(Red, z, a, b), c), d) => 
        Tree.make(Red, y, Tree.make(Black, z, a, b), Tree.make(Black, x, c, d))
      case (Black, Branch(Red, z, a, Branch(Red, y, b, c)), d) => 
        Tree.make(Red, y, Tree.make(Black, z, a, b), Tree.make(Black, x, c, d))
      case _ => Tree.make(c, x, l, r)
    }

    def balanceRight(c: Color, x: A, l: Tree[A], r: Tree[B]) = (c, l, r) match {
      case (Black, a, Branch(Red, y, b, Branch(Red, z, c, d))) =>
        Tree.make(Red, y, Tree.make(Black, x, a, b), Tree.make(Black, z, c, d))
      case (Black, a, Branch(Red, z, Branch(Red, y, b, c), d)) => 
        Tree.make(Red, y, Tree.make(Black, x, a, b), Tree.make(Black, z, c, d))
      case _ => Tree.make(c, x, l, r)
    }

    def blacken(t: Tree[B]) = Tree.make(Black, t.value, t.left, t.right)

    blacken(balancedAdd(this))
  }

  def height: Int =
    if (isEmpty) 0
    else math.max(left.height, right.height) + 1

  /**
   * Fails with given message.
   */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case class Branch[A <% Ordered[A]](color: Color, 
                              value: A, 
                              left: Tree[A], 
                              right: Tree[A]) extends Tree[A] {
  def isEmpty = false
}

case object Leaf extends Tree[Nothing] {
  def color: Color = Black
  def value: Nothing = fail("An empty tree.")
  def left: Tree[Nothing] = fail("An empty tree.")
  def right: Tree[Nothing] = fail("An empty tree.")
  def isEmpty = true
}

object Tree {

  /**
   * Returns an empty red-black tree instance.
   *
   * Time - O(1)
   * Space - O(1)
   */
   def empty[A]: Tree[A] = Leaf

   /**
    *
    */
   def make[A <% Ordered[A]](c: Color, x: A, l: Tree[A] = Leaf, r: Tree[A] = Leaf): Tree[A] =
     Branch(c, x, l, r)

  /**
   * Creates a new red-black tree from given 'xs' sequence.
   *
   * Time - O(n log n)
   * Space - O(log n)
   */
  def apply[A <% Ordered[A]](xs: A*): Tree[A] = {
    var r: Tree[A] = Leaf
    for (x <- xs) r = r.add(x)
    r
  }
}
