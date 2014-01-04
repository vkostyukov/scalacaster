/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Queue http://en.wikipedia.org/wiki/Queue_(abstract_data_type)
 *
 * -Notes-
 *
 * This queue also known as Banker's Queue. There is also Physics Queue. Interested in the topic -
 * read the Okasaki`s book.
 *
 * Enqueue - O(1)
 * Dequeue - O(1)
 * Front - O(1)
 * Rear - O(1)
 */

class Queue[+A](in: List[A] = Nil, out: List[A] = Nil) {

  /**
   * Check whether this queue is empty or not.
   */
  def isEmpty: Boolean = (in, out) match {
    case (Nil, Nil) => true
    case (_, _) => false
  }

  /**
   * Dequeues the first element from this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
   def dequeue: (A, Queue[A]) = out match {
    case hd :: tl => (hd, new Queue(in, tl))
    case Nil => in.reverse match {
      case hd :: tl => (hd, new Queue(Nil, tl))
      case Nil => throw new NoSuchElementException("Empty queue.")
    }
  }

  /**
   * Enqueues given element 'x' into the end of this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def enqueue[B >: A](x: B): Queue[B] = new Queue(x :: in, out)

  /**
   * Returns the first element of this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def front: A = dequeue match { case (a, _) => a }

  /**
   * Returns the rear of this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def rear: Queue[A] = dequeue match { case (_, q) => q }
}

object Queue {

  /**
   * Creates a new empty queue.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def empty[A]: Queue[A] = new Queue()

  /**
   * Creates a new queue fromm given 'xs' sequence.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def apply[A](xs: A*): Queue[A] = {
    var r: Queue[A] = new Queue()
    for (x <- xs) {
      r = r.enqueue(x)
    }
    r
  }
}
