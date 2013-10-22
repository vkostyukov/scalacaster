package graph 

/**
 * Inductive graph implementation based on Martin Erwig's paper: 
 * http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
 * 
 * Usage example
 *
 * val sample = 
 *     Context(Adj(List(("left", Node(2)), ("up", Node(3)))), 
 *             Node(1), "a", Adj(List(("right", Node(2))))) &:
 *     Context(Adj(List()), Node(2), "b", Adj(List(("down", Node(3))))) &:
 *     Context(Adj(List()), Node(3), "c", Adj(List())) &: 
 *     Empty
 * 
 * val updatedSample = sample.add("d")
 * 
 * val connectedSample = sample.connect(Node(3), Node(4), "new edge")
 */

case class Node(id: Int)
case class Adj[+B](links: List[(B, Node)])
case class Context[+A, +B](in: Adj[B], node: Node, content: A, out: Adj[B])

sealed abstract class Graph[+A, +B] {
  def isEmpty: Boolean

  def &:[C >: A, D >: B](c: Context[C, D]): Graph[C, D] = new &:(c, this)
  
  def :::[C >: A, D >: B](g: Graph[C, D]): Graph[C, D] = g match {
    case Empty => this
    case head &: rest => head &: rest ::: this
  }

  def size: Int = this match {
    case _ &: rest => 1 + rest.size
    case Empty => 0
  }

  def matchNode(n: Node): Option[Context[A, B]] = this match {
    case (ctx @ Context(_, node, _, _)) &: _ if n == node => Some(ctx)
    case _ &: rest => rest.matchNode(n)
    case Empty => None
  }

  def map[C >: A, D >: B](f: Context[C, D] => Context[C, D]): Graph[C, D] = {
    def loop(gg: Graph[C, D], acc: Graph[C, D]): Graph[C, D] = gg match {
      case ctx &: rest => loop(rest, f(ctx) &: acc)
      case Empty => acc
    }
    loop(this, Empty)
  }

  def ufold[T](init: T, f: (Context[A, B], T) => T): T = {
    def loop(gg: Graph[A, B], acc: T): T = gg match {
      case ctx &: rest => loop(rest, f(ctx, acc))
      case Empty => acc
    }
    loop(this, init)
  }

  def nodes[C >: A, D >: B]: List[Node] =
    ufold(List(), (c: Context[C, D], acc: List[Node]) => c.node :: acc)

  def successors(node: Node): List[Node] = {
    def succin(c: Context[A, B], acc: List[Node]): List[Node] =
      c.in.links.foldLeft(List[Node]())((a, i) => 
        if (i._2 == node) c.node :: a 
        else a
      ) ++ acc
    
    val outNodes = this.matchNode(node) match {
      case Some(ctx) => ctx.out.links.map(_._2)
      case None => List[Node]()
    }

    this.ufold(List[Node](), succin) ++ outNodes
  }

  def add[C >: A, D >: B](c: C): Graph[C,D] = 
    Context(in = Adj[D](List()), out = Adj[D](List()),
            node = Node(size+1), content = c ) &: this

  /*
   * Only works if at least one of the nodes exists. Will create edge to non-existing
   * node in case one of the two nodes does not exist. 
   */
  def connect[C >: A, D >: B](from: Node, to: Node, edgeLabel: D): Graph[C, D] = {
    def connector(graph: Graph[C, D], acc: Graph[C, D]): Graph[C, D] = graph match {
      case Context(in, n, con, out) &: rest if n == from => 
        // create outgoing link at from
        val newContext = Context(in, n, con, Adj((edgeLabel, to) :: out.links))
        newContext &: acc ::: rest
      case Context(in, n, con, out) &: rest if n == to => 
        // create incoming link at to
        val newContext = Context(Adj((edgeLabel, from) :: in.links), n, con, out)
        newContext &: acc ::: rest
      case head &: rest => connector(rest, head &: acc)
      case Empty => acc
    }
    connector(this, Empty)
  }
}

case class &:[A, B](c: Context[A, B], g: Graph[A, B]) extends Graph[A, B] {
  override def isEmpty: Boolean = false
  override def toString: String = "Context(" + c.in + ", " + c.node + 
    ", " + c.content + ", " + c.out + ") & \n" + g.toString
}

case object Empty extends Graph[Nothing, Nothing] {
  override def isEmpty: Boolean = true
}

object Graph {
  def empty[A, B]: Graph[A, B] = Empty

  def apply[A, B](n: A, t: Graph[A, B] = Empty): Graph[A, B] =
    new &:(Context(Adj[B](Nil), Node(1), n, Adj[B](Nil)), t)
}
