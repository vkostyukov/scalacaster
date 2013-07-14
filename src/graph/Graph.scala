/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * -Notes-
 * 
 * Usage example:
 * var g: Graph[Int, String] = Graph(
 *        ("A", 20, "B"), ("B", 30, "C"), 
 *        ("C", 40, "D"), ("D", 50, "A"), 
 *        ("A", 100, "C"), ("D", 200, "B"))
 * 
 * Creates the following graph:
 * 
 *  |<-------------------|
 *  D ---> A ---> B ---> C 
 *  |------|----->|      |
 *         |------------>|
 *
 */

case class Edge[E, N](source: Graph[E, N], target: Graph[E, N], value: E)

class Graph[E, N](var value: N = null.asInstanceOf[N]) {

  import scala.collection.immutable.Queue

  var inEdges: List[Edge[E, N]] = Nil
  var outEdges: List[Edge[E, N]] = Nil

  def succs: List[Graph[E, N]] = outEdges.map(_.target)
  def preds: List[Graph[E, N]] = inEdges.map(_.source)

  /**
   * Connects this graph with given 'g' graph via 'e' edge.
   *
   * Time - O()
   * Space - O()
   */
  def connect(from: N, via: E, to: N): (Graph[E, N], Graph[E, N]) = {
    val fromGraph: Graph[E, N] = if (value == null) {value = from; this} else hop(from) match {
      case Some(g) => g
      case None => Graph.one(from)
    }

    val toGraph: Graph[E, N] = hop(to) match {
      case Some(g) => g
      case None => Graph.one(to)
    }

    fromGraph.outEdges = new Edge(fromGraph, toGraph, via) :: fromGraph.outEdges
    toGraph.inEdges = new Edge(fromGraph, toGraph, via) :: toGraph.inEdges

    (fromGraph, toGraph)
  }

  def hop(n: N): Option[Graph[E, N]] = graphsByDepth.find(_.value == n)

  def nodesByDepth: List[N] = graphsByDepth.map(_.value)
  def nodesByBreadth: List[N] = graphsByBreadth.map(_.value)

  def graphsByDepth: List[Graph[E, N]] = {
    def loop(g: Graph[E, N], s: Set[Graph[E, N]]): Set[Graph[E, N]] = 
      if (!s(g)) g.succs.foldLeft(s + g)((acc, gg) => loop(gg, acc))
      else s

    loop(this, Set()).toList
  }

  def graphsByBreadth: List[Graph[E, N]] = {
    def loop(q: Queue[Graph[E, N]], s: Set[Graph[E, N]]): Set[Graph[E, N]] = 
      if (!q.isEmpty && !s(q.head)) 
        loop(q.head.succs.foldLeft(q.tail)((acc, gg) => acc :+ gg), s + q.head)
      else s

    loop(Queue(this), Set()).toList
  }

  override def equals(o: Any): Boolean = o match {
    case g: Graph[_, _] => g.value == value
    case _ => false
  }

  override def toString: String = "Graph(" + value + ")"
}

object Graph {
  def apply[E, N](tuples: (N, E, N)*): Graph[E, N] = {
    var g: Graph[E, N] = Graph.empty
    for ((from, via, to) <- tuples) {
      println(from + "->" + to)
      g.connect(from, via, to)
    }
    g
  }

  def one[E, N](n: N): Graph[E, N] = new Graph(n)

  def empty[E, N]: Graph[E, N] = new Graph
}

// var g: Graph[Int, String] = Graph(
//       ("A", 20, "B"), ("B", 30, "C"), 
//       ("C", 40, "D"), ("D", 50, "A"), 
//       ("A", 100, "C"), ("D", 200, "B"))

// println(g.nodesByDepth)
// println(g.nodesByBreadth)
