import scala.collection.mutable
/**
  * 2D Position
  * @param x position
  * @param y position
  */
class PositionPoint(val x: Double, val y: Double){
  override def toString: String = "--> x = " + x.toString + ", y = " + y.toString
}

/**
  * Node class
  * @param identifier node id
  * @param position position node
  */
class Node(identifier: Int, position: PositionPoint) {
  require(identifier > 0)
  val id: Int = identifier
  val pos: PositionPoint = position
  val neighbors = new mutable.MutableList[Node]
}


/**
  * Graph class
  */
class Graph {
  val nodes = new mutable.HashMap[Int, Node]()

  /**
    * Method by add new node to hash
    * @param newNode new node to add
    */
  def addNode(newNode: Node): Unit = {
    if(!nodes.contains(newNode.id))
      nodes(newNode.id) = newNode
    //else
    //  throw new Exception("Node exist")
  }

  /**
    * Connection between nodes
    * @param first first node
    * @param second second node
    */
  def addEdge(first: Node, second:Node): Unit = {
    if(!nodes(first.id).neighbors.contains(second))
      nodes(first.id).neighbors += second
    //else
    //  throw new Exception("Edge exist")
  }

  /**
    * Description object
    * @return
    */
  override def toString: String = {
    println("Graph:")
    nodes.foreach(elem => {
      print(" Node: " + elem._1.toString + ", pos: " + elem._2.pos + " neighbors(id): ")
      elem._2.neighbors.foreach(y => print(y.id.toString + " "))
      println()
    })
    ""
  }
}

/**
  * Fusion between two nodes
  * @param g1 first graph
  * @param g2 second graph
  * @param delta minimum distance(Int)
  */
class FusionGraph(val g1: Graph,val  g2: Graph, delta: Int){

  require(g1 != null && g2 != null && delta > 0)

  /**
    * Create a new graph after fusion
    * @return
    */
  def create(): Graph = {
    val graph = new Graph
    println("Distance(delta "+ delta.toString + "):")
    g1.nodes.foreach(x => {
      graph.addNode(x._2)
      g2.nodes.foreach( y => {
        graph.addNode(y._2)
        val dist: Double = distance(x._2.pos, y._2.pos).abs
        if(dist < delta){
          graph.addEdge(x._2, y._2)
          println("Graph 1 - Node " + x._2.id.toString + " " + x._2.pos)
          println("Graph 2 - Node " + y._2.id.toString + " " + y._2.pos)
          println(dist.toString)
        }
      })
    })
    graph
  }

  /**
    * Distance between position Nodes
    * @param p1 first position into Node
    * @param p2 second position into Node
    * @return
    */
  def distance(p1: PositionPoint, p2:PositionPoint): Double
    = Math.sqrt(Math.pow(p2.x - p1.x, 2) + Math.pow(p2.y - p1.y, 2))
}

/**
  * Singleton object
  */
object Principal {
  /**
    * Start method
    * @param args list argument
    */
  def main(args: Array[String]): Unit = {

    val graph = new Graph
    val n1 = new Node(1, new PositionPoint(1, 2))
    val n2 = new Node(2, new PositionPoint(8, 10))
    val n3 = new Node(3, new PositionPoint(5, 20))

    graph.addNode(n1)
    graph.addNode(n2)
    graph.addNode(n3)

    graph.addEdge(n1, n2)
    graph.addEdge(n2, n3)
    graph.addEdge(n3, n1)
    println(graph)

    //////////////////////////////

    val graph2 = new Graph
    val nn1 = new Node(4, new PositionPoint(2, 2.5))
    val nn2 = new Node(5, new PositionPoint(1, 10))
    graph2.addNode(nn1)
    graph2.addNode(nn2)

    graph2.addEdge(nn1, nn2)
    println(graph2)

    //////////////////////////////

    val graphFusion: Graph = new FusionGraph(graph, graph2, 5).create()
    println(graphFusion)
  }
}