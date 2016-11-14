module GraphLib

type Node<'T> = Node of 'T 

type NodeList<'T> = NodeList of Node<'T> list

type Edge<'T> = Edge of  Node<'T> * Node<'T> 

type EdgeList<'T> = EdgeList of Edge<'T> list

module Graph =

    let createNode data = Node data

    let createEdge nodeA nodeB = Edge (nodeA, nodeB)

        

