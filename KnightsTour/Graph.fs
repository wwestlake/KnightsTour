﻿(*
    Knights Tour in F#, an algorithm to solve the knights tour problem
    Copyright (C) 2016  William W. Westlake
    wwestlake@lagdaemon.com

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Source code available at: https://github.com/wwestlake/KnightsTour
*)

module GraphLib

type Node<'T> = Node of 'T 

type NodeList<'T> = NodeList of Node<'T> list

type Edge<'T> = Edge of  Node<'T> * Node<'T> 

type EdgeList<'T> = EdgeList of Edge<'T> list

module Graph =

    let createNode data = Node data

    let createEdge nodeA nodeB = Edge (nodeA, nodeB)

        

