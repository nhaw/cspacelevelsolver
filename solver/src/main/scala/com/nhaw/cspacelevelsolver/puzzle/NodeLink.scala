package com.nhaw.cspacelevelsolver.puzzle

/**
  * Created by nhaw on 2/14/2016.
  */

/**
  * Describes a unidirectional link from one node to another
  */
case class NodeLink(src: Node, dest: Node, reqs: Seq[Requirement]) {
}
