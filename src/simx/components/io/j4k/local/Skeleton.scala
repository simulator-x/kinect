/*
 * Copyright 2014 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.io.j4k.local

import simplex3d.math.floatx.Vec2f
import scala.collection.immutable
import simx.core.helper.Loggable

/**
 * Masterarbeit
 *
 * Konzeption und prototypische Implementierung eines modularen interaktiven Assistenzsystems
 * zur computergetsützten Visualisierung von Feedback für Training und Rehabilitation
 *
 * Author: Anke Giebler-Schubert
 * Date: 14.04.2014
 */


private abstract class Tree
private case class MultiNode(value : JointType, set : Set[Tree]) extends Tree
private case class SingleNode(values : List[JointType] ) extends Tree

object SkeletonTree {

}

trait SkeletonTree {
  //in [m]
  protected val positions2D = Map[JointType, Vec2f](
    Skeleton.HEAD -> Vec2f(0, 1.65f),
    Skeleton.SHOULDER_CENTER -> Vec2f(0, 1.45f),
    Skeleton.SPINE -> Vec2f(0, 1f),
    Skeleton.HIP_CENTER -> Vec2f(0, .82f),

    Skeleton.LEFT_SHOULDER -> Vec2f(-0.22f, 1.4f),
    Skeleton.RIGHT_SHOULDER -> Vec2f(0.22f, 1.4f),
    Skeleton.LEFT_ELBOW -> Vec2f(-0.54f, 1.6f),
    Skeleton.RIGHT_ELBOW -> Vec2f(0.54f, 1.6f),
    Skeleton.LEFT_WRIST -> Vec2f(-0.78f, 1.9f),
    Skeleton.RIGHT_WRIST -> Vec2f(0.78f, 1.9f),

    Skeleton.LEFT_HAND -> Vec2f(-0.79f, 1.9f),
    Skeleton.RIGHT_HAND -> Vec2f(0.79f, 1.9f),

    Skeleton.HIP_LEFT -> Vec2f(-.2f, .87f),
    Skeleton.HIP_RIGHT -> Vec2f(.2f, .87f),

    Skeleton.KNEE_LEFT -> Vec2f(-.2f, .37f),
    Skeleton.KNEE_RIGHT -> Vec2f(.2f, .37f),

    Skeleton.ANKLE_LEFT -> Vec2f(-.2f, .07f),
    Skeleton.ANKLE_RIGHT -> Vec2f(.2f, .07f),
    Skeleton.FOOT_LEFT -> Vec2f(-.21f, .08f),
    Skeleton.FOOT_RIGHT -> Vec2f(.21f, .08f)
  )

  private val jointOrder: Tree =
    MultiNode(Skeleton.HIP_CENTER, Set[Tree](
    MultiNode(Skeleton.SPINE, Set[Tree](
      MultiNode(Skeleton.SHOULDER_CENTER, Set[Tree](
      SingleNode(List(Skeleton.HEAD)),
      SingleNode(List(Skeleton.RIGHT_SHOULDER, Skeleton.RIGHT_ELBOW, Skeleton.RIGHT_WRIST, Skeleton.RIGHT_HAND, Skeleton.RIGHT_FINGERTIP) ),
      SingleNode(List(Skeleton.LEFT_SHOULDER, Skeleton.LEFT_ELBOW, Skeleton.LEFT_WRIST, Skeleton.LEFT_HAND, Skeleton.LEFT_FINGERTIP) ),
      SingleNode(List(Skeleton.HEAD)) )))),
    SingleNode(List(Skeleton.HIP_LEFT,Skeleton.KNEE_LEFT, Skeleton.ANKLE_LEFT, Skeleton.FOOT_LEFT)),
    SingleNode(List(Skeleton.HIP_RIGHT,Skeleton.KNEE_RIGHT, Skeleton.ANKLE_RIGHT, Skeleton.FOOT_RIGHT))
  ))


  def getConnectionsToShow(toShow: Set[JointType]): List[(JointType, JointType)] ={
    getConnections(jointOrder, getValidJoints(toShow))
  }

  def getNeighbourJoints(j : JointType) : List[JointType] = {
    if(!getValidJoints(Set(j)).isEmpty) {
      neighbourJoints(jointOrder, j)
    }
    else List()
  }

  private def neighbourJoints(t : Tree, j : JointType , b: Option[JointType]= None, f : Boolean = false) : List[JointType] ={

    var list = List[JointType]()
    var found = false
    t match {
      case MultiNode(value, set) =>
//        var pointToConnect : Option[JointType] = None
        if(f) {
          list = value :: list
          return list
        }
        if(j.equals(value)) {
          found = true
//          pointToConnect = Some(value)
          if(b.isDefined) list = b.get :: list
        }
//        else if(p.isDefined) pointToConnect = p

        set.foreach(node =>
          list = neighbourJoints(node, j, Some(value), found) ::: list
        )
      case SingleNode(values) =>
        if(f) {
          list = values.head :: list
          return list
        }
        else {
          if(values.contains(j)) {
            val index = values.indexOf(j)
            if(index == 0 && values.size > 1) list = b.get :: values(index+1) :: list
            else if (index == values.size-1 && values.size > 1) list = values(index-1) :: list
            else list = values(index-1) :: values(index+1) :: list
          }
          return list
        }
    }
    list
  }

  private def getConnections(t : Tree, s: Set[JointType], p: Option[JointType]= None) : List[(JointType, JointType)] = {
    var list = List[(JointType, JointType)]()
    t match {
      case MultiNode(value, set) =>
        var pointToConnect : Option[JointType] = None
        if(s.contains(value)) {
          pointToConnect = Some(value)
          if(p.isDefined) list = (p.get, value) :: list
        }
        else if(p.isDefined) pointToConnect = p

        set.foreach(node =>
          list = getConnections(node, s - value, pointToConnect) ::: list
        )
      case SingleNode(values) =>
        var first : Option[JointType] = p
        values.foreach(value =>
          if(s.contains(value)){
            if(first.isDefined) list = (first.get, value) :: list
            first = Some(value)
          }
        )
    }
    list
  }

  protected def joints : Set[JointType]
  def bones : List[(JointType,JointType)]
  def initialPositions : Map[JointType, Vec2f]
  def getValidJoints(toTest : Set[JointType]) : Set[JointType]
  def getAllJoints : Set[JointType]
}

sealed trait JointType{def id : Symbol}
object Skeleton{
  case object HEAD extends JointType {var id = 'HEAD}
  case object SHOULDER_CENTER extends JointType {var id ='SHOULDER_CENTER}
  case object SPINE extends JointType {var id ='SPINE}
  case object HIP_CENTER extends JointType {var id ='HIP_CENTER}
  case object LEFT_COLLAR extends JointType {var id ='LEFT_COLLAR}
  case object LEFT_SHOULDER extends JointType {var id ='LEFT_SHOULDER}
  case object LEFT_ELBOW extends JointType {var id ='LEFT_ELBOW}
  case object LEFT_WRIST extends JointType {var id ='LEFT_WRIST}
  case object LEFT_HAND extends JointType {var id ='LEFT_HAND}
  case object LEFT_FINGERTIP extends JointType {var id ='LEFT_FINGERTIP}
  case object RIGHT_COLLAR extends JointType {var id ='RIGHT_COLLAR}
  case object RIGHT_SHOULDER extends JointType {var id ='RIGHT_SHOULDER}
  case object RIGHT_ELBOW extends JointType {var id ='RIGHT_ELBOW}
  case object RIGHT_WRIST extends JointType {var id ='RIGHT_WRIST}
  case object RIGHT_HAND extends JointType {var id ='RIGHT_HAND}
  case object RIGHT_FINGERTIP extends JointType {var id ='RIGHT_FINGERTIP}
  case object HIP_LEFT extends JointType {var id ='HIP_LEFT}
  case object KNEE_LEFT extends JointType {var id ='KNEE_LEFT}
  case object ANKLE_LEFT extends JointType {var id ='ANKLE_LEFT}
  case object FOOT_LEFT extends JointType {var id ='FOOT_LEFT}
  case object HIP_RIGHT extends JointType {var id ='HIP_RIGHT}
  case object KNEE_RIGHT extends JointType {var id ='KNEE_RIGHT}
  case object ANKLE_RIGHT extends JointType {var id ='ANKLE_RIGHT}
  case object FOOT_RIGHT extends JointType {var id ='FOOT_RIGHT}
  case object THUMB_RIGHT extends JointType {var id ='THUMB_RIGHT}
  case object THUMB_LEFT extends JointType {var id ='THUMB_LEFT}
  case object SPINE_CENTER extends JointType {var id ='SPINE_CENTER}
}

abstract class SkeletonBase[T] extends SkeletonTree with Loggable{

  //Mapping JointType -> NativeIDs
  protected val mapping : immutable.Map[JointType,T]

  def getNativeId(j : JointType) : T ={
    mapping(j)
  }
  def getJointType(n : T) : JointType ={

    mapping.find(_._2 == n).get._1
  }

  protected def joints : Set[JointType] = mapping.keySet
  def bones : List[(JointType,JointType)] = getConnectionsToShow(mapping.keySet)
  def initialPositions : Map[JointType, Vec2f] = mapping.map(elem => elem._1 -> positions2D(elem._1))

  def getValidJoints(toTest : Set[JointType]) = {
    toTest.foreach(j => if(!joints.contains(j)) printError(j))
    joints.intersect(toTest)
  }

  def getAllJoints : Set[JointType] = joints

  private def printError(j : JointType) {
    error("[SkeletonBase] Joint of type " + j + " does not exist in skeleton definition. It will be ignored." )
  }
}

/**
 * Works with FAAST 1.0
 */
object KinectFAASTSkeleton extends SkeletonBase[Symbol]  {

  protected val mapping : Map[JointType, Symbol] = immutable.Map(
    Skeleton.HEAD-> Symbol("0"),
    Skeleton.SHOULDER_CENTER-> Symbol("1"),
    Skeleton.SPINE-> Symbol("2"),
    Skeleton.HIP_CENTER-> Symbol("3"),
    Skeleton.LEFT_SHOULDER-> Symbol("5"),
    Skeleton.LEFT_ELBOW-> Symbol("6"),
    Skeleton.LEFT_WRIST-> Symbol("7"),
    //    testSkeleton.LEFT_HAND-> Symbol("7"),
    Skeleton.RIGHT_SHOULDER-> Symbol("11"),
    Skeleton.RIGHT_ELBOW-> Symbol("12"),
    Skeleton.RIGHT_WRIST-> Symbol("13"),
    //    testSkeleton.RIGHT_HAND-> Symbol("11"),
    Skeleton.HIP_LEFT-> Symbol("16"),
    Skeleton.KNEE_LEFT-> Symbol("17"),
    Skeleton.ANKLE_LEFT-> Symbol("18"),
    //    testSkeleton.FOOT_LEFT-> Symbol("15"),
    Skeleton.HIP_RIGHT-> Symbol("20"),
    Skeleton.KNEE_RIGHT-> Symbol("21"),
    Skeleton.ANKLE_RIGHT-> Symbol("22")//,
    //    testSkeleton.FOOT_RIGHT-> Symbol("19")
  )
}
/**
 * Works with FAAST 1.1 & 1.2
 */
object KinectFAASTSkeleton_1_1 extends SkeletonBase[Symbol]  {

  protected val mapping: Map[JointType, Symbol] = immutable.Map(
    Skeleton.HEAD-> Symbol("0"),
    Skeleton.SHOULDER_CENTER-> Symbol("1"),
    Skeleton.SPINE-> Symbol("2"),
    Skeleton.HIP_CENTER-> Symbol("3"),
    Skeleton.LEFT_SHOULDER-> Symbol("4"),
    Skeleton.LEFT_ELBOW-> Symbol("5"),
    Skeleton.LEFT_WRIST-> Symbol("6"),
    Skeleton.LEFT_HAND-> Symbol("7"),
    Skeleton.RIGHT_SHOULDER-> Symbol("8"),
    Skeleton.RIGHT_ELBOW-> Symbol("9"),
    Skeleton.RIGHT_WRIST-> Symbol("10"),
    Skeleton.RIGHT_HAND-> Symbol("11"),
    Skeleton.HIP_LEFT-> Symbol("12"),
    Skeleton.KNEE_LEFT-> Symbol("13"),
    Skeleton.ANKLE_LEFT-> Symbol("14"),
    Skeleton.FOOT_LEFT-> Symbol("15"),
    Skeleton.HIP_RIGHT-> Symbol("16"),
    Skeleton.KNEE_RIGHT-> Symbol("17"),
    Skeleton.ANKLE_RIGHT-> Symbol("18"),
    Skeleton.FOOT_RIGHT-> Symbol("19")
  )
}