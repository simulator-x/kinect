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

package simx.components.io.j4k

import edu.ufl.digitalworlds.j4k.{DepthMap, J4KSDK, Skeleton, VideoFrame}
import simplex3d.math.floatx.ConstVec3f
import simx.components.io.j4k.local.{JointType, SkeletonBase}
import simx.core.svaractor.SVarActor

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

/**
 * Masterarbeit
 *
 * Konzeption und prototypische Implementierung eines modularen interaktiven Assistenzsystems
 * zur computergetsützten Visualisierung von Feedback für Training und Rehabilitation
 *
 * Author: Anke Giebler-Schubert
 * Date: 14.04.2014
 */

class KinectConnector(component : SVarActor.Ref) extends J4KSDK {

  var depthMap : DepthMap = null
  var skeletons : Array[Skeleton] = null
  var skipValue = 1
  var videoTexture=new VideoFrame()
  var maskPlayer = Array(false, false, false, false, false, false)
  var showSkeleton = Array(false, false, false, false, false, false)
  var playerIdToJ4kIdMap = Map[Int,Int]()




  def activateMask(id : Int) {
    try {
      maskPlayer(id) = true
      playerIdToJ4kIdMap +=(id -> 0)
      computeUV(true)
    }
    catch {
      case e : ArrayIndexOutOfBoundsException => throw J4KConnectionException("No more than 6 persons can be masked!", e.getCause)
    }
  }

  def deactivateMask(id : Int) {
    maskPlayer(id) = false
    //playerIdToJ4kIdMap -= id
  }

  def activateSkeleton(id : Int) {
    try {
      if(showSkeleton.filter(x => x).length == 0) {
//        startSkeletonTracking(false)
      }
      showSkeleton(id) = true
      playerIdToJ4kIdMap +=(id -> 0)
    }
    catch {
      case e: ArrayIndexOutOfBoundsException => throw J4KConnectionException("No more than 6 persons can be tracked!", e.getCause)
    }

  }

  def deactivateSkeleton(id : Int) {
    showSkeleton(id) = false
    playerIdToJ4kIdMap -= id
    if(showSkeleton.filter(x => x).length == 0){} //stopSkeletonTracking
  }

  def updateDefaultSkeleton(id: Int) {
    val jointPos = Skeleton.defaultStance().getJointPositions
    for(i<-0 until jointPos.length by 3) {
      component ! UpdateJoint(id, KinectJ4KSkeleton.getJointType(i/3).id, ConstVec3f(jointPos(i), jointPos(i+1), jointPos(i+2)))
    }
  }

  //TODO: has to be updated to new version
  /**
   * This abstract method is called when a new depth frame is received by the sensor.
   * Optionally, the corresponding U,V mapping of the video frame to this depth frame
   * could also be provided, if U,V computation is enabled (see next method).
   **/
  @Override
  def onDepthFrameEvent(depth: Array[Short], player_index: Array[Byte], XYZ: Array[Float], UV: Array[Float]) = {
    // println("Update Depth Frame")
//    depthMap = new DepthMap(getDepthWidth, getDepthHeight, XYZ)
//    depthMap.setPlayerIndex(depth, player_index)
//    depthMap.setMaximumAllowedDeltaZ(0.1f)
//    if(UV != null) depthMap.setUV(UV)
//
//    var j4kIds = Set[Int]()
//    depthMap.player.toSet[Byte].filterNot(_==0).foreach(j4kId => {
//      j4kIds += (j4kId.toInt -1)
//    })
//    recalculatePlayerMapping(j4kIds)
//
//    for(id : Int<- 0 until maskPlayer.length) {
//      if(maskPlayer(id) && playerIdToJ4kIdMap(id)!= -1 ) {
//        depthMap.maskPlayer(playerIdToJ4kIdMap(id))
//        val data = calculateMeshData
//        if(!data._1.isEmpty)
//          component ! UpdatePlayerMask(id, data._1, data._2, data._3, data._4)
//      }
//    }
//
//    component ! UpdateDepthMap(getDepthWidth, getDepthHeight, depth)
  }


  /**
   * This abstract method is called when a new skeleton frame is received by the
   * sensor. The skeleton data contain the coordinates of the joints of up to 6
   * skeletons, and the flags indicate which of the 6 skeletons contain valid data
   * in this frame.
   **/
  @Override
  def onSkeletonFrameEvent(skeleton_flags: Array[Boolean], positions: Array[Float], orientations: Array[Float], jointstatus: Array[Byte]) = {
    var j4kIds = Set[Int]()
    getSkeletons.foreach{s =>
      if(s.isTracked) j4kIds += s.getPlayerID
    }
    recalculatePlayerMapping(j4kIds)

    for(id : Int<- 0 until showSkeleton.length) {
      if(showSkeleton(id) && playerIdToJ4kIdMap(id)!= -1 ) {
        val jointPos = Skeleton.getSkeleton(playerIdToJ4kIdMap(id), skeleton_flags, positions, orientations, jointstatus, this).getJointPositions

        for(i<-0 until jointPos.length by 3) {
          component !UpdateJoint(id, KinectJ4KSkeleton.getJointType(i/3).id, ConstVec3f(jointPos(i), jointPos(i+1), jointPos(i+2)))
        }
      }
    }
  }

  @Override
  def onColorFrameEvent(color_frame: Array[Byte]): Unit ={

    //TODO add timestamp to msg to use it for svar set
    component ! UpdateVideoFrame(color_frame, getColorWidth, getColorHeight, System.currentTimeMillis())
  }

  private def recalculatePlayerMapping(j4kIds : Set[Int]) {
    //no player recognized
    if(j4kIds.size == 0){
      playerIdToJ4kIdMap.keySet.foreach( key => {playerIdToJ4kIdMap += key-> -1})
    }
    else {
      var ids = j4kIds.toSet
      playerIdToJ4kIdMap.foreach( entry => {
        if(ids.contains(entry._2)) ids = ids-entry._2
        else playerIdToJ4kIdMap += (entry._1 -> -1)
      })
      ids.foreach(id => {
        playerIdToJ4kIdMap.find(_._2 == -1).collect{
          case entry => playerIdToJ4kIdMap += entry._1 -> id
        }
      })
    }
  }

  private def calculateMeshData : (Array[Int], Array[Float], Array[Float], Array[Float]) ={
    val indices = ArrayBuffer[Int]()
    val vertexPos = ArrayBuffer[Float]()
    val normals = ArrayBuffer[Float]()
    val texCoords = ArrayBuffer[Float]()

    val skip = skipValue+1

    var draw_flag = true
    var is_region = true

    if ((depthMap.realX == null) || (depthMap.realY == null)) {
     // depthMap.computeXY()
    }

    var ignoreUV = false
    if ((depthMap.U == null) || (depthMap.V == null)) {
      ignoreUV = true
    }

    var quadCount = 0

    for (i <- 0 until depthMap.getWidth - skip by skip) {
      for (j <- 0 until depthMap.getHeight - skip by skip) {
        val idx = j * depthMap.getWidth + i

        if ( (depthMap.realZ(idx) < DepthMap.FLT_EPSILON) ||
          (depthMap.realZ(idx + skip) < DepthMap.FLT_EPSILON) ||
          (depthMap.realZ(idx + depthMap.getWidth * skip) < DepthMap.FLT_EPSILON) ||
          (depthMap.realZ(idx + depthMap.getWidth * skip + skip) < DepthMap.FLT_EPSILON)) {
          draw_flag = false
        } else {
          draw_flag = true
        }

        if (draw_flag && (depthMap.mask != null)) {
          if (  !depthMap.mask(idx) ||
            !depthMap.mask(idx + skip) ||
            !depthMap.mask(idx + depthMap.getWidth * skip) ||
            !depthMap.mask(idx + depthMap.getWidth * skip + skip)) {
            is_region = false
          } else {
            is_region = true
          }
        }

        if (draw_flag) {

          if (depthMap.mask != null) {
            draw_flag = is_region
          }

          if ( (Math.abs(depthMap.realZ(idx) - depthMap.realZ(idx + skip)) > depthMap.getMaximumAllowedDeltaZ) ||
            (Math.abs(depthMap.realZ(idx + depthMap.getWidth * skip) - depthMap.realZ(idx + depthMap.getWidth * skip + skip)) > depthMap.getMaximumAllowedDeltaZ) ||
            (Math.abs(depthMap.realZ(idx) - depthMap.realZ(idx + depthMap.getWidth * skip)) > depthMap.getMaximumAllowedDeltaZ) ||
            (Math.abs(depthMap.realZ(idx + skip) - depthMap.realZ(idx + depthMap.getWidth * skip + skip)) > depthMap.getMaximumAllowedDeltaZ)) {
            draw_flag = false
          }
        }

        if (draw_flag) {
          /**
           *    1---0    0-1-2
           *    | / |
           *    2---3    0-2-3
           */

          val indicesIdx = quadCount* 4
          indices += (indicesIdx, indicesIdx+1, indicesIdx+2, indicesIdx, indicesIdx+2, indicesIdx+3)

          //calculate Vertex 0
          var idx2 = idx + skip
          vertexPos += (depthMap.realX(idx2), depthMap.realY(idx2), -depthMap.realZ(idx2))
          normals += (depthMap.realZ(idx + depthMap.getWidth * skip) - depthMap.realZ(idx) / 1.0f,
            (depthMap.realZ(idx + skip) - depthMap.realZ(idx)) / 1.0f,
            0.005f)
          if(!ignoreUV) texCoords += ( depthMap.U(idx2), depthMap.V(idx2))


          //calculate Vertex 1
          idx2 = idx
          vertexPos += (depthMap.realX(idx2), depthMap.realY(idx2), -depthMap.realZ(idx2))
          normals += (depthMap.realZ(idx + depthMap.getWidth * skip) - depthMap.realZ(idx) / 1.0f,
            (depthMap.realZ(idx + skip) - depthMap.realZ(idx)) / 1.0f,
            0.005f)
          if(!ignoreUV) texCoords += ( depthMap.U(idx2), depthMap.V(idx2))

          //calculate Vertex 2
          idx2 = idx + depthMap.getWidth * skip
          vertexPos += (depthMap.realX(idx2), depthMap.realY(idx2), -depthMap.realZ(idx2))
          normals += (depthMap.realZ(idx + depthMap.getWidth * skip) - depthMap.realZ(idx) / 1.0f,
            (depthMap.realZ(idx + skip) - depthMap.realZ(idx)) / 1.0f,
            0.005f)
          if(!ignoreUV) texCoords += ( depthMap.U(idx2), depthMap.V(idx2))


          //calculate Vertex 3
          idx2 = idx + depthMap.getWidth * skip + skip
          vertexPos += (depthMap.realX(idx2), depthMap.realY(idx2), -depthMap.realZ(idx2))
          normals += (depthMap.realZ(idx + depthMap.getWidth * skip) - depthMap.realZ(idx) / 1.0f,
            (depthMap.realZ(idx + skip) - depthMap.realZ(idx)) / 1.0f,
            0.005f)
          if(!ignoreUV) texCoords += ( depthMap.U(idx2), depthMap.V(idx2))

          quadCount = quadCount+1
        }
      }
    }
    // END
    (indices.toArray, vertexPos.toArray, normals.toArray, texCoords.toArray)
  }
}

/**
 * Works with J4K 1.0
 */
object KinectJ4KSkeleton extends SkeletonBase[Int]  {

  protected val mapping : Map[JointType, Int] = immutable.Map(
    simx.components.io.j4k.local.Skeleton.HEAD            -> Skeleton.HEAD,
    simx.components.io.j4k.local.Skeleton.SHOULDER_CENTER -> Skeleton.NECK,
    simx.components.io.j4k.local.Skeleton.SPINE           -> Skeleton.SPINE_MID,
    simx.components.io.j4k.local.Skeleton.HIP_CENTER      -> Skeleton.SPINE_BASE,
    simx.components.io.j4k.local.Skeleton.LEFT_SHOULDER   -> Skeleton.SHOULDER_LEFT,
    simx.components.io.j4k.local.Skeleton.LEFT_ELBOW      -> Skeleton.ELBOW_LEFT,
    simx.components.io.j4k.local.Skeleton.LEFT_WRIST      -> Skeleton.WRIST_LEFT,
    simx.components.io.j4k.local.Skeleton.LEFT_HAND       -> Skeleton.HAND_LEFT,
    simx.components.io.j4k.local.Skeleton.RIGHT_SHOULDER  -> Skeleton.SHOULDER_RIGHT,
    simx.components.io.j4k.local.Skeleton.RIGHT_ELBOW     -> Skeleton.ELBOW_RIGHT,
    simx.components.io.j4k.local.Skeleton.RIGHT_WRIST     -> Skeleton.WRIST_RIGHT,
    simx.components.io.j4k.local.Skeleton.RIGHT_HAND      -> Skeleton.HAND_RIGHT,
    simx.components.io.j4k.local.Skeleton.HIP_LEFT        -> Skeleton.HIP_LEFT,
    simx.components.io.j4k.local.Skeleton.KNEE_LEFT       -> Skeleton.KNEE_LEFT,
    simx.components.io.j4k.local.Skeleton.ANKLE_LEFT      -> Skeleton.ANKLE_LEFT,
    simx.components.io.j4k.local.Skeleton.FOOT_LEFT       -> Skeleton.FOOT_LEFT,
    simx.components.io.j4k.local.Skeleton.HIP_RIGHT       -> Skeleton.HIP_RIGHT,
    simx.components.io.j4k.local.Skeleton.KNEE_RIGHT      -> Skeleton.KNEE_RIGHT,
    simx.components.io.j4k.local.Skeleton.ANKLE_RIGHT     -> Skeleton.ANKLE_RIGHT,
    simx.components.io.j4k.local.Skeleton.FOOT_RIGHT      -> Skeleton.FOOT_RIGHT,
    simx.components.io.j4k.local.Skeleton.LEFT_FINGERTIP  -> Skeleton.HAND_TIP_LEFT,
    simx.components.io.j4k.local.Skeleton.RIGHT_FINGERTIP  -> Skeleton.HAND_TIP_RIGHT,
    simx.components.io.j4k.local.Skeleton.THUMB_LEFT      -> Skeleton.THUMB_LEFT,
    simx.components.io.j4k.local.Skeleton.THUMB_RIGHT     -> Skeleton.THUMB_RIGHT,
    simx.components.io.j4k.local.Skeleton.SPINE_CENTER     -> Skeleton.SPINE_SHOULDER
  )
}

case class J4KConnectionException(msg : String, cause : Throwable) extends Exception(msg, cause)
