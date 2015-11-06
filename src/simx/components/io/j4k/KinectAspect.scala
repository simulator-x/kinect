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

import java.awt.image.BufferedImage

import simplex3d.math.floatx.{ConstMat4f, Mat4f}
import simx.core.entity.description.EntityAspect
import simx.core.ontology._

/**
 * Masterarbeit
 *
 * Konzeption und prototypische Implementierung eines modularen interaktiven Assistenzsystems
 * zur computergetsützten Visualisierung von Feedback für Training und Rehabilitation
 *
 * Author: Anke Giebler-Schubert
 * Date: 14.04.2014
 */

class KinectAspect {

}

/**
 * [[simx.core.entity.description.EntityAspect]] holding the mesh-data of a single player mask. The mesh data are calculated with the depth map
 * updated by the kinect sensor.
 * @param playerId The id of the player [0-4]
 */
case class KinectPlayerMaskEntityAspect(playerId : Int) extends EntityAspect(simx.components.io.j4k.local.Symbols.kinectComponent, simx.components.io.j4k.local.Symbols.mask, Nil) {
  /**
   * the features the entity will at least have when it is created
   * @return the features the entity will at least have when it is created
   */
  def getFeatures = {
    Set(simx.components.io.j4k.local.types.PlayerId, types.Texture, types.Mesh )
  }

  /**
   * the features the component for which this aspect is designed *must* provide (i.e. provide initial values for those
   * features)
   * @return a set of features
   */
  def getProvidings = Set(simx.components.io.j4k.local.types.PlayerId)

  /**
   *
   * The list of create parameters
   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instanciate an
   *         entity with this aspect
   */
  def getCreateParams = addCVar( simx.components.io.j4k.local.types.PlayerId.asConst(playerId))
}

/**
 *
 * @param playerId
 * @param kinectToWorldXForm
 */
case class KinectSkeletonEntityAspect(playerId : Int, kinectToWorldXForm : ConstMat4f) extends EntityAspect(simx.components.io.j4k.local.Symbols.kinectComponent, simx.components.io.j4k.local.Symbols.skeleton, Nil) {

  def getFeatures = {
    Set(simx.components.io.j4k.local.types.PlayerId, types.Transformation)
  }

  def getProvidings = getFeatures

  def getCreateParams = addCVars(simx.components.io.j4k.local.types.PlayerId.asConst(playerId) and types.Transformation(kinectToWorldXForm))
}

case class KinectJointEntityAspect(jointType : Symbol) extends EntityAspect(simx.components.io.j4k.local.Symbols.kinectComponent, Symbols.joint, Nil) {

  def getFeatures = {
    Set(types.Identifier, types.Transformation)
  }

  def getProvidings = {
    Set(types.Identifier, types.Transformation)
  }

  def getCreateParams = addCVars(types.Identifier.asConst(jointType) and types.Transformation(Mat4f.Identity))
}

case class KinectRawDepthMapEntityAspect() extends EntityAspect(simx.components.io.j4k.local.Symbols.kinectComponent, simx.components.io.j4k.local.Symbols.depthMap, Nil) {

  def getFeatures = Set(types.Image)

  def getProvidings = Set(types.Image)

  def getCreateParams = addCVar(types.Image(new BufferedImage(1,1,BufferedImage.TYPE_INT_RGB)))
}

case class KinectVideoFrameEntityAspect() extends EntityAspect(simx.components.io.j4k.local.Symbols.kinectComponent, simx.components.io.j4k.local.Symbols.videoFrame, Nil) {

  def getFeatures = Set(types.Image)

  def getProvidings = Set(types.Image)

  def getCreateParams = addCVar(types.Image(new BufferedImage(1,1,BufferedImage.TYPE_INT_RGB)))
}


//private case class JointVisual() {
//,
//  NameIt(name)
//  ) {
//  }
//
//}extends Sphere(
//  name = "j",
//  transformation_ = Left(ReadFromElseWhere),//Right(ConstMat4f(Mat4x3f.translate(Vec3f(0,0,-3f)))),
//  scale_ = ConstMat4f(Mat4x3f.scale(0.05f)))