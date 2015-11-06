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

import java.awt.Transparency
import java.awt.color.ColorSpace
import java.awt.image._
import java.nio.ShortBuffer

import edu.ufl.digitalworlds.j4k.J4KSDK
import simplex3d.math.ConstVec2i
import simplex3d.math.floatx.{ConstMat4f, ConstVec3f, Mat4x3f}
import simx.core.component.Component
import simx.core.components.renderer.createparameter.{AnimatedObject, ReadFromElseWhere, ShapeFromFile}
import simx.core.entity.Entity
import simx.core.entity.component.{ComponentAspect, EntityConfigLayer}
import simx.core.entity.description.{EntityAspect, NamedSValSet, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.{EntityDescription, Symbols, types}
import simx.core.svaractor.TimedRingBuffer
import simx.core.svaractor.TimedRingBuffer.At
import simx.core.svaractor.unifiedaccess.?
import simx.core.worldinterface.naming.NameIt

import scala.collection.mutable


/**
 * Masterarbeit
 *
 * Konzeption und prototypische Implementierung eines modularen interaktiven Assistenzsystems
 * zur computergetsützten Visualisierung von Feedback für Training und Rehabilitation
 *
 * Author: Anke Giebler-Schubert
 * Date: 14.04.2014
 */


case class KinectComponentAspect(name : Symbol, args : Any*) extends ComponentAspect[KinectComponent](simx.components.io.j4k.local.Symbols.kinectComponent, name, args){
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set(
    simx.components.io.j4k.local.types.Mode.withAnnotations(simx.components.io.j4k.local.Symbols.near),
    types.Angle.withAnnotations(simx.components.io.j4k.local.Symbols.degrees))

  def getCreateParams: NamedSValSet = NamedSValSet(aspectType,
    types.Resolution.withAnnotations(simx.components.io.j4k.local.Symbols.videoFrame)(ConstVec2i(640,480)),
    types.Resolution.withAnnotations(simx.components.io.j4k.local.Symbols.depthMap)(ConstVec2i(320,240)),
    simx.components.io.j4k.local.types.Mode.withAnnotations(simx.components.io.j4k.local.Symbols.near)(true)
  )
}

/*--------------------------------------------------*/
/*                   MESSAGES                       */
/*--------------------------------------------------*/
case class UpdatePlayerMask(id : Int, i : Array[Int], vp : Array[Float], n: Array[Float], tc : Array[Float])
case class UpdateJoint(playerId: Int, jointType : Symbol, pos : ConstVec3f)
case class UpdateVideoFrame(data : Array[Byte], width: Int, height: Int, timestamp: Long)
case class UpdateDepthMap(width: Int, height: Int, data : Array[Short])

/**
 * Component for registering a kinect sensor
 * @param componentName the name of the component
 * @see http://research.dwi.ufl.edu/ufdw/j4k/index.php
 */
class KinectComponent(override val componentName: Symbol)
  extends Component(componentName, simx.components.io.j4k.local.Symbols.kinectComponent)
  with EntityConfigLayer {

  private var kinectConnectionEstablished = false

  private var kinectConnection : KinectConnector = null

  private var playerIdMaskEntityMap = mutable.Map[Int, Entity]()
  private var playerIdSkeletonWithJointEntitiesMap = mutable.Map[Int, (Entity, mutable.Map[Symbol, Entity])]()
  private var depthMapEntity : Option[Entity] = None
  private var videoFrameEntity : Option[Entity] = None


  /*--------------------------------------------------*/
  /*                COMPONENT CONFIG                  */
  /*--------------------------------------------------*/

  /**
   * provide initial values for this component, similar to requestInitialValues for entities
   * @param toProvide the values to be provided
   * @param aspect the component aspect
   * @param e the entity which will represent this component later on
   * @return a SValSet containing the initial configuration for this component
   */
  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) = {

    SValSet(
      types.Angle.withAnnotations(simx.components.io.j4k.local.Symbols.degrees)(0f),
      simx.components.io.j4k.local.types.Mode.withAnnotations(simx.components.io.j4k.local.Symbols.near)(
        aspect.getCreateParams.firstValueFor(simx.components.io.j4k.local.types.Mode.withAnnotations(simx.components.io.j4k.local.Symbols.near)))
    )

  }

  /**
   * (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params: SValSet) = {}



  /*--------------------------------------------------*/
  /*                ENTITY CONFIG                     */
  /*--------------------------------------------------*/

  /**
   * provideInitialValues has to be called within this method with the full set of initial values to be provided
   * @note the component should create its local representation within this mehtod
   * @param toProvide the convertibletraits for which values shall be provided
   * @param aspect the aspect providing the context for this method call
   * @param e the entity to be filled
   * @param given a set of create parameters that were already provided
   *
   */
  protected def requestInitialValues(toProvide: Set[ConvertibleTrait[_]],
                                     aspect: EntityAspect, e: Entity, given: SValSet) = {

    if( aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.mask) ||
      aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.skeleton) ||
      aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.joint) ||
      aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.depthMap) ||
      aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.videoFrame) ){
      provideInitialValues(e, provideValues(aspect, toProvide, given))
    }
    else error("[KinectComponent] Unsupported aspect: " + aspect.aspectType)
  }

  /**
   * used to integrate the entity into the simulation
   * @param e the entity to be integrated
   * @param aspect the aspect which the component has to process
   */
  protected def entityConfigComplete(e: Entity, aspect: EntityAspect) = {
    // in case if mask-Entity
    if(aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.mask)) {
      e.get(simx.components.io.j4k.local.types.PlayerId).head{
        id : Int =>
          kinectConnection.activateMask(id)
          playerIdMaskEntityMap.get(id).collect{
            case oldEntity => oldEntity.remove()
          }
          playerIdMaskEntityMap += (id->e)
      }
    }

    // in case if skeleton-Entity
    else if(aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.skeleton)) {
      var jointTypeEntityMap = mutable.Map[Symbol, Entity]()

      //TODO HACKIDIHACK
      var counter = e.getSVars(types.HasPart).size
      e.get(types.HasPart -> ?).foreach{
        jointEntity : Entity =>
          // counter += 1
          jointEntity.get(types.Identifier).head{jointType : Symbol =>
            jointTypeEntityMap += (jointType -> jointEntity)
            counter -= 1
            if(counter == 0) {
              e.get(simx.components.io.j4k.local.types.PlayerId).head((id : Int) => {
                kinectConnection.activateSkeleton(id)
                val tupel = (e, jointTypeEntityMap)
                playerIdSkeletonWithJointEntitiesMap += (id -> tupel)

                //set default skeleton for player 0 if no kinect is connected
                if(id==0 && !kinectConnectionEstablished) {
                  kinectConnection.updateDefaultSkeleton(id)
                }
              })
            }
          }
      }
    }

    // in case if joint-Entity
    else if(aspect.semanticsEqual(Symbols.joint)) {  }

    // in case if depthMap-Entity
    else if(aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.depthMap)) {
      depthMapEntity = Some(e)
    }

    // in case if videoFrame-Entity
    else if(aspect.semanticsEqual(simx.components.io.j4k.local.Symbols.videoFrame)) {
      videoFrameEntity = Some(e)
    }
  }

  /**
   * called when the construction ot the entity representing this component is completed
   * @param e the entity representing this component
   */
  protected def finalizeConfiguration(e: Entity) = {}

  /**
   * method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  protected def removeFromLocalRep(e: Entity) = {
    var found = false

    //case maskEntity
    playerIdMaskEntityMap.find( entry => e==entry._2).collect{
      case entry =>
        kinectConnection.deactivateMask(entry._1)
        playerIdMaskEntityMap -= entry._1
        found=true
    }

    //case skeletonEntity
    if(!found) {
      playerIdSkeletonWithJointEntitiesMap.find(entry => e == entry._2._1).collect{
        case entry =>
          kinectConnection.deactivateSkeleton(entry._1)
          playerIdSkeletonWithJointEntitiesMap -= entry._1
      }
    }
  }



  private def provideValues(aspect: EntityAspect, toProvide: Set[ConvertibleTrait[_]], given: SValSet) =
    toProvide.foldLeft(SValSet()) {
      (a, b) => b match {
        case x => a.addIfNew(getTypedValue(x,aspect))
          a
      }
    }

  private def getTypedValue[T](c : ConvertibleTrait[T], aspect: EntityAspect) = {
    c(aspect.getCreateParams.firstValueFor(c))
  }


  /*--------------------------------------------------*/
  /*                      RUN                         */
  /*--------------------------------------------------*/

  /**
   * called when the actor is started
   */
  override protected def startUp() = {
    super.startUp()

    kinectConnection = new KinectConnector(this.self)
    /**
     * J4KSDK.NUI_IMAGE_RESOLUTION_320x240: depth for skeleton tracking
     */
    if(!kinectConnection.start(J4KSDK.COLOR|J4KSDK.DEPTH|J4KSDK.SKELETON)) {
      error("[KinectComponent] No Kinect connection established!")
    }
    else {
      println("[KinectComponent] Kinect connection successfully established!")
      kinectConnectionEstablished = true
    //kinectConnection.showViewerDialog()
    }
    //

//    val kinectConnection2 = new KinectConnector(this.self)
//    if(kinectConnection2.start(true,J4KSDK.NUI_IMAGE_RESOLUTION_320x240, J4KSDK.NUI_IMAGE_RESOLUTION_640x480) == 0) {
//      error("[KinectComponent] No Kinect connection established!")
//    }
 //   kinectConnection.stopSkeletonTracking()
  }

  /**
   * Called for each simulation step the component should execute. The freqency with which this method is called
   * depends on the used [[simx.core.component.ExecutionStrategy]].
   */
  protected def performSimulationStep() = {simulationCompleted()}



  /*--------------------------------------------------*/
  /*                    HANDLER                       */
  /*--------------------------------------------------*/

  addHandler[UpdatePlayerMask]{msg => updatePlayerMask(msg.id, msg.i, msg.vp, msg.n, msg.tc)}
  private def updatePlayerMask(playerId : Int, i : Array[Int], vp : Array[Float], n: Array[Float], tc : Array[Float]) {
    playerIdMaskEntityMap.get(playerId).collect{
      case e =>
      //        val normals = if(n.isEmpty)null else n
      //        val texCoords = if(tc.isEmpty)null else tc
      //       e.get(types.Mesh).head.set(new MeshGeometry(i, vp, normals, texCoords, null))

      //        e.set(types.Mesh(new MeshGeometry(i, vp, null, null, null)   )) TODO: must be realized (Thesis Giebler-Schubert)
    }
  }
  
  addHandler[UpdateJoint]{msg => updateJoint(msg.playerId, msg.jointType, msg.pos)}
  private def updateJoint(playerId: Int, jointType : Symbol, pos : ConstVec3f) {
  //  println("updated joint" + playerId + jointType + pos)
    playerIdSkeletonWithJointEntitiesMap.get(playerId).collect{ case (skeletonEntity, map) =>
      map.get(jointType).collect{ case jointEntity : Entity =>
          skeletonEntity.get(types.Transformation).head((toWorldXForm : ConstMat4f) => {
            jointEntity.set(types.Transformation (toWorldXForm * ConstMat4f(Mat4x3f.translate(pos))))
          })
      }
    }
  }

  addHandler[UpdateVideoFrame]{msg => updateVideoFrame(msg.data , msg.width, msg.height, msg.timestamp)}

  /**
   * @param factor Has to be a power of 2 and width as well as height of the image have to be dividable by factor.
   */
  private def getDownScaledCopy(imageData : Array[Byte], factor: Int, width: Int, height: Int): Array[Byte] = {
    val res = new Array[Byte](imageData.length/(factor*factor))

    val resultWidth = width/factor

    for(r <- 0 until height by factor; c <- 0 until width by factor){
      val originCursor = c + r * width
      val resultCursor = (c/factor) + (r/factor) * resultWidth
      Array.copy(imageData, originCursor*4, res, resultCursor*4, 4)

    }
//
//    for(row <- 0 until height){
//        for(col <- 0 until width){
//          val cursor = row*factor*4 + col*factor*4
//          Array.copy(imageData, cursor, res, col*4+row*4, 4)
//      }
//    }
//    for(i <- 0 until ((imageData.length/4)/factor)) {
//      val cursor = i * 4 * factor
//      Array.copy(imageData, cursor, res, i*4, 4)
////      res(i + 0) = imageData(cursor + 0)
////      res(i + 1) = imageData(cursor + 1)
////      res(i + 2) = imageData(cursor + 2)
////      res(i + 3) = imageData(cursor + 3)
//    }
    res
  }


  /**
   *
   * @param _data image data: R+G+B+A
   * @param _width
   * @param _height
   */
  private def updateVideoFrame(_data : Array[Byte], _width : Int, _height: Int, timestamp: Long) = {
    val factor = 4
    val data = getDownScaledCopy(_data, factor, _width, _height)
    val width = _width / factor
    val height = _height / factor


    //clear alpha channel
    for(i <- 3 until data.length by 4)
      data.update(i,255.toByte)

    //TODO import partly from Anke's version
    //if any mask entity is defined
//    playerIdMaskEntityMap.values.foreach { meshEntity => {
//    }

    //if video frame entity is defined
    videoFrameEntity.foreach{ videoE =>
      //update BufferedImage
      val dataBuffer = new DataBufferByte(data, data.length)
      val bOffs: Array[Int] = Array(2, 1, 0, 3) // map RGBA to BGRA
      val rasterBGRA: WritableRaster =
        Raster.createInterleavedRaster(dataBuffer, width, height, width * 4, 4, bOffs, null)
      videoE.set(types.Image(createVideoFrameImage(rasterBGRA)), At(timestamp), TimedRingBuffer.UnbufferedTimeStoring)((_: videoE.SelfType)=>{}) //getImageCopy removed due to the usage of getDownScaledCopy
    }
  }

  private def createVideoFrameImage(rasterBGRA : WritableRaster) : BufferedImage ={
    val cs: ColorSpace = ColorSpace.getInstance(ColorSpace.CS_sRGB)
    val nBits: Array[Int] = Array(8, 8, 8, 8)
    val colorModelBGRA = new ComponentColorModel(cs, nBits, true, false, Transparency.TRANSLUCENT, DataBuffer.TYPE_BYTE)
    new BufferedImage(colorModelBGRA, rasterBGRA, true, null)
  }

  private def getImageCopy(bi : BufferedImage) : BufferedImage ={
    val cm = bi.getColorModel
    val isAlphaPreMultiplied = cm.isAlphaPremultiplied
    val raster = bi.copyData(null)
    new BufferedImage(cm, raster, isAlphaPreMultiplied, null)
  }

  addHandler[UpdateDepthMap]{msg => updateDepthMap(msg.width, msg.height, msg.data)}
  private def updateDepthMap(width: Int, height: Int, data : Array[Short]) {

    //if depth map entity is defined
    depthMapEntity.collect{
      case depthE =>
        //version 2
        val bImg = new BufferedImage(width, height, BufferedImage.TYPE_USHORT_GRAY)
        bImg.getRaster.setDataElements(0,0,width,height, ShortBuffer.wrap(data))
        depthE.set(types.Image(bImg))        // TODO: test me!!
    }
  }
}

case class PlayerMaskEntityDescription(playerId : Int, initialTransform : ConstMat4f) extends EntityDescription(
  AnimatedObject(transformation = initialTransform),
  //  RawMeshAspect(), TODO: have to be implemented in renderer (see Master thesis Giebler-Schubert)!!
  KinectPlayerMaskEntityAspect(playerId),
  NameIt("KinectPlayerMask-" + playerId)
)


object SkeletonEntityDescription{
  def getDescriptions: List[EntityDescription] ={
    (for(j <- KinectJ4KSkeleton.getAllJoints) yield JointEntityDescription(j.id)).toList
  }
}

case class SkeletonEntityDescription( playerId : Int, kinectToWorldXForm : ConstMat4f ) extends EntityDescription(
  List(
    KinectSkeletonEntityAspect(playerId, kinectToWorldXForm),
    NameIt("KinectSkeleton-" + playerId)) :::
    SkeletonEntityDescription.getDescriptions,
  'Skeleton
)


private case class JointEntityDescription(jointType : Symbol) extends EntityDescription(
  List(KinectJointEntityAspect(jointType),
    ShapeFromFile(
      file           = "assets/vis/Sphere.dae",
      transformation = Left(ReadFromElseWhere),
      scale = ConstMat4f(Mat4x3f.scale(0.05f))
    ),
    NameIt("Joint-" + jointType)),
  Symbol("Joint_" + jointType.name)
)

private case class KinectDepthMapEntityDescription() extends EntityDescription(
  KinectRawDepthMapEntityAspect(),
  NameIt("KinectDepthMap")
)

case class KinectVideoFrameEntityDescription() extends EntityDescription(
  List(KinectVideoFrameEntityAspect(),
    NameIt("KinectVideoFrame")),
  'KinectVideoFrame
)

