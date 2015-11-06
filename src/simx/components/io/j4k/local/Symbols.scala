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

import simx.core.ontology.types.OntologySymbol
import simx.core.ontology.SValDescription

object Symbols {

  object kinectComponent extends OntologySymbol(Symbol("KinectComponent"))

  object mask extends OntologySymbol(Symbol("Mask"))

  object skeleton extends OntologySymbol(Symbol("Skeleton"))

  object joint extends OntologySymbol(Symbol("Joint"))

  object playerId extends OntologySymbol(Symbol("PlayerId"))

  object depthMap extends OntologySymbol(Symbol("DepthMap"))

  object videoFrame extends OntologySymbol(Symbol("VideoFrame"))

  object mode extends OntologySymbol(Symbol("Mode"))

  object near extends OntologySymbol(Symbol("Near"))

  object degrees extends  OntologySymbol(Symbol("Degrees"))
}


package object types{
  def init(){}
  object PlayerId extends SValDescription(simx.core.ontology.types.Integer as Symbols.playerId )
  object Mode extends SValDescription(simx.core.ontology.types.Boolean as Symbols.mode definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Mode")
}