-- |
-- Module      : Graphics.Formats.Assimp.PostProcess
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiConfig.h

module Graphics.Formats.Assimp.Config (
    Config(..)
  , configName
  ) where

-- | Global configuration
data Config =
  -- | Enables time measurements.
  --
  -- If enabled, measures the time needed for each part of the loading process
  -- (i.e. IO time, importing, postprocessing, ..) and dumps these timings to
  -- the DefaultLogger. See <http://assimp.sourceforge.net/lib_html/perf.html>
  -- for more information on this topic.
  --
  -- Default: False
    GlobMeasureTime Bool
  -- | Specifies the maximum angle that may be between two vertex tangents that
  -- their tangents and bitangents are smoothed.
  --
  -- This applies to the CalcTangentSpace-Step. The angle is specified in
  -- degrees, so 180 is PI. The default value is 45 degrees. The maximum value
  -- is 175.
  | PpCtMaxSmoothingAngle Float
  -- | Specifies the maximum angle that may be between two face normals at the
  -- same vertex position that their are smoothed together.
  --
  -- Sometimes referred to as 'crease angle'.
  --
  -- This applies to the GenSmoothNormals-Step. The angle is specified in
  -- degrees, so 180 is PI. The default value is 175 degrees (all vertex
  -- normals are smoothed). The maximum value is 175, too. Property type:
  -- float.  Warning: setting this option may cause a severe loss of
  -- performance. The performance is unaffected if the 'FavorSpeed' flag is set
  -- but the output quality may be reduced.
  | PpGsnMaxSmoothingAngle Float
  -- | Sets the colormap (= palette) to be used to decode embedded textures in
  -- MDL (Quake or 3DGS) files.
  --
  -- This must be a valid path to a file. The file is 768 (256*3) bytes large
  -- and contains RGB triplets for each of the 256 palette entries.  The
  -- default value is colormap.lmp. If the file is not found, a default palette
  -- (from Quake 1) is used. 
  | ImportMdlColormap String
  -- | Configures the 'RemoveRedundantMaterials' step to keep materials
  -- matching a name in a given list.
  --
  -- This is a list of 1 to n strings, ' ' serves as delimiter character.
  -- Identifiers containing whitespaces must be enclosed in *single* quotation
  -- marks. For example: @\"keep-me and_me_too anotherMaterialToBeKept \\\'name
  -- with whitespace\\\'\"@.  If a material matches on of these names, it will
  -- not be modified or removed by the postprocessing step nor will other
  -- materials be replaced by a reference to it.
  --
  -- This option might be useful if you are using some magic material names
  -- to pass additional semantics through the content pipeline. This ensures
  -- they won't be optimized away, but a general optimization is still 
  -- performed for materials not contained in the list.
  --
  -- Note: Linefeeds, tabs or carriage returns are treated as whitespace.
  --   Material names are case sensitive.
  --
  -- Default: n/a
  | PpRrmExcludeList String
  -- | Configures the 'PretransformVertices' step to keep the scene hierarchy. 
  --
  -- Meshes are moved to worldspace, but no optimization is performed (read:
  -- meshes with equal materials are not joined. The total number of meshes
  -- won't change).
  --
  -- This option could be of use for you if the scene hierarchy contains
  -- important additional information which you intend to parse. For rendering,
  -- you can still render all meshes in the scene without any
  -- transformations.
  --
  -- Default: False
  | PpPtvKeepHierarchy Bool
  -- | Configures the 'PretransformVertices' step to normalize all vertex
  -- components into the -1...1 range.
  --
  -- That is, a bounding box for the whole scene is computed, the maximum
  -- component is taken and all meshes are scaled appropriately (uniformly of
  -- course!). This might be useful if you don't know the spatial dimension of
  -- the input data.
  --
  -- Default: False
  | PpPtvNormalize Bool
  -- | Configures the 'FindDegenerates' step to remove degenerated primitives
  -- from the import - immediately.
  --
  -- The default behaviour converts degenerated triangles to lines and
  -- degenerated lines to points. See the documentation to the
  -- 'FindDegenerates' step for a detailed example of the various ways to get
  -- rid of these lines and points if you don't want them.
  --
  -- Default: False
  | PpFdRemove Bool
  -- | Configures the 'OptimizeGraph' step to preserve nodes matching a name in
  -- a given list.
  -- 
  --  This is a list of 1 to n strings, ' ' serves as delimiter character.
  --  Identifiers containing whitespaces must be enclosed in *single* quotation
  --  marks. For example: @\"keep-me and_me_too anotherNodeToBeKept \\\'name
  --  with whitespace\\\'\"@.  If a node matches on of these names, it will not
  --  be modified or removed by the postprocessing step.
  --
  --  This option might be useful if you are using some magic node names to
  --  pass additional semantics through the content pipeline. This ensures they
  --  won't be optimized away, but a general optimization is still performed
  --  for nodes not contained in the list.
  --
  --  Note: Linefeeds, tabs or carriage returns are treated as whitespace.
  --  Node names are case sensitive.
  --
  --  Default: n/a
  | PpOgExcludeList String
  -- | Set the maximum number of triangles in a mesh.
  --
  -- This is used by the 'SplitLargeMeshes' PostProcess-Step to determine
  -- whether a mesh must be split or not.
  --
  -- Note: The default value is AI_SLM_DEFAULT_MAX_TRIANGLES
  | PpSlmTriangleLimit Int
  -- | Set the maximum number of vertices in a mesh.
  --
  -- This is used by the 'SplitLargeMeshes' PostProcess-Step to determine
  -- whether a mesh must be split or not.
  --
  -- Note: The default value is AI_SLM_DEFAULT_MAX_VERTICES
  | PpSlmVertexLimit Int
  -- | Set the maximum number of bones affecting a single vertex
  --
  -- This is used by the 'LimitBoneWeights' PostProcess-Step.
  --
  -- Note: The default value is AI_LBW_MAX_WEIGHTS
  | PpLbwMaxWeights Int
  -- | Set the size of the post-transform vertex cache to optimize the vertices
  -- for. This configures the 'ImproveCacheLocality' step.
  --
  -- The size is given in vertices. Of course you can't know how the vertex
  -- format will exactly look like after the import returns, but you can still
  -- guess what your meshes will probably have.
  --
  -- Note: The default value is PP_ICL_PTCACHE_SIZE. That results in slight
  -- performance improvements for most nVidia/AMD cards since 2002.
  | PpIclPtcacheSize Int
  -- | Input parameter to the RemoveComponent step: Specifies the parts of the
  -- data structure to be removed.
  --
  -- See the documentation to this step for further details. The property is
  -- expected to be an integer, a bitwise combination of the 'Component' flags
  -- defined above in this header. The default value is 0. Important: if no
  -- valid mesh is remaining after the step has been executed (e.g you thought
  -- it was funny to specify ALL of the flags defined above) the import FAILS.
  -- Mainly because there is no data to work on anymore ...
  --
  -- Default: 0
  | PpRvcFlags Int
  -- | Input parameter to the SortByPType step: Specifies which primitive types
  -- are removed by the step.
  --
  -- This is a bitwise combination of the 'PrimitiveType' flags. Specifying all
  -- of them is illegal, of course. A typical use would be to exclude all line
  -- and point meshes from the import.
  --
  --  Default: 0
  | PpSbpRemove Int
  -- | Input parameter to the FindInvalidData step: Specifies the
  -- floating-point accuracy for animation values.
  --
  -- The step checks for animation tracks where all frame values are absolutely
  -- equal and removes them. This tweakable controls the epsilon for
  -- floating-point comparisons - two keys are considered equal if the
  -- invariant abs(n0-n1)>epsilon holds true for all vector respectively
  -- quaternion components. The default value is 0.f - comparisons are exact
  -- then.
  --
  -- Default: 0.0
  | PpFidAnimAccuracy Float
  -- | Input parameter to the TransformUVCoords step: Specifies which UV
  -- transformations are evaluated.
  --
  --  This is a bitwise combination of the AI_UVTRAFO_XXX flags. By default all
  --  transformations are enabled (AI_UVTRAFO_ALL).
  | PpTuvEvaluate Int
  -- | A hint to assimp to favor speed against import quality.
  --
  -- Enabling this option may result in faster loading, but it needn't.
  -- It represents just a hint to loaders and post-processing steps to use
  -- faster code paths, if possible. 
  --
  -- Default: False
  | FavorSpeed Bool
  -- | Set the vertex animation keyframe to be imported
  --
  -- ASSIMP does not support vertex keyframes (only bone animation is
  -- supported). The library reads only one frame of models with vertex
  -- animations. By default this is the first frame.
  --
  -- Note: The default value is 0. This option applies to all importers.
  -- However, it is also possible to override the global setting for a specific
  -- loader. You can use the AI_CONFIG_IMPORT_XXX_KEYFRAME options (where XXX
  -- is a placeholder for the file format for which you want to override the
  -- global setting).
  | ImportGlobalKeyframe Int
  | ImportMd3Keyframe Int -- ^ See 'ImportGlobalKeyframe'
  | ImportMd2Keyframe Int -- ^ See 'ImportGlobalKeyframe'
  | ImportMdlKeyframe Int -- ^ See 'ImportGlobalKeyframe'
  | ImportMdcKeyframe Int -- ^ See 'ImportGlobalKeyframe'
  | ImportSmdKeyframe Int -- ^ See 'ImportGlobalKeyframe'
  | ImportUnrealKeyframe Int -- ^ See 'ImportGlobalKeyframe'

  -- | Configures the AC loader to collect all surfaces which have the
  -- "Backface cull" flag set in separate meshes. 
  --
  -- Default: True
  | ImportAcSeparateBfCull Bool
  -- | Configures whether the AC loader evaluates subdivision surfaces
  --
  -- (indicated by the presence of the 'subdiv' attribute in the file). By
  -- default, Assimp performs the subdivision using the standard Catmull-Clark
  -- algorithm
  | ImportAcEvalSubdivision Bool
  -- | Configures the UNREAL 3D loader to separate faces with different surface
  -- flags (e.g. two-sided vs. single-sided).
  --
  -- Default: True
  | ImportUnrealHandleFlags Bool
  -- | Configures the terragen import plugin to compute uv's for terrains, if
  -- not given. Furthermore a default texture is assigned.
  --
  -- UV coordinates for terrains are so simple to compute that you'll usually
  -- want to compute them on your own, if you need them. This option is
  -- intended for model viewers which want to offer an easy way to apply
  -- textures to terrains.
  --
  -- Default: False
  | ImportTerMakeUvs Bool
  -- | Configures the ASE loader to always reconstruct normal vectors basing on
  -- the smoothing groups loaded from the file.
  -- 
  -- Some ASE files have carry invalid normals, other don't.
  --
  -- Default: True
  | ImportAseReconstructNormals Bool
  -- | Configures the M3D loader to detect and process multi-part Quake player
  -- models.
  --
  -- These models usually consist of 3 files, lower.md3, upper.md3 and
  -- head.md3. If this property is set to true, Assimp will try to load and
  -- combine all three files if one of them is loaded.
  --
  -- Default: True
  | ImportMd3HandleMultipart Bool
  -- | Tells the MD3 loader which skin files to load.
  --
  -- When loading MD3 files, Assimp checks whether a file
  -- \<md3_file_name\>_\<skin_name\>.skin is existing. These files are used by
  -- Quake III to be able to assign different skins (e.g. red and blue team) to
  -- models. 'default', 'red', 'blue' are typical skin names.
  --
  -- Default: "default"
  | ImportMd3SkinName String
  -- | Specify the Quake 3 shader file to be used for a particular MD3 file.
  -- This can also be a search path.
  --
  -- By default Assimp's behaviour is as follows: If a MD3 file
  -- @\<any_path\>\/models\/\<any_q3_subdir\>\/\<model_name\>\/\<file_name\>.md3@
  -- is loaded, the library tries to locate the corresponding shader file in
  -- @\<any_path\>\/scripts\/\<model_name\>.shader@. This property overrides
  -- this behaviour. It can either specify a full path to the shader to be
  -- loaded or alternatively the path (relative or absolute) to the directory
  -- where the shaders for all MD3s to be loaded reside. Assimp attempts to
  -- open @\<dir\>\/\<model_name\>.shader@ first,
  -- @\<dir\>\/\<file_name\>.shader@ is the fallback file. Note that \<dir\>
  -- should have a terminal (back)slash.
  | ImportMd3ShaderSrc String
  -- | Configures the LWO loader to load just one layer from the model.
  -- 
  -- LWO files consist of layers and in some cases it could be useful to load
  -- only one of them. This property can be either a string - which specifies
  -- the name of the layer - or an integer - the index of the layer. If the
  -- property is not set the whole LWO model is loaded. Loading fails if the
  -- requested layer is not available. The layer index is zero-based and the
  -- layer name may not be empty.
  --
  -- Default: all layers are loaded
  | ImportLwoOneLayerOnly (Either String Int)
  -- | Configures the MD5 loader to not load the MD5ANIM file for a MD5MESH
  -- file automatically.
  -- 
  -- The default strategy is to look for a file with the same name but the
  -- MD5ANIM extension in the same directory. If it is found, it is loaded and
  -- combined with the MD5MESH file. This configuration option can be used to
  -- disable this behaviour.
  --
  -- Default: False
  | ImportMd5NoAnimAutoload Bool
  -- | Defines the begin of the time range for which the LWS loader evaluates
  -- animations and computes 'NodeAnim's.
  -- 
  -- Assimp provides full conversion of LightWave's envelope system, including
  -- pre and post conditions. The loader computes linearly subsampled animation
  -- chanels with the frame rate given in the LWS file. This property defines
  -- the start time. Note: animation channels are only generated if a node has
  -- at least one envelope with more tan one key assigned. This property.  is
  -- given in frames, '0' is the first frame. By default, if this property is
  -- not set, the importer takes the animation start from the input LWS file
  -- ('FirstFrame' line)
  --
  -- Default: taken from file
  | ImportLwsAnimStart Int
  -- | See 'ImportLwsAnimStart' - end of the imported time range
  | ImportLwsAnimEnd Int
  -- | Defines the output frame rate of the IRR loader.
  -- 
  -- IRR animations are difficult to convert for Assimp and there will always
  -- be a loss of quality. This setting defines how many keys per second are
  -- returned by the converter.
  --
  -- Default: 100
  | ImportIrrAnimFps Int
  -- | Importer will try to load this Materialfile
  --
  -- Ogre Meshes contain only the MaterialName, not the MaterialFile. If there
  -- is no material file with the same name as the material, Ogre Importer will
  -- try to load this file and search the material in it.
  | ImportOgreMaterialFile String

configName :: Config -> String
configName config = case config of
  GlobMeasureTime _             -> "GLOB_MEASURE_TIME"
  PpCtMaxSmoothingAngle _       -> "PP_CT_MAX_SMOOTHING_ANGLE"
  PpGsnMaxSmoothingAngle _      -> "PP_GSN_MAX_SMOOTHING_ANGLE"
  ImportMdlColormap _           -> "PP_MDL_COLORMAP"
  PpRrmExcludeList _            -> "PP_RRM_EXCLUDE_LIST"
  PpPtvKeepHierarchy _          -> "PP_PTV_KEEP_HIERARCHY"
  PpPtvNormalize _              -> "PP_PTV_NORMALIZE"
  PpFdRemove _                  -> "PP_FD_REMOVE"
  PpOgExcludeList _             -> "PP_OG_EXCLUDE_LIST"
  PpSlmTriangleLimit _          -> "PP_SLM_TRIANGLE_LIMIT"
  PpSlmVertexLimit _            -> "PP_SLM_VERTEX_LIMIT"
  PpLbwMaxWeights _             -> "PP_LBW_MAX_WEIGHTS"
  PpIclPtcacheSize _            -> "PP_ICL_PTCACHE_SIZE"
  PpRvcFlags _                  -> "PP_RVC_FLAGS"
  PpSbpRemove _                 -> "PP_SBP_REMOVE"
  PpFidAnimAccuracy _           -> "PP_FID_ANIM_ACCURACY"
  PpTuvEvaluate _               -> "PP_TUV_EVALUATE"
  FavorSpeed _                  -> "FAVOUR_SPEED"
  ImportGlobalKeyframe _        -> "IMPORT_GLOBAL_KEYFRAME"
  ImportMd3Keyframe _           -> "IMPORT_MD3_KEYFRAME"
  ImportMd2Keyframe _           -> "IMPORT_MD2_KEYFRAME"
  ImportMdlKeyframe _           -> "IMPORT_MDL_KEYFRAME"
  ImportMdcKeyframe _           -> "IMPORT_MDC_KEYFRAME"
  ImportSmdKeyframe _           -> "IMPORT_SMD_KEYFRAME"
  ImportUnrealKeyframe _        -> "IMPORT_UNREAL_KEYFRAME"
  ImportAcSeparateBfCull _      -> "IMPORT_AC_SEPARATE_BFCULL"
  ImportAcEvalSubdivision _     -> "IMPORT_AC_EVAL_SUBDIVISION"
  ImportUnrealHandleFlags _     -> "UNREAL_HANDLE_FLAGS"
  ImportTerMakeUvs _            -> "IMPORT_TER_MAKE_UVS"
  ImportAseReconstructNormals _ -> "IMPORT_ASE_RECONSTRUCT_NORMALS"
  ImportMd3HandleMultipart _    -> "IMPORT_MD3_HANDLE_MULTIPART"
  ImportMd3SkinName _           -> "IMPORT_MD3_SKIN_NAME"
  ImportMd3ShaderSrc _          -> "IMPORT_MD3_SHADER_SRC"
  ImportLwoOneLayerOnly _       -> "IMPORT_LWO_ONE_LAYER_ONLY"
  ImportMd5NoAnimAutoload _     -> "IMPORT_MD5_NO_ANIM_AUTOLOAD"
  ImportLwsAnimStart _          -> "IMPORT_LWS_ANIM_START"
  ImportLwsAnimEnd _            -> "IMPORT_LWS_ANIM_END"
  ImportIrrAnimFps _            -> "IMPORT_IRR_ANIM_FPS"
  ImportOgreMaterialFile _      -> "IMPORT_OGRE_MATERIAL_FILE"
